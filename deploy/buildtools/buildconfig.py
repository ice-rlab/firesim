from __future__ import annotations
from enum import Enum, auto
import sys
import logging

from time import strftime, gmtime
import pprint
import yaml
from pathlib import Path

from awstools.awstools import valid_aws_configure_creds, aws_resource_names
from buildtools.bitbuilder import BitBuilder
import buildtools
from util.deepmerge import deep_merge
from util.targetprojectutils import extra_target_project_make_args, resolve_path

# imports needed for python type checking
from typing import Set, Any, Optional, Dict, TYPE_CHECKING

if TYPE_CHECKING:
    from buildtools.buildconfigfile import BuildConfigFile

rootLogger = logging.getLogger()


class InvalidBuildConfigSetting(Exception):
    pass


# All known strategy strings. A given platform may not implement all of them.
class BuildStrategy(Enum):
    BASIC = auto()
    AREA = auto()
    TIMING = auto()
    EXPLORE = auto()
    CONGESTION = auto()
    NORETIMING = auto()
    DEFAULT = auto()

    @staticmethod
    def from_string(input: str) -> BuildStrategy:
        """Constructs an instance of this enum from an input string"""
        try:
            return BuildStrategy[input]
        except KeyError:
            all_names = [name for name, _ in BuildStrategy.__members__.items()]
            raise InvalidBuildConfigSetting(
                f"Invalid buildstrategy name '{input}'. \n Valid options: {all_names}"
            )


class BuildConfig:
    """Represents a single build configuration used to build RTL, drivers, and bitstreams.

    Attributes:
        name: Name of config i.e. name of `config_build_recipe.yaml` section.
        build_config_file: Pointer to global build config file.
        TARGET_PROJECT: Target project to build.
        TARGET_PROJECT_MAKEFRAG: Target project makefrag location to build.
        DESIGN: Design to build.
        TARGET_CONFIG: Target config to build.
        deploy_sextuplet: Deploy sextuplet override.
        launch_time: Launch time of the manager.
        PLATFORM_CONFIG: Platform config to build.
        fpga_frequency: Frequency for the FPGA build.
        strategy: Strategy for the FPGA build.
        post_build_hook: Post build hook script.
        bitbuilder: bitstream configuration class.
    """

    name: str
    build_config_file: BuildConfigFile
    TARGET_PROJECT: str
    TARGET_PROJECT_MAKEFRAG: Optional[str]
    DESIGN: str
    TARGET_CONFIG: str
    deploy_sextuplet: Optional[str]
    frequency: float
    strategy: BuildStrategy
    launch_time: str
    PLATFORM_CONFIG: str
    post_build_hook: str
    bitbuilder: BitBuilder
    circt_flags : str

    def __init__(
        self,
        name: str,
        recipe_config_dict: Dict[str, Any],
        build_config_file: BuildConfigFile,
        launch_time: str,
    ) -> None:
        """
        Args:
            name: Name of config i.e. name of `config_build_recipe.yaml` section.
            recipe_config_dict: `config_build_recipe.yaml` options associated with name.
            build_config_file: Global build config file.
            launch_time: Time manager was launched.
        """
        self.name = name
        self.build_config_file = build_config_file

        # default provided for old build recipes that don't specify TARGET_PROJECT, PLATFORM
        self.PLATFORM = recipe_config_dict.get("PLATFORM", "f1")
        self.TARGET_PROJECT = recipe_config_dict.get("TARGET_PROJECT", "firesim")

        # resolve the path as an absolute path if set
        self.TARGET_PROJECT_MAKEFRAG = recipe_config_dict.get("TARGET_PROJECT_MAKEFRAG")
        if self.TARGET_PROJECT_MAKEFRAG:
            base = build_config_file.build_config_recipes_file_path
            abs_deploy_makefrag = resolve_path(self.TARGET_PROJECT_MAKEFRAG, base)
            if abs_deploy_makefrag is None:
                raise Exception(
                    f"Unable to find TARGET_PROJECT_MAKEFRAG ({self.TARGET_PROJECT_MAKEFRAG}) either as an absolute path or relative to {base}"
                )
            else:
                self.TARGET_PROJECT_MAKEFRAG = abs_deploy_makefrag

        self.DESIGN = recipe_config_dict["DESIGN"]
        self.TARGET_CONFIG = recipe_config_dict["TARGET_CONFIG"]

        if (
            "deploy_triplet" in recipe_config_dict
            and ("deploy_quintuplet" in recipe_config_dict or "deploy_sextuplet" in recipe_config_dict)
        ):
            rootLogger.error(
                "Cannot have both 'deploy_triplet' and ('deploy_quintuplet' or 'deploy_sextuplet') in build config. "
                "Define only 'deploy_sextuplet' (preferred)."
            )
            sys.exit(1)

        if "deploy_quintuplet" in recipe_config_dict and "deploy_sextuplet" in recipe_config_dict:
            rootLogger.error(
                "Cannot have both 'deploy_quintuplet' and 'deploy_sextuplet' in build config. "
                "Define only 'deploy_sextuplet'."
            )
            sys.exit(1)

        if "deploy_quintuplet" in recipe_config_dict:
            rootLogger.warning(
                "Please rename your 'deploy_quintuplet' key in your build config to 'deploy_sextuplet'. "
                "Support for 'deploy_quintuplet' will be removed in the future."
            )

        if "deploy_triplet" in recipe_config_dict:
            rootLogger.warning(
                "Please rename your 'deploy_triplet' key in your build config to 'deploy_sextuplet'. "
                "Support for 'deploy_triplet' will be removed in the future."
            )

        self.deploy_sextuplet = recipe_config_dict.get("deploy_sextuplet")
        if self.deploy_sextuplet is None:
            # backwards compat: quintuplet, then triplet
            self.deploy_sextuplet = recipe_config_dict.get("deploy_quintuplet")
        if self.deploy_sextuplet is None:
            self.deploy_sextuplet = recipe_config_dict.get("deploy_triplet")


        if self.deploy_sextuplet is not None and len(self.deploy_sextuplet.split("-")) == 3:
            self.deploy_sextuplet = f"{self.PLATFORM}-{self.TARGET_PROJECT}-" + self.deploy_sextuplet


        if self.deploy_sextuplet is not None and len(self.deploy_sextuplet.split("-")) == 5:
            self.deploy_sextuplet = self.deploy_sextuplet + f"-{self.name}"

        self.launch_time = launch_time

        # run platform specific options
        self.PLATFORM_CONFIG = recipe_config_dict["PLATFORM_CONFIG"]
        self.post_build_hook = recipe_config_dict["post_build_hook"]

        # retrieve frequency and strategy selections
        bitstream_build_args = recipe_config_dict["platform_config_args"]
        self.fpga_frequency = bitstream_build_args["fpga_frequency"]
        self.build_strategy = BuildStrategy.from_string(
            bitstream_build_args["build_strategy"]
        )
        
        # optional circt compiler flags
        self.circt_flags = recipe_config_dict.get("circt_flags", "")
        self.circt_flags = " ".join(self.get_circt_flags().split())  # sanitize whitespace in circt flags

        rootLogger.info(f"CIRCT flags: {self.circt_flags}")

        # retrieve the bitbuilder section
        bitbuilder_conf_dict = None
        with open(recipe_config_dict["bit_builder_recipe"], "r") as yaml_file:
            bitbuilder_conf_dict = yaml.safe_load(yaml_file)

        bitbuilder_type_name = bitbuilder_conf_dict["bit_builder_type"]
        bitbuilder_args = bitbuilder_conf_dict["args"]

        # add the overrides if it exists
        override_args = recipe_config_dict.get("bit_builder_arg_overrides")
        if override_args:
            bitbuilder_args = deep_merge(bitbuilder_args, override_args)

        bitbuilder_dispatch_dict = dict(
            [(x.__name__, x) for x in buildtools.buildconfigfile.inheritors(BitBuilder)]
        )

        if not bitbuilder_type_name in bitbuilder_dispatch_dict:
            raise Exception(
                f"Unable to find {bitbuilder_type_name} in available bitbuilder classes: {bitbuilder_dispatch_dict.keys()}"
            )

        # validate the frequency
        if (self.fpga_frequency is None) or not (0 < self.fpga_frequency <= 300.0):
            raise Exception(
                f"{self.fpga_frequency} is not a valid build frequency. Valid frequencies are between 0.0-300.0 (MHz)"
            )

        # create dispatcher object using class given and pass args to it
        self.bitbuilder = bitbuilder_dispatch_dict[bitbuilder_type_name](
            self, bitbuilder_args
        )

    def get_chisel_triplet(self) -> str:
        """Get the unique build-specific '-' deliminated triplet.

        Returns:
            Chisel triplet
        """
        return f"{self.DESIGN}-{self.TARGET_CONFIG}-{self.PLATFORM_CONFIG}"

    def get_effective_deploy_triplet(self) -> str:
        """Get the effective deploy triplet, i.e. the triplet version of
        get_effective_deploy_sextuplet().

        Returns:
            Effective deploy triplet
        """
        return "-".join(self.get_effective_deploy_sextuplet().split("-")[2:])

    def get_chisel_sextuplet(self) -> str:
        """Get the unique build-specific '-' deliminated sextuplet.

        Returns:
            Chisel sextuplet
        """
        return f"{self.PLATFORM}-{self.TARGET_PROJECT}-{self.DESIGN}-{self.TARGET_CONFIG}-{self.PLATFORM_CONFIG}"

    def get_effective_deploy_sextuplet(self) -> str:
        """Get the effective deploy sextuplet, i.e. the value specified in
        deploy_sextuplet if specified, otherwise just get_chisel_sextuplet().

        Returns:
            Effective deploy sextuplet
        """
        if self.deploy_sextuplet:
            return self.deploy_sextuplet
        return self.get_chisel_sextuplet()
    
    def get_circt_flags(self) -> str:
        """Get the circt compiler flags specified in the build config.

        Returns:
            CIRCT compiler flags string.
        """
        return self.circt_flags

    def get_deploy_makefrag(self) -> Optional[str]:
        return self.TARGET_PROJECT_MAKEFRAG

    def get_frequency(self) -> float:
        """Get the desired fpga frequency.

        Returns:
            Specified FPGA frequency (float)
        """
        return self.fpga_frequency

    def get_strategy(self) -> BuildStrategy:
        """Get the strategy string.

        Returns:
            Specified build strategy
        """
        return self.build_strategy

    def get_build_dir_name(self) -> str:
        """Get the name of the local build directory.

        Returns:
            Name of local build directory (based on time/name).
        """
        return f"{self.launch_time}-{self.name}"

    def make_recipe(self, recipe: str, deploy_dir: str) -> str:
        """Create make command for a given recipe using the tuple variables.

        Args:
            recipe: Make variables/target to run.

        Returns:
            Fully specified make command.
        """
        rootLogger.warning(f"""MAKING RECIPE: make PLATFORM={self.PLATFORM} TARGET_PROJECT={self.TARGET_PROJECT} {extra_target_project_make_args(self.TARGET_PROJECT, self.TARGET_PROJECT_MAKEFRAG, deploy_dir)} DESIGN={self.DESIGN} TARGET_CONFIG={self.TARGET_CONFIG} PLATFORM_CONFIG={self.PLATFORM_CONFIG} {recipe}""")
        return f"""make PLATFORM={self.PLATFORM} TARGET_PROJECT={self.TARGET_PROJECT} {extra_target_project_make_args(self.TARGET_PROJECT, self.TARGET_PROJECT_MAKEFRAG, deploy_dir)} DESIGN={self.DESIGN} TARGET_CONFIG={self.TARGET_CONFIG} PLATFORM_CONFIG={self.PLATFORM_CONFIG} CIRCT_FLAGS={self.get_circt_flags()} {recipe}"""

    def __repr__(self) -> str:
        return f"< {type(self)}(name={self.name!r}, build_config_file={self.build_config_file!r}) @{id(self)} >"

    def __str__(self) -> str:
        return pprint.pformat(vars(self), width=1, indent=10)
