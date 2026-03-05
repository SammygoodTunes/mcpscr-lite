"""
Construct
"""

import traceback
from enum import Enum
from glob import glob
from json import loads
from os import path, mkdir
from pathlib import Path

from mcpscr.api.parser.converter import Converter
from mcpscr.api.parser.parser import JavaParam, Parser
from mcpscr.api.resources.constants import APIConstants
from mcpscr.generator import Generator

from mcpscr.logger import logger

class InstanceType(Enum):
    """ Type of instance """
    CLIENT = 0
    SERVER = 1

class APIConstructor:
    """
    API constructor
    """
    def __init__(self, mcp_dir: str) -> None:
        self.mcp_dir = mcp_dir
        self.parser = Parser()
        self.converter = Converter()

        self.constants = APIConstants(self.parser)

    def construct(self, instance_type: InstanceType) -> None:
        """
        Construct all the API classes
        """
        directory = 'minecraft'
        if instance_type == InstanceType.SERVER:
            directory = 'minecraft_server'

        src_path = f'{self.mcp_dir}/src/{directory}/net/minecraft/src'
        files = glob(path.join(self.mcp_dir, f'{src_path}/*.java'))
        for i, file in enumerate(files):
            self.parser.read(file)

        api_path = f'{self.mcp_dir}/src/{directory}/net/minecraft/mcpscr'
        if not path.exists(api_path):
            mkdir(api_path)

        resources = loads(open(f'mcpscr/api/api_resources.json').read())
        resources = resources['server'] if instance_type == InstanceType.SERVER else resources['client']

        try:
            self.constants.construct(self.converter, resources['vars'])
            self.constants.write_to_disk(self.converter, Path(f'{self.mcp_dir}/src/{directory}/net/minecraft/mcpscr'))
        except KeyError as e:
            logger.error(f'Failed to construct {self.constants.__class__.__name__}: Key \'{e}\' not found!')
        except IndexError as e:
            logger.error(f'Failed to construct {self.constants.__class__.__name__}: Invalid index {e}')
        except Exception as e:
            logger.error(f'Failed to construct {self.constants.__class__.__name__}: {e}')
            traceback.print_exc()

        for c in self.parser.project.find_package_by_name('net.minecraft.src').classes:
            self.converter.write_from(c, Path(src_path) / Path(c.file_name))

    def randomise_from_preset(self,
                              instance_type: InstanceType,
                              preset_data: dict,
                              probability: float,
                              intensity: float) -> None:
        """
        Read from preset data
        :param instance_type:
        :param preset_data:
        :param probability:
        :param intensity:
        """
        try:
            Generator.reset()

            directory = 'minecraft'
            if instance_type == InstanceType.SERVER:
                directory = 'minecraft_server'

            constants = preset_data['content']['vars']
            for constant in constants:
                constant_id: str = constant['id']
                constant_min, constant_max = constant['range']
                self.constants.set_value(constant_id, Generator.generate_int(
                    self.constants.get_value(constant_id),
                    probability,
                    intensity,
                    constant_min,
                    constant_max
                ))
            self.constants.write_to_disk(self.converter, Path(f'{self.mcp_dir}/src/{directory}/net/minecraft/mcpscr'))
        except KeyError as e:
            logger.error(f'Failed to randomise from preset: Key \'{e}\' not found!')
        except Exception as e:
            logger.error(f'Failed to randomise from preset: {e}')
            traceback.print_exc()
