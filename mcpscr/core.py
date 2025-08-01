"""
MCPSCR Core
"""

from mcpscr import utils, javaparser
from mcpscr.logger import logger
from glob import glob
from random import randint, uniform
from os import path

class MCPSCR:
    """
    MCPSCR Class
    """

    def __init__(self, mcp_dir):
        self.mcp_dir = mcp_dir
        if not utils.has_supported_system():
            raise Exception(f'System [{utils.OS_SYS}] not supported.')
        if not utils.has_mcp(self.mcp_dir):
            raise Exception('MCP not found! Please make sure the \'mcp\' folder exists and contains the MCP.')

    def prepare(self) -> None:
        """
        Start MCPSCR
        :return:
        """
        logger.info(f"MCP Directory: {self.mcp_dir}")
        if not utils.mcp_setup(self.mcp_dir):
            raise Exception("Failed to run setup.")
        if not utils.has_mcp_sources(self.mcp_dir):
            logger.info('Sources not found, running decompiler')
            if not utils.mcp_decompile(self.mcp_dir):
                raise Exception("Failed to decompile sources")
        logger.info('Welcome to MCPSCR-Lite!')

    def randomise(self) -> None:
        """
        Randomise some files
        :return:
        """
        logger.info("Randomising")
        files = glob(path.join(self.mcp_dir, "**/NoiseGenerator*.java"), recursive=True)
        changes = 0
        for file in files:
            with open(file) as f:
                data_lines = f.readlines()
            for i, line in enumerate(data_lines):
                doubles = javaparser.find_doubles(line)
                if not doubles:
                    continue
                l = ""
                start = 0
                for j, double in enumerate(doubles):
                    col = double[1].column - 1
                    length = len(double[0])
                    if j < len(doubles) - 1:
                        end = doubles[j + 1][1].column - 1
                    else:
                        end = len(line)
                    if randint(1, 100) <= 90:
                        value = double[0]
                    else:
                        value = f'{float(double[0][:-1]) + uniform(0, 100):.2f}D'
                        changes += 1
                    l += line[start:col] + value + line[col + length:end]
                    start = end
                data_lines[i] = l

            with open(file, 'w') as f:
                f.writelines(data_lines)

        logger.info(f'{changes} change(s) made!')

    def update(self) -> None:
        """
        Recompile the sources
        :return:
        """
        if not utils.mcp_recompile(self.mcp_dir):
            raise Exception("Could not recompile")

    def cleanup(self) -> None:
        """
        Cleanup and decompile to fresh source code
        :return:
        """
        if not utils.mcp_cleanup(self.mcp_dir):
            raise Exception("Could not clean up")
        if not utils.mcp_decompile(self.mcp_dir):
            raise Exception("Could not decompile")

    def run_game(self):
        """
        Run the client
        :return:
        """
        if not utils.mcp_start_client(self.mcp_dir):
            raise Exception("Could not run the client")

    def run_server(self):
        """
        Run the server
        :return:
        """
        if not utils.mcp_start_server(self.mcp_dir):
            raise Exception("Could not run the server")


