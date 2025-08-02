"""
MCPSCR Core
"""

from mcpscr import utils, javaparser, randomiser
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
        Prepare MCPSCR
        :return:
        """
        logger.info(f"MCP Directory: {self.mcp_dir}")
        if not utils.mcp_setup(self.mcp_dir):
            raise Exception("Failed to run setup.")
        if not utils.has_mcp_sources(self.mcp_dir):
            logger.info('Sources not found, running decompiler')
            if not utils.mcp_decompile(self.mcp_dir):
                raise Exception("Failed to decompile sources")
            if not utils.mcp_recompile(self.mcp_dir):
                raise Exception("Failed to decompile sources")
        logger.info('Welcome to MCPSCR-Lite!')

    def main_menu(self) -> None:
        """
        Start MCPSCR main menu
        :return:
        """
        running = True
        while running:
            option = input(
                "[R] Randomise\n[S] Start game\n[T] Start server"
                "\n[U] Update\n[C] Clean up \n[E] Exit\n>> "
            ).lower()
            if option == 'e':
                running = False
            elif option == 'r':
                self.randomiser_menu()
            elif option == 'c':
                self.cleanup()
                self.update()
            elif option == 's':
                self.run_game()
            elif option == 't':
                self.run_server()
            elif option == 'u':
                self.update()

    def randomiser_menu(self) -> None:
        """
        Show MCPSCR randomiser menu
        :return:
        """
        source_type = input("Source type: [C] Client-side / [S] Server-side: ").lower()
        if source_type == 'c':
            source_type = 'minecraft'
        elif source_type == 's':
            source_type = 'minecraft_server'
        else:
            logger.error("Invalid source type!")
            return
        randomiser_option = input("Randomise: [N] Noise Gen / [M] Models / [A] All: ").lower()
        try:
            prob = int(input("Probability: "))
            if prob < 0 or prob > 100:
                raise ValueError
        except ValueError:
            logger.error("Invalid probability, must be an int between 0 and 100!")
            return
        if randomiser_option == 'a':
            logger.info("Randomising from all files")
            self.randomise(f'sources/{source_type}/**/*.java', prob)
            return
        elif randomiser_option == 'n':
            logger.info("Randomising from Noise files")
            self.randomise(f'sources/{source_type}/**/Noise*.java', prob)
            return
        elif randomiser_option == 'n':
            logger.info("Randomising from Models files")
            self.randomise(f'sources/{source_type}/**/Model*.java', prob)
            return
        logger.error("Invalid option!")


    def randomise(self, token: str, prob: int) -> None:
        """
        Randomise some files
        :param token:
        :param prob:
        :return:
        """
        logger.info("Randomising")
        files = glob(path.join(self.mcp_dir, token), recursive=True)
        changes = 0
        for file in files:
            with open(file) as f:
                data_lines = f.readlines()
            for i, line in enumerate(data_lines):
                l = line
                l, c = randomiser.randomise_doubles(l, javaparser.find_doubles(line), prob)
                changes += c
                l, c = randomiser.randomise_floats(l, javaparser.find_floats(l), prob)
                changes += c
                data_lines[i] = l
            with open(file, 'w') as f:
                f.writelines(data_lines)
        logger.info(f'{changes} change(s) made!')
        self.update()

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


