"""
MCPSCR Core
"""

from mcpscr import utils, javaparser, randomiser
from mcpscr.logger import logger
from glob import glob
from os import path
from shutil import copytree, rmtree
from random import seed
from json import dumps

from mcpscr.utils import RAND_D


class MCPSCR:
    """
    MCPSCR Class
    """

    def __init__(self, mcp_dir):
        self.mcp_dir = mcp_dir
        self.probability = 0
        self.range = (0.0, 20.0)
        self.seed = ''
        self.blocks = []
        self.settings = {
            utils.RAND_D: False,
            utils.RAND_F: False,
            utils.RAND_I: False,
            utils.RAND_B: False,
            utils.RAND_ICDC: False,
            utils.RAND_MATH: True,
            utils.RAND_BL: False,
        }
        if not utils.has_supported_system():
            raise Exception(f'System [{utils.OS_SYS}] not supported.')
        if not utils.has_mcp(self.mcp_dir):
            raise Exception('MCP not found! Please make sure the \'mcp\' folder exists and contains the MCP.')

    def prepare(self) -> None:
        """
        Prepare MCPSCR
        :return:
        """
        logger.info(f'MCP Directory: {self.mcp_dir}')
        if not utils.mcp_setup(self.mcp_dir):
            raise Exception('Failed to run setup.')
        if not utils.has_mcp_sources(self.mcp_dir):
            logger.info('Sources not found, running decompiler')
            if not utils.mcp_decompile(self.mcp_dir):
                raise Exception('Failed to decompile sources')
            if not utils.mcp_recompile(self.mcp_dir):
                raise Exception('Failed to decompile sources')
            if not utils.has_mcp_backup_sources(self.mcp_dir):
                logger.info('Backing up sources')
                rmtree(path.join(self.mcp_dir, 'backup'))
                copytree(
                    path.join(self.mcp_dir, utils.get_mcp_sources_name(self.mcp_dir)),
                    path.join(self.mcp_dir, 'backup')
                )
        with open(glob(path.join(self.mcp_dir, 'backup', '**/Block.java'), recursive=True)[0]) as blocks_file:
            self.blocks = javaparser.gather_blocks(blocks_file.read())
        logger.info('Welcome to MCPSCR-Lite!')

    def main_menu(self) -> None:
        """
        Start MCPSCR main menu
        :return:
        """
        running = True
        while running:
            option = input(
                '[R] Randomise\n[S] Start game\n[T] Start server'
                '\n[U] Update\n[X] Quick reset\n[C] Clean up \n[O] Options\n[E] Exit\n>> '
            ).lower()
            if option == 'e':
                running = False
            elif option == 'o':
                self.options_menu()
            elif option == 'r':
                self.randomiser_menu()
            elif option == 'x':
                self.reset_sources()
            elif option == 'c':
                self.cleanup()
                if path.exists(path.join(self.mcp_dir, 'runtime')):
                    continue
                self.update()
            elif option == 's':
                self.run_game()
            elif option == 't':
                self.run_server()
            elif option == 'u':
                self.update()

    def options_menu(self) -> None:
        """
        Show MCPSCR options menu
        :return:
        """
        for key in self.settings.keys():
            state = input(f'{key}? [y/n] (currently {self.settings[key]}) ').lower().strip()
            if not state or state not in 'yn':
                continue
            self.settings[key] = True if state == 'y' else False


    def randomiser_menu(self) -> None:
        """
        Show MCPSCR randomiser menu
        :return:
        """
        sources_dir = utils.get_mcp_sources_name(self.mcp_dir)
        source_type = input('Source type: [C] Client-side / [S] Server-side: ').lower()
        if source_type == 'c':
            source_type = 'minecraft'
        elif source_type == 's':
            source_type = 'minecraft_server'
        else:
            logger.error('Invalid source type!')
            return
        randomiser_option = input('Randomise: [W] World Gen / [M] Models / [E] Entity / [A] All: ').lower()
        self.seed = input('Seed (leave blank for random): ')
        if not self.seed.strip():
            self.seed = utils.random_seed(utils.MAX_SEED_LEN)
        try:
            self.probability = int(input('Probability: '))
            if self.probability < 0 or self.probability > 100:
                raise ValueError
        except ValueError:
            logger.error('Invalid probability: Must be an int between 0 and 100!')
            return
        try:
            range_ = input('Range [MIN MAX]: ').split()
            if len(range_) < 2: raise ValueError
            if float(range_[0]) >= float(range_[1]): raise ValueError
            self.range = float(range_[0]), float(range_[1])
        except ValueError:
            logger.error('Invalid range: MIN and MAX must be floats or ints, be spaced out and MIN < MAX!')
            logger.info(f'Using current range: {self.range}')
        if randomiser_option == 'a':
            logger.info('Randomising from all files')
            self.randomise(f'{sources_dir}/{source_type}/**/*.java')
            return
        elif randomiser_option == 'w':
            logger.info('Randomising from World Gen files')
            self.randomise(f'{sources_dir}/{source_type}/**/*Gen*.java')
            return
        elif randomiser_option == 'm':
            logger.info('Randomising from Models files')
            self.randomise(f'{sources_dir}/{source_type}/**/Model*.java')
            return
        elif randomiser_option == 'e':
            logger.info('Randomising from Entity files')
            self.randomise(f'{sources_dir}/{source_type}/**/Entity*.java')
            return
        logger.error('Invalid option!')


    def randomise(self, token: str | list[str]) -> None:
        """
        Randomise some files
        :param token:
        :return:
        """
        logger.info(f'Randomising with:'
                    f'\n{"=" * 16}'
                    f'\nSeed [{self.seed}]'
                    f'\nRange [{self.range}]'
                    f'\n{self.probability}% randomisation chance'
                    f'\nSettings: {dumps(self.settings, indent=4)}'
                    f'\n{"=" * 16}')
        seed(self.seed)
        if isinstance(token, list):
            files = [
                j for i in list(map(lambda pattern: glob(path.join(self.mcp_dir, pattern), recursive=True),
                [f for f in token])) for j in i
            ]
        else:
            files = glob(path.join(self.mcp_dir, token), recursive=True)
        changes = 0
        for file in files:
            with open(file) as f:
                data_lines = f.readlines()
            for i, line in enumerate(data_lines):
                l = line
                if self.settings[utils.RAND_D]:
                    l, c = randomiser.randomise_doubles(line, javaparser.find_doubles(l), self.probability, self.range)
                    changes += c
                if self.settings[utils.RAND_F]:
                    l, c = randomiser.randomise_floats(l, javaparser.find_floats(l), self.probability, self.range)
                    changes += c
                if self.settings[utils.RAND_I]:
                    l, c = randomiser.randomise_ints(l, javaparser.find_ints(l), self.probability, self.range)
                    changes += c
                if self.settings[utils.RAND_B]:
                    l, c = randomiser.randomise_bool(l, javaparser.find_bools(l), self.probability)
                    changes += c
                if self.settings[utils.RAND_ICDC]:
                    l, c = randomiser.randomise_incdec(l, javaparser.find_incdec(l), self.probability)
                    changes += c
                if self.settings[utils.RAND_MATH]:
                    l, c = randomiser.randomise_math(l, javaparser.find_math(l), self.probability)
                    changes += c
                if self.settings[utils.RAND_BL]:
                    l, c = randomiser.randomise_blocks(l, javaparser.find_blocks(l, self.blocks), self.probability, self.blocks)
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
            raise Exception('Could not recompile')

    def reset_sources(self) -> None:
        """
        Replace sources with backup sources
        :return:
        """
        if utils.has_mcp_sources(self.mcp_dir):
            rmtree(path.join(self.mcp_dir, utils.get_mcp_sources_name(self.mcp_dir)))
        copytree(
            path.join(self.mcp_dir, 'backup'),
            path.join(self.mcp_dir, utils.get_mcp_sources_name(self.mcp_dir))
        )
        logger.info("Recompilation necessary to apply changes")

    def cleanup(self) -> None:
        """
        Cleanup and decompile to fresh source code
        :return:
        """
        if not utils.mcp_cleanup(self.mcp_dir):
            raise Exception('Could not clean up')
        if not utils.mcp_decompile(self.mcp_dir):
            raise Exception('Could not decompile')
        if utils.has_mcp_backup_sources(self.mcp_dir):
            rmtree(path.join(self.mcp_dir, 'backup'))
        logger.info('Backing up sources')
        copytree(
            path.join(self.mcp_dir, utils.get_mcp_sources_name(self.mcp_dir)),
            path.join(self.mcp_dir, 'backup')
        )

    def run_game(self):
        """
        Run the client
        :return:
        """
        if not utils.mcp_start_client(self.mcp_dir):
            raise Exception('Could not run the client')

    def run_server(self):
        """
        Run the server
        :return:
        """
        if not utils.mcp_start_server(self.mcp_dir):
            raise Exception('Could not run the server')
