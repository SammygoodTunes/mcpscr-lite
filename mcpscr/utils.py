"""
MCPSCR Utilities
"""

from os import path, chdir, getcwd
from random import choice
from subprocess import run
from platform import system as sys
from glob import glob
from string import ascii_letters, digits

from mcpscr.logger import logger

OS_SYS = sys().lower()

MCPSCR_ROOT = getcwd()

MCP_SETUP_SCRIPT = 'setup'
MCP_DECOMP_SCRIPT = 'decompile'
MCP_RECOMP_SCRIPT = 'recompile'
MCP_STCL_SCRIPT = 'test_game'
MCP_STSV_SCRIPT = 'test_server'
MCP_STCL_SCRIPT2 = 'startclient'
MCP_STSV_SCRIPT2 = 'startserver'
MCP_CLEAN_SCRIPT = 'cleanup'
MCP_SETUP_CMD = f'source {MCP_SETUP_SCRIPT}.sh' if OS_SYS == 'linux' else f'{MCP_SETUP_SCRIPT}.bat'
MCP_DECOMP_CMD = f'source {MCP_DECOMP_SCRIPT}.sh' if OS_SYS == 'linux' else f'{MCP_DECOMP_SCRIPT}.bat'
MCP_RECOMP_CMD = f'source {MCP_RECOMP_SCRIPT}.sh' if OS_SYS == 'linux' else f'{MCP_RECOMP_SCRIPT}.bat'
MCP_STCL_CMD = f'source {MCP_STCL_SCRIPT}.sh' if OS_SYS == 'linux' else f'{MCP_STCL_SCRIPT}.bat'
MCP_STSV_CMD = f'source {MCP_STSV_SCRIPT}.sh' if OS_SYS == 'linux' else f'{MCP_STSV_SCRIPT}.bat'
MCP_STCL_CMD2 = f'source {MCP_STCL_SCRIPT2}.sh' if OS_SYS == 'linux' else f'{MCP_STCL_SCRIPT2}.bat'
MCP_STSV_CMD2 = f'source {MCP_STSV_SCRIPT2}.sh' if OS_SYS == 'linux' else f'{MCP_STSV_SCRIPT2}.bat'
MCP_CLEAN_CMD = f'source {MCP_CLEAN_SCRIPT}.sh' if OS_SYS == 'linux' else f'{MCP_CLEAN_SCRIPT}.bat'

MCP_CHECK_PYTHON = f'python2.7 -V 1>/dev/null 2>/dev/null' if OS_SYS == 'linux' else f'python -V 1>NUL 2>NUL'

MAX_SEED_LEN = 16

RAND_D = 'Enable double randomisation'
RAND_F = 'Enable float randomisation'
RAND_I = 'Enable int randomisation'
RAND_B = 'Enable bool randomisation'
RAND_ICDC = 'Enable ++/-- randomisation'
RAND_MATH = 'Enable math func randomisation'
RAND_BL = 'Enable block randomisation'

EXCLUDE_FOR_MATH = ['TextureCompassFX']

def get_fname(file_path: str) -> str:
    """
    Get the file name (without extension) in a path
    :param path:
    :return:
    """
    return path.splitext(path.basename(file_path))[0]

def get_mcp_sources_name(mcp_dir: str) -> str:
    """
    Get the correct MCP sources folder name
    :param mcp_dir:
    :return:
    """
    return 'sources' if path.exists(path.join(mcp_dir, 'sources')) else 'src'

def has_supported_system() -> bool:
    """
    Return whether user has a supported system
    :return:
    """
    return OS_SYS in ['windows', 'linux']


def has_mcp(mcp_dir: str) -> bool:
    """
    Return whether MCP folder exists
    :param mcp_dir:
    :return: bool
    """
    return path.exists(mcp_dir)


def has_mcp_sources(mcp_dir: str) -> bool:
    """
    Return whether MCP source folder exists
    :param mcp_dir:
    :return:
    """
    sources_dir = get_mcp_sources_name(mcp_dir)
    sources_dir_exists = path.exists(path.join(mcp_dir, sources_dir))
    sources_exist = len(glob(path.join(mcp_dir, f'{sources_dir}/**/*.java'), recursive=True)) > 0
    return sources_dir_exists and sources_exist

def has_mcp_backup_sources(mcp_dir: str) -> bool:
    """
    Return whether MCP backup source folder exists
    :param mcp_dir:
    :return:
    """
    backup_sources_dir_exists = path.exists(path.join(mcp_dir, 'backup'))
    backup_sources_exist = len(glob(path.join(mcp_dir, 'backup/**/*.java'), recursive=True)) > 0
    return backup_sources_dir_exists and backup_sources_exist


def run_mcp_script(mcp_dir: str, command: str) -> bool:
    """
    Command run wrapper
    :param mcp_dir:
    :param command:
    :return:
    """
    chdir(mcp_dir)
    status = run(command, shell=True, executable="/bin/bash").returncode == 0
    chdir(MCPSCR_ROOT)
    return status

def has_mcp_script(mcp_dir: str, script: str) -> bool:
    """
    Return True if provided script exists
    :param mcp_dir:
    :param script:
    :return:
    """
    return path.isfile(path.join(mcp_dir, script))

def mcp_setup(mcp_dir: str) -> bool:
    """
    Run setup, return True if successful
    :param mcp_dir:
    :return:
    """
    if not path.exists(path.join(mcp_dir, MCP_SETUP_CMD)):
        logger.info('Skipping setup as does not exist')
        if not path.exists(path.join(mcp_dir, 'runtime')):
            return True
        if run(MCP_CHECK_PYTHON, shell=True).returncode != 0:
            Exception("Python 2.7 is required!")
        logger.info('Patching Linux scripts')
        scripts = glob(path.join(mcp_dir, '*.sh'))
        for script in scripts:
            with open(script) as s:
                data = s.read()
            if data.find('python2.7') != -1:
                continue
            data = data.replace('python', 'python2.7')
            with open(script, 'w') as s:
                s.write(data)
        return True
    return run_mcp_script(mcp_dir, MCP_SETUP_CMD)


def mcp_decompile(mcp_dir: str) -> bool:
    """
    Run decompilation, return True if successful
    :param mcp_dir:
    :return:
    """
    return run_mcp_script(mcp_dir, MCP_DECOMP_CMD)


def mcp_recompile(mcp_dir: str) -> bool:
    """
    Run recompilation, return True if successful
    :param mcp_dir:
    :return:
    """
    return run_mcp_script(mcp_dir, MCP_RECOMP_CMD)


def mcp_start_client(mcp_dir: str) -> bool:
    """
    Run client, return True if successful
    :param mcp_dir:
    :return:
    """
    return run_mcp_script(mcp_dir, MCP_STCL_CMD if has_mcp_script(mcp_dir, MCP_STCL_CMD) else MCP_STCL_CMD2)


def mcp_start_server(mcp_dir: str) -> bool:
    """
    Run server, return True if successful
    :param mcp_dir:
    :return:
    """
    return run_mcp_script(mcp_dir, MCP_STSV_CMD if has_mcp_script(mcp_dir, MCP_STSV_CMD) else MCP_STSV_CMD2)

def mcp_cleanup(mcp_dir: str) -> bool:
    """
    Run cleanup, return True if successful
    :param mcp_dir:
    :return:
    """
    return run_mcp_script(mcp_dir, MCP_CLEAN_CMD)

def random_seed(length: int) -> str:
    """
    Generate a random alphanumerical seed
    :param length:
    :return:
    """
    return ''.join(choice(ascii_letters + digits) for _ in range(length))
