"""
MCPSCR Utilities
"""

from os import path, chdir, getcwd
from random import choice
from subprocess import run
from platform import system as sys
from glob import glob
from string import ascii_letters, digits

OS_SYS = sys().lower()

MCPSCR_ROOT = getcwd()

MCP_ROOT = 'mcp'
MCP_SRC = path.join(MCP_ROOT, 'sources', 'minecraft')

MCP_SETUP_SCRIPT = 'setup'
MCP_DECOMP_SCRIPT = 'decompile'
MCP_RECOMP_SCRIPT = 'recompile'
MCP_STCL_SCRIPT = 'test_game'
MCP_STSV_SCRIPT = 'test_server'
MCP_CLEAN_SCRIPT = 'cleanup'
MCP_SETUP_CMD = f'source {MCP_SETUP_SCRIPT}.sh' if OS_SYS == 'linux' else f'{MCP_SETUP_SCRIPT}.bat'
MCP_DECOMP_CMD = f'source {MCP_DECOMP_SCRIPT}.sh' if OS_SYS == 'linux' else f'{MCP_DECOMP_SCRIPT}.bat'
MCP_RECOMP_CMD = f'source {MCP_RECOMP_SCRIPT}.sh' if OS_SYS == 'linux' else f'{MCP_RECOMP_SCRIPT}.bat'
MCP_STCL_CMD = f'source {MCP_STCL_SCRIPT}.sh' if OS_SYS == 'linux' else f'{MCP_STCL_SCRIPT}.bat'
MCP_STSV_CMD = f'source {MCP_STSV_SCRIPT}.sh' if OS_SYS == 'linux' else f'{MCP_STSV_SCRIPT}.bat'
MCP_CLEAN_CMD = f'source {MCP_CLEAN_SCRIPT}.sh' if OS_SYS == 'linux' else f'{MCP_CLEAN_SCRIPT}.bat'

MAX_SEED_LEN = 16

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
    sources_dir_exists = path.exists(path.join(mcp_dir, 'sources'))
    sources_exist = len(glob(path.join(mcp_dir, 'sources/**/*.java'), recursive=True)) > 0
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


def mcp_setup(mcp_dir: str) -> bool:
    """
    Run setup, return True if successful
    :param mcp_dir:
    :return:
    """
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
    return run_mcp_script(mcp_dir, MCP_STCL_CMD)


def mcp_start_server(mcp_dir: str) -> bool:
    """
    Run server, return True if successful
    :param mcp_dir:
    :return:
    """
    return run_mcp_script(mcp_dir, MCP_STSV_CMD)

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
