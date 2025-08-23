"""
MCPSCR Randomiser
"""

from copy import copy
from javalang.tokenizer import Position
from random import randint, uniform, choice


def randomise_core(
        line: str,
        values: list[tuple[str, Position]],
        prob: int,
        r: tuple[float, float] | None,
        function
):
    """
    Token-search algorithm
    :param line: Line of code
    :param values: Token values and positions
    :param prob: Probability of success (0-100)
    :param r: Range (min, max)
    :param function: Randomisation function to run
    :return:
    """
    changes = 0
    start = 0
    l = ""
    if not values:
        return line, changes
    for j, value in enumerate(values):
        col = value[1].column - 1
        length = len(value[0])
        end = values[j + 1][1].column - 1 if j < len(values) - 1 else len(line)
        result, success = function(prob, r, value)
        if success:
            changes += 1
        l += line[start:col] + result + line[col + length:end]
        start = end
    return l, changes


def randomise_doubles(
        line: str,
        doubles: list[tuple[str, Position]],
        p: int,
        r: tuple[float, float]
) -> tuple[str, int]:
    """
    Randomise doubles
    :param line: Line of code
    :param doubles: Double values and positions
    :param p: Probability of success (0-100)
    :param r: Range (min, max)
    :return:
    """
    return randomise_core(
        line,
        doubles,
        p,
        r,
        lambda a, b, c: (f'{float(c[0][:-1]) + uniform(b[0], b[1]):.2f}D', True) if randint(0, 100) > 100 - a else (c[0], False)
    )


def randomise_floats(
        line: str,
        floats: list[tuple[str, Position]],
        p: int,
        r: tuple[float, float]
) -> tuple[str, int]:
    """
    Randomise floats
    :param line: Line of code
    :param floats: Float values and positions
    :param p: Probability of success (0-100)
    :param r: Range (min, max)
    :return:
    """
    return randomise_core(
        line,
        floats,
        p,
        r,
        lambda a, b, c: (f'{float(c[0][:-1]) + uniform(b[0], b[1]):.2f}F', True) if randint(0, 100) > 100 - a else (c[0], False)
    )


def randomise_ints(
        line: str,
        ints: list[tuple[str, Position]],
        p: int,
        r: tuple[float, float]
) -> tuple[str, int]:
    """
    Randomise floats
    :param line: Line of code
    :param ints: Integer values and positions
    :param p: Probability of success (0-100)
    :param r: Range (min, max)
    :return:
    """
    value = randint(round(r[0]), round(r[1]))
    return randomise_core(
        line,
        ints,
        p,
        r,
        lambda a, b, c: (f'{int(c[0]) + value if int(c[0]) + value >= 1 else 1}', True) if randint(0, 100) > 100 - a else (c[0], False)
    )


def randomise_incdec(line: str, operators: list[tuple[str, Position]], p: int) -> tuple[str, int]:
    """
    Randomise increments/decrements
    :param line: Line of code
    :param operators: Float values and positions
    :param p: Probability of success (0-100)
    :return:
    """
    def f(a, _, c):
        if randint(0, 100) > 100 - a:
            return ['++', '--'][(['++', '--'].index(c[0]) + 1) % 2], True
        return c[0], False
    return randomise_core(line, operators, p, None, f)


def randomise_bool(line: str, bools: list[tuple[str, Position]], p: int) -> tuple[str, int]:
    """
    Randomise booleans
    :param line: Line of code
    :param bools: Float values and positions
    :param p: Probability of success (0-100)
    :return:
    """
    def f(a, _, c):
        if randint(0, 100) > 100 - a:
            return ['true', 'false'][(['true', 'false'].index(c[0]) + 1) % 2], True
        return c[0], False
    return randomise_core(line, bools, p, None, f)


def randomise_blocks(
        line: str,
        blocks: list[tuple[str, Position]],
        p: int,
        block_list: list[str]
) -> tuple[str, int]:
    """
    Randomise blocks
    :param line: Line of code
    :param blocks: Block values and positions
    :param p: Probability of success (0-100)
    :param block_list: List of all block names in the game
    :return:
    """
    def f(a, _, c):
        if randint(0, 100) > 100 - a:
            temp = copy(block_list)
            temp.remove(c[0])
            return choice(block_list), True
        return c[0], False
    # Patches
    if line.find('(BlockFlower)Block.blocksList') != -1:
        line = line.replace('(BlockFlower)', '')
    return randomise_core(line, blocks, p, None, f)
