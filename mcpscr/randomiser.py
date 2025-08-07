"""
MCPSCR Randomiser
"""
from copy import copy

from javalang.tokenizer import Position
from random import randint, uniform, choice


def randomise_doubles(
        line: str,
        doubles: list[tuple[str, Position]],
        prob: int,
        r: tuple[float, float]
) -> tuple[str, int]:
    """
    Randomise doubles
    :param line: Line of code
    :param doubles: Double values and positions
    :param prob: Probability of success (0-100)
    :param r: Range (min, max)
    :return:
    """
    changes = 0
    l = ""
    if not doubles:
        return line, changes
    start = 0
    for j, double in enumerate(doubles):
        col = double[1].column - 1
        length = len(double[0])
        if j < len(doubles) - 1:
            end = doubles[j + 1][1].column - 1
        else:
            end = len(line)
        if randint(0, 100) > 100 - prob:
            value = f'{float(double[0][:-1]) + uniform(r[0], r[1]):.2f}D'
            changes += 1
        else:
            value = double[0]
        l += line[start:col] + value + line[col + length:end]
        start = end
    return l, changes


def randomise_floats(
        line: str,
        floats: list[tuple[str, Position]],
        prob: int,
        r: tuple[float, float]
) -> tuple[str, int]:
    """
    Randomise floats
    :param line: Line of code
    :param floats: Float values and positions
    :param prob: Probability of success (0-100)
    :param r: Range (min, max)
    :return:
    """
    changes = 0
    l = ""
    if not floats:
        return line, changes
    start = 0
    for j, decimal in enumerate(floats):
        col = decimal[1].column - 1
        length = len(decimal[0])
        if j < len(floats) - 1:
            end = floats[j + 1][1].column - 1
        else:
            end = len(line)
        if randint(0, 100) > 100 - prob:
            value = f'{float(decimal[0][:-1]) + uniform(r[0], r[1]):.2f}F'
            changes += 1
        else:
            value = decimal[0]
        l += line[start:col] + value + line[col + length:end]
        start = end
    return l, changes

def randomise_incdec(line: str, floats: list[tuple[str, Position]], prob: int) -> tuple[str, int]:
    """
    Randomise increments/decrements
    :param line: Line of code
    :param floats: Float values and positions
    :param prob: Probability of success (0-100)
    :return:
    """
    changes = 0
    l = ""
    if not floats:
        return line, changes
    start = 0
    for j, operator in enumerate(floats):
        col = operator[1].column - 1
        length = len(operator[0])
        if j < len(floats) - 1:
            end = floats[j + 1][1].column - 1
        else:
            end = len(line)
        if randint(0, 100) > 100 - prob:
            values = ['++', '--']
            values.remove(operator[0])
            value = values[0]
            changes += 1
        else:
            value = operator[0]
        l += line[start:col] + value + line[col + length:end]
        start = end
    return l, changes

def randomise_blocks(
        line: str,
        blocks: list[tuple[str, Position]],
        prob: int,
        block_list: list[str]
) -> tuple[str, int]:
    """
    Randomise floats
    :param line: Line of code
    :param blocks: Block values and positions
    :param prob: Probability of success (0-100)
    :param block_list: List of all block names in the game
    :return:
    """
    changes = 0
    l = ""
    if not blocks:
        return line, changes
    start = 0
    for j, block in enumerate(blocks):
        col = block[1].column - 1
        length = len(block[0])
        if j < len(blocks) - 1:
            end = blocks[j + 1][1].column - 1
        else:
            end = len(line)
        if randint(0, 100) > 100 - prob:
            temp = copy(block_list)
            temp.remove(block[0])
            value = choice(block_list)
            changes += 1
        else:
            value = block[0]
        l += line[start:col] + value + line[col + length:end]
        start = end
    return l, changes
