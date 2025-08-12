"""
Java parser and token-finding algorithms
"""

from javalang import tokenizer


def find_doubles(data: str) -> list[tuple[str, tokenizer.Position]]:
    """
    Find all doubles in a data buffer and return a list of tuple results
    :param data:
    :return:
    """
    tokens = tokenizer.tokenize(data)
    result = []
    try:
        for token in tokens:
            if (token.value == '-' and tokens.__next__().value.upper().find("D") == -1) or token.value.upper().find("D") == -1:
                continue
            try:
                if token.value == '-':
                    float(token.value + tokens.__next__().value[:-1])
                    result.append((token.value + tokens.__next__().value, token.position))
                else:
                    float(token.value[:-1])
                    result.append((token.value, token.position))
            except ValueError:
                pass
        return result
    except tokenizer.LexerError:
        return []

def find_floats(data: str) -> list[tuple[str, tokenizer.Position]]:
    """
    Find all floats in a data buffer and return a list of tuple results
    :param data:
    :return:
    """
    tokens = tokenizer.tokenize(data)
    result = []
    try:
        for token in tokens:
            if (token.value == '-' and tokens.__next__().value.upper().find("F") == -1) or token.value.upper().find("F") == -1:
                continue
            try:
                if token.value == '-':
                    float(token.value + tokens.__next__().value[:-1])
                    result.append((token.value + tokens.__next__().value, token.position))
                else:
                    float(token.value[:-1])
                    result.append((token.value, token.position))
            except ValueError:
                pass
        return result
    except tokenizer.LexerError:
        return []

def find_incdec(data: str) -> list[tuple[str, tokenizer.Position]]:
    """
    Find all increments/decrements in a data buffer and return a list of tuple results
    :param data:
    :return:
    """
    tokens = tokenizer.tokenize(data)
    result = []
    try:
        for token in tokens:
            if data.find("for") != -1:
                return []
            if token.value in ['++', '--']:
                result.append((token.value, token.position))
        return result
    except tokenizer.LexerError:
        return []


def find_blocks(data: str, block_list: list[str]) -> list[tuple[str, tokenizer.Position]]:
    """
    Find all blocks in a data buffer and return a list of tuple results
    :param data:
    :param block_list:
    :return:
    """
    tokens = tokenizer.tokenize(data)
    result = []
    try:
        for token in tokens:
            if token.value != 'Block' or tokens.__next__().value != '.':
                continue
            t = tokens.__next__()
            if t.value not in block_list:
                continue
            result.append((t.value, t.position))
        return result
    except StopIteration:
        return result
    except tokenizer.LexerError:
        return []

def gather_blocks(data: str) -> list[str]:
    """
    Gather all the block name variables to re-use for block randomisation
    :param data:
    :return:
    """
    blocks = []
    lines = data.split('\n')
    for line in lines:
        if line.find('public static final Block') != -1 and line.find('[]') == -1:
            blocks.append(line.split()[4].split(';')[0])
    return blocks
