"""
Java parser and token-finding algorithms
"""

from javalang import tokenizer


def find_doubles(data: str) -> list[tuple[str, tokenizer.Position]]:
    """
    Find all doubles in a data buffer and return a list of tuple results
    :param data:
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