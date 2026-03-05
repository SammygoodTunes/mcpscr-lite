"""
Generator
"""
from enum import Enum
from random import randint, uniform, seed


class Generator:
    """ Code generator class """

    changes: int = 0

    class Intensity(Enum):
        """ Generator intensity """
        LIGHT = 0
        MEDIUM = 1
        HEAVY = 2
        NOLIMIT = 3

    @classmethod
    def add(cls) -> None:
        """
        Increment change count
        """
        cls.changes += 1

    @classmethod
    def reset(cls) -> None:
        """
        Reset change count
        """
        cls.changes = 0

    @staticmethod
    def set_seed(s: str):
        """
        Set seed for generator
        :param s:
        :return:
        """
        seed(s)

    @staticmethod
    def generate_int(value: int, probability: float, intensity: float, min_: int, max_: int) -> int:
        """
        Generate integer
        :param value:
        :param probability:
        :param intensity:
        :param min_:
        :param max_:
        :return: Generated int or default value
        """
        if uniform(0.0, 100.0) > 100.0 - probability:
            # center = ((max_ - min_) / 2 + min_)
            # min_range = center * (1.0 - intensity / 100.0)
            # max_range = center + (center + min_) * (intensity / 100.0)
            min_range = (min_ - value) * (intensity / 100.0)
            max_range = (max_ - value) * (intensity / 100.0)
            Generator.add()
            return value + randint(round(min_range), round(max_range))
        return value