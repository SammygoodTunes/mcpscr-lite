"""
API Constants
"""

from typing import Any

from mcpscr.api.parser.converter import Converter
from mcpscr.api.parser.parser import JavaClass, JavaModifier
from mcpscr.api.resources.base import APIBase
from mcpscr.logger import logger


class APIConstants(APIBase):
    """ Global class constants """
    def __init__(self, parser) -> None:
        super().__init__(parser,
            JavaClass(
                 package_name='net.minecraft.mcpscr',
                 imports=['java.util.Random'],
                 file_name=f'{self.__class__.__name__}.java',
                 modifier=JavaModifier.PUBLIC.value,
                 name=f'{self.__class__.__name__}',
            )
        )

    def set_value(self, name: str, value: Any) -> None:
        """
        Set the value of a constant
        :param name:
        :param value:
        """
        index = self._class.find_attribute_by_name(name)
        if index is None:
            logger.warn(f'Index not found for attribute of name \'{name}\'!')
            return
        self._class.attributes[index].value = str(value)

    def get_value(self, name) -> Any | None:
        """
        Set the value of a constant
        :param name:
        :return: value
        """
        index = self._class.find_attribute_by_name(name)
        if index is None:
            return None
        type_ = self._class.attributes[index].type
        if type_ == 'int':
            return int(self._class.attributes[index].value)
        if type_ == 'float':
            return float(self._class.attributes[index].value)
        return self._class.attributes[index].value

    def construct(self, converter: Converter, constants: dict):
        for constant in constants:
            self.add_attribute(modifier=JavaModifier.PUBLIC,
                               static=True,
                               final=True,
                               type_=constants[constant]['type'],
                               name=constant,
                               value=constants[constant]['value'])

            target_class, target = str(constants[constant]['target']).split(':')
            target_type: str = constants[constant]['target-type']
            index_path: list[int] = constants[constant]['index-path']

            if target_type == 'method':
                indices = self.parser.project.find_classes_by_name(target_class)
                if len(indices) == 0:
                    continue
                package_index, class_index = indices[0]
                index = self.parser.project.packages[package_index].classes[class_index].find_method_by_name(target)
                method = self.parser.project.packages[package_index].classes[class_index].methods[index]
                self.parser.project.packages[package_index].classes[class_index].imports.append(
                    'net.minecraft.mcpscr.*'
                )
                body = method.body
                for i in index_path[:-1]:
                    body = body[i]
                body[index_path[-1]] = f'{self.__class__.__name__}.{constant}'
