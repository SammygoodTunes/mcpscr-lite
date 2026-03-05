"""
API Base
"""

from pathlib import Path

from mcpscr.api.parser.converter import Converter
from mcpscr.api.parser.parser import JavaClass, Parser, JavaAttribute
from mcpscr.api.parser.parser import JavaInstruction
from mcpscr.api.parser.parser import JavaKeyword
from mcpscr.api.parser.parser import JavaLoop
from mcpscr.api.parser.parser import JavaMethod
from mcpscr.api.parser.parser import JavaModifier
from mcpscr.api.parser.parser import JavaParam
from mcpscr.api.parser.parser import JavaStatement

from mcpscr.logger import logger


class APIBase:
    """ Base for API resources """
    def __init__(self, parser: Parser, class_: JavaClass):
        self.parser = parser
        self._class = class_

    def construct(self, converter: Converter, data: dict) -> None:
        """
        Construct the API
        """
        ...

    def write_to_disk(self, converter: Converter, path: Path) -> bool:
        """
        Write the class to the disk
        :param converter:
        :param path:
        :return bool:
        """
        try:
            path = Path(f'{str(path)}/{self._class.name}')
            converter.write_from(self._class, path)
            return True
        except Exception as e:
            logger.error(f'Failed to write to disk: {e}')
        finally:
            return False

    def add_method(self,
                   name: str,
                   params: list[JavaParam],
                   body: list[JavaInstruction | JavaStatement | JavaLoop]):
        """
        Add method to class
        :param name:
        :param params:
        :param body:
        :return:
        """
        self._class.methods.append(JavaMethod(
            modifier=JavaModifier.PUBLIC.value,
            static=True,
            return_type=JavaKeyword.VOID.value,
            name=name,
            body=body,
            parameters=params
        ))

    def add_attribute(self,
                      modifier: JavaModifier,
                      static: bool,
                      final: bool,
                      type_: str,
                      name: str,
                      value: str):
        """
        Add attribute to class
        :param modifier:
        :param static:
        :param final:
        :param type_:
        :param name:
        :param value:
        :return:
        """
        self._class.attributes.append(JavaAttribute(
            modifier=modifier.value,
            static=static,
            final=final,
            type=type_,
            name=name,
            value=value
        ))
