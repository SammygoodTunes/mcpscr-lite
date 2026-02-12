
from glob import glob
from pprint import pprint
from typing import Any, cast

from mcpscr.parser.parser import INDENT, JavaStatementIfElse, JavaOperator, JavaCondition, JavaConditionType, \
    JavaLoopFor, JavaAttribute, JavaLoopWhile, JavaLoopDoWhile, JavaStatementTryCatch, JavaParam, JavaInstructionLabel, \
    JavaStatementSwitchCase, JavaStatementSynchronized
from mcpscr.parser.parser import JavaClass
from mcpscr.parser.parser import JavaInstruction
from mcpscr.parser.parser import JavaInstructionType
from mcpscr.parser.parser import JavaKeyword
from mcpscr.parser.parser import JavaLoop
from mcpscr.parser.parser import JavaLoopType
from mcpscr.parser.parser import JavaMethod
from mcpscr.parser.parser import JavaModifier
from mcpscr.parser.parser import JavaSeparator
from mcpscr.parser.parser import JavaStatement
from mcpscr.parser.parser import JavaStatementType
from mcpscr.parser.parser import Parser

class Converter:
    """
    Convert data objects back to Java
    """

    def __init__(self):
        self.depth = 0

    def indent(self, s: str) -> str:
        return ' ' * self.depth * INDENT + s

    def wi(self, f: Any, s: str) -> None:
        f.write(self.indent(s))

    @staticmethod
    def get_modifier(o: JavaClass | JavaMethod | JavaAttribute) -> str:
        modifier = o.modifier + ' ' if o.modifier else ''
        if o.static: modifier += JavaModifier.STATIC.value + ' '
        if o.final: modifier +=  JavaModifier.FINAL.value + ' '
        if o.__class__ != JavaAttribute:
            if o.abstract: modifier +=  JavaModifier.ABSTRACT.value + ' '
        return modifier

    @staticmethod
    def get_implements(java_class: JavaClass) -> str:
        implements = ''
        for i, impl in enumerate(java_class.implements):
            if i == 0: implements += JavaKeyword.IMPLEMENTS.value + ' '
            implements += f'{impl}{f", " if i != len(java_class.implements) - 1 else ""}'
        return implements

    @staticmethod
    def get_extends(java_class: JavaClass) -> str:
        extends = ''
        for i, e in enumerate(java_class.extends):
            if i == 0: extends += JavaKeyword.EXTENDS.value + ' '
            extends += f'{e}{f", " if i != len(java_class.extends) - 1 else ""}'
        return extends

    @staticmethod
    def get_parameters(param: list[JavaParam]) -> str:
        params_str = ''
        for i, a in enumerate(param):
            params_str += f'{a.type} {a.name}{'[]' * a.array_dim if a.is_array else ''}{f", " if i != len(param) - 1 else ""}'
        return params_str

    @staticmethod
    def get_throws(throws: list[str]) -> str:
        throws_str = ''
        for i, t in enumerate(throws):
            if i == 0: throws_str += JavaKeyword.THROWS.value + ' '
            throws_str += f'{t}{f", " if i != len(throws) - 1 else ""}'
        return throws_str

    @staticmethod
    def get_conditions(conditions: list[JavaCondition | JavaConditionType]) -> str:
        conditions_str = ''
        for condition in conditions:
            if condition.__class__ == JavaCondition:
                lop, op, rop = condition.lop, condition.op, condition.rop
                if lop: conditions_str += lop
                if op: conditions_str += op if op == JavaOperator.NOT.value else f' {op} '
                if rop: conditions_str += rop
                continue
            if condition.value == JavaConditionType.AND.value:
                conditions_str += f' {JavaOperator.IAND.value} '
                continue
            if condition.value == JavaConditionType.OR.value:
                conditions_str += f' {JavaOperator.IOR.value} '
        return conditions_str

    def get_blocks(self, blocks: list[list[JavaInstruction | JavaStatement | JavaCondition]], static: bool):
        blocks_str = ''
        for i, block in enumerate(blocks):
            if static: blocks_str += self.indent(f'{JavaModifier.STATIC.value} ')
            blocks_str += '{\n'
            blocks_str += self.get_body(block)
            blocks_str += self.indent('}\n')
            if i != len(blocks) - 1: blocks_str += '\n'
        return blocks_str

    def get_body(self, body: list[JavaInstruction | JavaStatement | JavaLoop]) -> str:
        body_str = ''
        self.depth += 1
        for i, instr in enumerate(body):
            if instr.__class__ == JavaStatementIfElse:
                body_str += self.indent(f'{JavaKeyword.IF.value}(')
                body_str += self.get_conditions(cast(JavaStatementIfElse, instr).if_c)
                body_str += f') {{\n'
                if_body = self.get_body(cast(JavaStatementIfElse, instr).if_body)
                body_str += if_body
                body_str += self.indent('} ')
                for j, s in enumerate(cast(JavaStatementIfElse, instr).elseif_c):
                    body_str += f'{JavaKeyword.ELSE.value} {JavaKeyword.IF.value}('
                    body_str += self.get_conditions(s)
                    body_str += f') {{\n'
                    if cast(JavaStatementIfElse, instr).elseif_body[j]:
                        body_str += self.get_body(cast(JavaStatementIfElse, instr).elseif_body[j])
                    body_str += self.indent('} ')
                if cast(JavaStatementIfElse, instr).else_body:
                    body_str +=f'{JavaKeyword.ELSE.value} {{\n'
                    else_body = self.get_body(cast(JavaStatementIfElse, instr).else_body)
                    body_str += else_body
                    body_str += self.indent('}')
                body_str += '\n'
                continue

            if instr.__class__ == JavaStatementTryCatch:
                exceptions = cast(JavaStatementTryCatch, instr).catch_exceptions
                body_str += self.indent(f'{JavaKeyword.TRY.value} {{\n')
                body_str += self.get_body(cast(JavaStatementTryCatch, instr).try_body)
                body_str += self.indent('}')
                body_str += '\n'
                if exceptions:
                    body_str += self.indent(f'{JavaKeyword.CATCH.value} ')
                    params = self.get_parameters(exceptions)
                    body_str += f'({params}) '
                    body_str += '{\n'
                    body_str += self.get_body(cast(JavaStatementTryCatch, instr).catch_body)
                    body_str += self.indent('}')
                    body_str += '\n'
                if cast(JavaStatementTryCatch, instr).finally_body:
                    body_str += self.indent(f'{JavaKeyword.FINALLY.value} {{\n')
                    body_str += self.get_body(cast(JavaStatementTryCatch, instr).finally_body)
                    body_str += self.indent('}')
                    body_str += '\n'
                continue

            if instr.__class__ == JavaStatementSwitchCase:
                switch_var = cast(JavaStatementSwitchCase, instr).switch
                cases = cast(JavaStatementSwitchCase, instr).cases
                default = cast(JavaStatementSwitchCase, instr).default
                body_str += self.indent(f'{JavaKeyword.SWITCH.value}({switch_var}) {{\n')
                for i, case in enumerate(cases):
                    body_str += self.indent(f'{JavaKeyword.CASE.value} {case.value}:\n')
                    body_str += self.get_body(case.body)
                    if i != len(cases) - 1: body_str += '\n'
                if default:
                    body_str += self.indent(f'{JavaKeyword.DEFAULT}:\n')
                    body_str += self.get_body(default)
                body_str += self.indent('}\n')
                continue

            if instr.__class__ == JavaStatementSynchronized:
                _object = cast(JavaStatementSynchronized, instr).object
                body = cast(JavaStatementSynchronized, instr).body
                body_str += self.indent(f'{JavaKeyword.SYNCHRONIZED.value}({_object}) {{\n')
                body_str += self.get_body(body)
                body_str += self.indent('}\n')
                continue

            if instr.__class__ == JavaLoopFor:
                iterator = cast(JavaLoopFor, instr).iterator
                conditions = cast(JavaLoopFor, instr).conditions
                step = cast(JavaLoopFor, instr).step
                body_str += self.indent(f'{JavaKeyword.FOR.value}(')
                if iterator: body_str += iterator
                body_str += '; '
                body_str += self.get_conditions(conditions)
                body_str += '; '
                if step: body_str += step
                body_str += ') {\n'
                body_str += self.get_body(cast(JavaLoopFor, instr).body)
                body_str += self.indent('}\n')
                continue

            if instr.__class__ == JavaLoopWhile:
                body_str += self.indent(f'{JavaKeyword.WHILE.value}(')
                body_str += self.get_conditions(cast(JavaLoopWhile, instr).conditions)
                if cast(JavaLoopWhile, instr).body:
                    body_str += ') {\n'
                    body_str += self.get_body(cast(JavaLoopWhile, instr).body)
                    body_str += self.indent('}\n')
                else:
                    body_str += ');\n'
                continue

            if instr.__class__ == JavaLoopDoWhile:
                body_str += self.indent(f'{JavaKeyword.DO.value} {{\n')
                body_str += self.get_body(cast(JavaLoopDoWhile, instr).body)
                body_str += self.indent(f'}} {JavaKeyword.WHILE.value} (')
                body_str += self.get_conditions(cast(JavaLoopDoWhile, instr).conditions)
                body_str += ');\n'
                continue

            if instr.__class__ == JavaInstructionLabel:
                name = cast(JavaInstructionLabel, instr).name
                body_str += f'{name}:\n'
                continue

            body_str += self.indent(f'{instr.code};\n')
        self.depth -= 1
        return body_str

    def write_from(self, java_class: JavaClass, path: str='') -> None:
        """
        Write JavaClass data object to Java file
        :param java_class: JavaClass data object
        :param path: Path and file name of the class file to be written
        :return:
        """
        modifier: str
        implements: str
        extends: str

        if not path:
            path = f'{java_class.name}'
        if not path:
            path = java_class.file_name.split('.')[0]

        if path.__contains__('.java'):
            path = path.removesuffix('.java')

        with open(f'{path}.java', 'w') as f:
            # Package
            self.wi(f, f'{JavaKeyword.PACKAGE.value} {java_class.package};\n\n')
            # Imports
            for i in java_class.imports:
                f.write(f'{JavaKeyword.IMPORT.value} {i};\n')
            if len(java_class.imports) > 0: f.write(f'\n')
            # Class
            modifier = self.get_modifier(java_class)
            # Always extends first, then implements
            extends = self.get_extends(java_class)
            implements = self.get_implements(java_class)
            if implements and extends: extends += ' '
            if java_class.interface:
                type = JavaKeyword.INTERFACE.value
            elif java_class.enum:
                type = JavaKeyword.ENUM.value
            else:
                type = JavaKeyword.CLASS.value
            self.wi(f, f'{modifier}{type} {java_class.name} {extends}{implements}\n{{\n')
            self.depth += 1
            for i, enum in enumerate(java_class.enums):
                self.wi(f, f'{enum.name}({enum.value}){';\n\n' if i == len(java_class.enums) - 1 else ','}\n')
            for constructor in java_class.constructors:
                modifier = self.get_modifier(constructor)
                params = self.get_parameters(constructor.parameters)
                throws = self.get_throws(constructor.throws)
                if throws: throws = ' ' + throws
                self.wi(f, f'{modifier}{constructor.name}({params}){throws}')
                if constructor.abstract:
                    f.write(';\n\n')
                else:
                    f.write('\n')
                    self.wi(f, '{\n')
                    body = self.get_body(constructor.body)
                    f.write(body)
                    self.wi(f, '}\n\n')
            for method in java_class.methods:
                modifier = self.get_modifier(method)
                return_type = method.return_type
                params = self.get_parameters(method.parameters)
                throws = self.get_throws(method.throws)
                if throws: throws = ' ' + throws
                self.wi(f, f'{modifier}{return_type} {method.name}({params}){throws}')
                if method.abstract:
                    f.write(';\n\n')
                else:
                    f.write('\n')
                    self.wi(f, '{\n')
                    body = self.get_body(method.body)
                    f.write(body)
                    self.wi(f, '}\n\n')
            for attribute in java_class.attributes:
                modifier = self.get_modifier(attribute)
                value = f' = {attribute.value}' if attribute.value else ''
                array = '[]' * attribute.array_dim if attribute.is_array else ''
                self.wi(f, f'{modifier}{attribute.type} {attribute.name}{array}{value};\n')
            static_blocks = self.get_blocks(java_class.static_blocks, static=True)
            instance_blocks = self.get_blocks(java_class.instance_blocks, static=False)
            if static_blocks: f.write('\n')
            f.write(static_blocks)
            if instance_blocks: f.write('\n')
            f.write(instance_blocks)
            self.depth -= 1
            f.write('}')

