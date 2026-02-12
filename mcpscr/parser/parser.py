"""
Parser
"""

from typing import Any
import javalang
from dataclasses import dataclass
from enum import Enum
from os.path import basename

INDENT = 4

class JavaKeyword(Enum):
    """ Java keywords """
    BREAK = 'break'
    CASE = 'case'
    CATCH = 'catch'
    CLASS = 'class'
    CONTINUE = 'continue'
    DEFAULT = 'default'
    DO = 'do'
    ELSE = 'else'
    ENUM = 'enum'
    EXTENDS = 'extends'
    FINALLY = 'finally'
    FOR = 'for'
    IF = 'if'
    IMPLEMENTS = 'implements'
    IMPORT = 'import'
    INSTANCEOF = 'instanceof'
    INTERFACE = 'interface'
    NEW = 'new'
    PACKAGE = 'package'
    RETURN = 'return'
    SUPER = 'super'
    SWITCH = 'switch'
    SYNCHRONIZED = 'synchronized'
    THIS = 'this'
    THROWS = 'throws'
    TRY = 'try'
    VOID = 'void'
    WHILE = 'while'

class JavaSeparator(Enum):
    """ Java separators """
    BRACKET_CLOSED = ')'
    BRACKET_OPEN = '('
    COLON = ':'
    COMMA = ','
    CURLY_BRACKET_CLOSED = '}'
    CURLY_BRACKET_OPEN = '{'
    PERIOD = '.'
    SEMICOLON = ';'
    SQUARE_BRACKET_CLOSED = ']'
    SQUARE_BRACKET_OPEN = '['

class JavaOperator(Enum):
    """ Java operators """
    ADD = '+'
    ADDEQ = '+='
    AEQ = '='
    ALS = '<<'
    AND = '&'
    ANDEQ = '&='
    ARS = '>>'
    DEC = '--'
    DIV = '/'
    DIVEQ = '/='
    ELSE = ':'
    EQ = '=='
    GE = '>='
    GT = '>'
    IAND = '&&'
    IF = '?'
    INC = '++'
    IOR = '||'
    LE = '<='
    LSL = '<<<'
    LSR = '>>>'
    LT = '<'
    MUL = '*'
    MULEQ = '*='
    NEQ = '!='
    NOT = '!'
    OR = '|'
    OREQ = '|='
    SUB = '-'
    SUBEQ = '-='
    XOR = '^'
    XOREQ = '^='

class JavaModifier(Enum):
    """ Java modifiers """
    ABSTRACT = 'abstract'
    FINAL = 'final'
    PRIVATE = 'private'
    PROTECTED = 'protected'
    PUBLIC = 'public'
    STATIC = 'static'

class JavaBasicTypes(Enum):
    """ Java basic types """
    BOOLEAN = 'boolean'
    DOUBLE = 'double'
    FLOAT = 'float'
    INT = 'int'

class JavaAttributeType(Enum):
    """ Java attribute type """
    BOOL = 0
    INT = 1

class JavaInstructionType(Enum):
    """ Java instruction type """
    UNKNOWN = 0
    ATTRIBUTE_DECLARE = 1
    ATTRIBUTE_ASSIGN = 2
    METHOD_CALL = 3
    VALUE_RETURN = 4
    CONTINUE = 5
    PASS = 6
    LABEL = 7

class JavaStatementType(Enum):
    """ Java statement type """
    IF_ELSE = 0
    TRY_CATCH = 1
    SWITCH_CASE = 2
    SYNCHRONIZED = 3

class JavaLoopType(Enum):
    """ Java statement type """
    FOR = 1
    WHILE = 2
    DO_WHILE = 3

class JavaConditionType(Enum):
    """ Java condition type """
    AND = 0
    OR = 1
    XOR = 2

@dataclass
class JavaAttribute:
    """ Java attribute """
    modifier: JavaModifier | None
    static: bool | None
    final: bool | None
    type: str
    name: str
    value: str | None
    is_array: bool
    array_dim: int | None

@dataclass
class EnumEntry:
    """ Java enum entry """
    name: str | None
    value: str | None

@dataclass
class JavaParam:
    """ Java parameter """
    name: str
    type: str
    is_array: bool
    array_dim: int | None

@dataclass
class JavaInstruction:
    """ Java instruction """
    type: JavaInstructionType
    code: str

@dataclass
class JavaCondition:
    """ Java condition """
    lop: JavaAttribute | str | None
    op: JavaOperator | str | None
    rop: JavaAttribute | str | None
    depth: int

@dataclass
class JavaStatement:
    """ Java statement """
    type: JavaStatementType

@dataclass
class JavaLoop:
    """ Java loop """
    type: JavaLoopType

@dataclass
class JavaCase:
    """ Java switch case """
    value: str
    body: list[JavaInstruction | JavaStatement | JavaLoop]

@dataclass
class JavaInstructionLabel(JavaInstruction):
    """ Java label instruction """
    name: str

@dataclass
class JavaStatementIfElse(JavaStatement):
    """ Java if-elseif-else statement """
    if_c: list[JavaCondition | JavaConditionType]
    if_body: list[JavaInstruction | JavaStatement | JavaLoop]
    elseif_c: list[list[JavaCondition | JavaConditionType]]
    elseif_body: list[list[JavaInstruction | JavaStatement | JavaLoop]]
    else_body: list[JavaInstruction | JavaStatement | JavaLoop]

@dataclass
class JavaStatementTryCatch(JavaStatement):
    """ Java try-catch-finally statement """
    try_body: list[JavaInstruction | JavaStatement | JavaLoop]
    catch_body: list[JavaInstruction | JavaStatement | JavaLoop]
    finally_body: list[JavaInstruction | JavaStatement | JavaLoop]
    catch_exceptions: list[JavaParam]

@dataclass
class JavaStatementSwitchCase(JavaStatement):
    """ Java switch-case statement """
    switch: JavaAttribute | str
    cases: list[JavaCase]
    default: list[JavaInstruction]

@dataclass
class JavaStatementSynchronized(JavaStatement):
    """ Java synchronized block statement """
    object: str | None
    body: list[JavaInstruction | JavaStatement | JavaLoop]

@dataclass
class JavaLoopFor(JavaLoop):
    """ Java for loop """
    iterator: JavaAttribute | str | None
    conditions: list[JavaCondition]
    step: str | None
    body: list[JavaInstruction | JavaStatement | JavaLoop]

@dataclass
class JavaLoopWhile(JavaLoop):
    """ Java while loop """
    conditions: list[JavaCondition]
    body: list[JavaInstruction | JavaStatement | JavaLoop]

@dataclass
class JavaLoopDoWhile(JavaLoop):
    """ Java do-while loop """
    conditions: list[JavaCondition]
    body: list[JavaInstruction | JavaStatement | JavaLoop]

@dataclass
class JavaMethod:
    """ Java method """
    class_name: str
    modifier: JavaModifier | None
    static: bool | None
    final: bool | None
    abstract: bool | None
    return_type: str
    name: str
    parameters: list[JavaParam]
    throws: list[str]
    body: list[JavaInstruction | JavaStatement | JavaLoop]
    variables: list[JavaAttribute]

@dataclass
class JavaMethodCall:
    """ Java method call """
    method: JavaMethod
    args: list[JavaAttribute]

@dataclass
class JavaClass:
    """ Java class/interface """
    package_name: str
    file_name: str
    interface: bool | None
    enum: bool | None
    modifier: JavaModifier | None
    static: bool | None
    final: bool | None
    abstract: bool | None
    imports: list[str]
    name: str
    implements: list[str]
    extends: list[str]
    constructors: list[JavaMethod]
    attributes: list[JavaAttribute]
    enums: list[EnumEntry]
    methods: list[JavaMethod]
    static_blocks: list[list[JavaInstruction | JavaStatement | JavaCondition]]
    instance_blocks: list[list[JavaInstruction | JavaStatement | JavaCondition]]

    def find_attribute_by_name(self, name: str) -> int | None:
        """
        Find class attribute by name and return the index
        :param name:
        :return:
        """
        return next((i for i, a in enumerate(self.attributes) if a.name == name), None)

    def find_method_by_name(self, name: str) -> int | None:
        """
        Find class method by name and return the index
        :param name:
        :return:
        """
        return next((i for i, m in enumerate(self.methods) if m.name == name), None)

@dataclass
class JavaPackage:
    """ Java package """
    name: str
    classes: list[JavaClass]

    def find_methods_by_name(self, name: str) -> tuple[int, int] | None:
        """
        Find all class methods by name and return their indices
        :param name:
        :return: Tuple (class index, method index)
        """
        return next((i, j for i, c in enumerate(self.classes)
                     for j, m in enumerate(c.methods) if m.name == name), None)

    def find_class_by_name(self, name: str) -> int | None:
        """
        Find class by name and return the index
        :param name:
        :return:
        """
        return next((i for i, c in enumerate(self.classes) if c.name == name), None)

@dataclass
class JavaProject:
    """ Java project """
    name: str
    packages: list[JavaPackage]

    def find_package_by_name(self, name: str) -> int | None:
        """
        Find package by name and return the index
        :param name:
        :return:
        """
        return next((p for p in self.packages if p.name == name), None)

class Parser:
    """
    Parser class
    """
    def __init__(self) -> None:
        self.project = JavaProject(name='Minecraft Server', packages=[
            JavaPackage(name='net.minecraft.server', classes=[]),
            JavaPackage(name='net.minecraft.src', classes=[])
        ])
        self.indentation = INDENT
        self.depth = 0
        self.current_package: JavaPackage | None = None
        self.current_class: JavaClass | None = None
        self.current_method: JavaMethod | None = None
        self.inside_class: bool = False
        self.inside_method: bool = False
        self.tokens: Any = None
        self.prev_token: Any = None
        self.token: Any = None

    def t_next(self) -> None:
        """
        Jump to next token
        """
        self.prev_token = self.token
        self.token = self.tokens.__next__()

    def search(self, path: str) -> None:
        """
        Search through tokens of Java file
        :param path:
        """
        self.tokens = javalang.tokenizer.tokenize(open(path).read())

        for token in self.tokens:
            self.token = token
            if self.token.__class__.__name__ == javalang.tokenizer.Keyword.__name__:
                if self.token.value == JavaKeyword.PACKAGE.value:
                    self.create_class(path)
                    continue

                if self.token.value == JavaKeyword.IMPORT.value:
                    self.create_class_imports()
                    continue

            if self.token.__class__.__name__ in [
                javalang.tokenizer.Identifier.__name__,
                javalang.tokenizer.Modifier.__name__,
                javalang.tokenizer.BasicType.__name__,
                javalang.tokenizer.Keyword.__name__
            ]:
                self.build_class()

    def build_class(self) -> None:
        """
        Build a java class and its body (methods, attributes, etc)
        """
        modifier = ''
        if self.token.value in [
            JavaModifier.PUBLIC.value,
            JavaModifier.PRIVATE.value,
            JavaModifier.PROTECTED.value
        ]:
            modifier = self.token.value
            self.t_next()
        static = False
        final = False
        abstract = False
        while self.token.__class__.__name__ == javalang.tokenizer.Modifier.__name__:
            if not static: static = self.token.value == JavaModifier.STATIC.value
            if not final: final = self.token.value == JavaModifier.FINAL.value
            if not abstract: abstract = self.token.value == JavaModifier.ABSTRACT.value
            self.t_next()
        if self.token.value == JavaSeparator.CURLY_BRACKET_OPEN.value:
            self.t_next()
            if static: self.current_class.static_blocks.append(self.get_body(has_braces=True))
            else: self.current_class.instance_blocks.append(self.get_body(has_braces=True))
            return
        if self.token.value == JavaKeyword.CLASS.value:
            self.update_class(modifier, static, final, abstract)
            return
        if self.token.value == JavaKeyword.INTERFACE.value:
            self.current_class.interface = True
            self.update_class(modifier, static, final, abstract)
            return
        if self.token.value == JavaKeyword.ENUM.value:
            self.current_class.enum = True
            self.update_class(modifier, static, final, abstract)
            return
        if self.token.__class__.__name__ == javalang.tokenizer.BasicType.__name__:
            self.create_class_method_or_attribute_basic_type(modifier, static, final, abstract)
            return
        if self.token.value == JavaKeyword.VOID.value:
            self.create_class_method_or_attribute_basic_type(modifier, static, final, abstract)
            return
        if self.token.__class__.__name__ == javalang.tokenizer.Identifier.__name__:
            self.create_class_method_or_attribute_complex_type(modifier, static, final, abstract)

    def create_class(self, path: str) -> None:
        """
        Create a JavaClass data object and get the package
        """
        package_name = ''
        self.t_next()
        while self.token.value != JavaSeparator.SEMICOLON.value:
            package_name += self.token.value
            self.t_next()
        self.current_package = self.project.find_package_by_name(package_name)
        self.current_package.classes.append(JavaClass(
            package_name=self.current_package.name,
            file_name=basename(path),
            interface=None,
            enum=None,
            modifier=None,
            static=None,
            final=None,
            abstract=None,
            imports=[],
            name='',
            implements=[],
            extends=[],
            constructors=[],
            attributes=[],
            enums=[],
            methods=[],
            static_blocks=[],
            instance_blocks=[]
        ))
        self.current_class = self.current_package.classes[len(self.current_package.classes) - 1]

    def create_class_imports(self) -> None:
        """
        Create imports for the current Java class
        """
        if self.current_class is None: return
        import_name = ''
        self.t_next()
        while self.token.value != JavaSeparator.SEMICOLON.value:
            import_name += self.token.value
            self.t_next()
        self.current_class.imports.append(import_name)

    def update_class(self, modifier: JavaModifier, static: bool, final: bool, abstract: bool) -> None:
        """
        Update class properties, and add all classes that it extends and/or implements
        :param modifier: Access modifier
        :param static: Is static?
        :param final: Is final?
        :param abstract: Is abstract?
        """
        self.t_next()
        self.current_class.name = self.token.value
        self.current_class.modifier = modifier
        self.current_class.static = static
        self.current_class.final = final
        self.current_class.abstract = abstract
        self.t_next()
        while self.token.value != JavaSeparator.CURLY_BRACKET_OPEN.value:
            if self.token.value not in [JavaKeyword.IMPLEMENTS.value, JavaKeyword.EXTENDS.value]:
                self.t_next()
                continue
            _type = self.token.value
            self.t_next()
            while (self.token.__class__.__name__ == javalang.tokenizer.Identifier.__name__
                   or self.token.value == JavaSeparator.COMMA.value):
                if self.token.value != JavaSeparator.COMMA.value:
                    if _type == JavaKeyword.IMPLEMENTS.value:
                        self.current_class.implements.append(self.token.value)
                    elif _type == JavaKeyword.EXTENDS.value:
                        self.current_class.extends.append(self.token.value)
                self.t_next()
        self.inside_class = True
        self.depth += 1

    def create_class_method_or_attribute_basic_type(self,
                                                    modifier: JavaModifier,
                                                    static: bool,
                                                    final: bool,
                                                    abstract: bool) -> None:
        """
        Create a class method or attribute of basic (return) type
        :param modifier: Access modifier
        :param static: Is static?
        :param final: Is final?
        :param abstract: Is abstract?
        """
        _type = self.token.value
        self.t_next()
        if self.token.value == JavaSeparator.SQUARE_BRACKET_OPEN.value:
            for _ in range(2):
                _type += self.token.value
                self.t_next()
        _name = self.token.value
        self.t_next()
        if self.token.value == JavaSeparator.BRACKET_OPEN.value:
            self.t_next()
            self.create_class_method(modifier, static, final, abstract, _type, _name)
            return
        self.create_class_attribute(modifier, static, final, _type, _name)

    def create_enum_entry(self, _name: str) -> None:
        """
        Create a Java enum entry
        :param _name:
        """
        if self.token.value == JavaSeparator.BRACKET_OPEN.value:
            self.t_next()
        value = ''
        while self.token.value != JavaSeparator.BRACKET_CLOSED.value:
            value += self.token.value
            if self.token.value == JavaSeparator.COMMA.value: value += ' '
            self.t_next()
        self.t_next() # Assumes a separator character at the end of the entry
        self.current_class.enums.append(EnumEntry(name=_name, value=value))



    def create_class_attribute( self,
                                modifier: JavaModifier,
                                static: bool,
                                final: bool,
                                _type: str,
                                _name: str) -> None:
        """
        Create a class attribute
        :param modifier: Access modifier
        :param static: Is static?
        :param final: Is final?
        :param _type:
        :param _name:
        :return:
        """
        _value = None
        _is_array = False
        _array_dim = 0
        while self.token.value == JavaSeparator.SQUARE_BRACKET_OPEN.value:
            self.t_next() # ]
            self.t_next() # ; or =
            _is_array = True
            _array_dim += 1
        if self.token.__class__.__name__ == javalang.tokenizer.Operator.__name__:
            if self.token.value == JavaOperator.AEQ.value:
                _value = ''
                self.t_next()
                while self.token.value != JavaSeparator.SEMICOLON.value:
                    if self.can_token_be_spaced():
                        _value += f' {self.token.value} '
                    elif self.can_token_be_spaced_right():
                        _value += f'{self.token.value} '
                    else:
                        _value += self.token.value
                    self.t_next()
        if self.token.value == JavaSeparator.SEMICOLON.value:
            self.current_class.attributes.append(JavaAttribute(
                modifier=modifier,
                static=static,
                final=final,
                type=_type,
                name=_name,
                value=_value,
                is_array=_is_array,
                array_dim=_array_dim
            ))

    def create_class_method(self,
                            modifier: JavaModifier,
                            static: bool,
                            final: bool,
                            abstract: bool,
                            return_type: str,
                            name: str) -> None:
        """
        Create a class method
        :param modifier: Access modifier
        :param static: Is static?
        :param final: Is final?
        :param abstract: Is abstract?
        :param return_type:
        :param name:
        """
        throws: list[str] = []
        params: list[JavaParam] = self.get_parameters()
        body: list[JavaInstruction | JavaStatement | JavaLoop] = []
        self.t_next()
        if self.token.value == JavaKeyword.THROWS.value:
            throws = self.get_method_throws()
        if not abstract:
            self.t_next()
            body = self.get_body(has_braces=True)
        self.current_class.methods.append(JavaMethod(
            class_name=self.current_class.name,
            modifier=modifier,
            static=static,
            final=final,
            abstract=abstract,
            return_type=return_type,
            name=name,
            parameters=params,
            throws=throws,
            body=body,
            variables=[]
        ))
        self.current_method = self.current_class.methods[len(self.current_class.methods) - 1]

    def create_class_method_or_attribute_complex_type(self,
                                                      modifier: JavaModifier,
                                                      static: bool,
                                                      final: bool,
                                                      abstract: bool) -> None:
        """
        Create a class method, attribute of complex type or enum entry
        :param modifier: Access modifier
        :param static: Is static?
        :param final: Is final?
        :param abstract: Is abstract?
        :return:
        """
        type_or_name = self.token.value # Type, constructor name or name of enum
        self.t_next()
        if self.token.value == JavaSeparator.BRACKET_OPEN.value:
            if self.current_class.enum and not modifier:
                self.create_enum_entry(type_or_name)
                return
            self.t_next()
            self.create_class_constructor(modifier, static, final, abstract, type_or_name)
            return
        if self.token.value == JavaSeparator.SQUARE_BRACKET_OPEN.value:
            for _ in range(2):
                type_or_name += self.token.value
                self.t_next()
        if self.token.__class__.__name__ == javalang.tokenizer.Identifier.__name__:
            _name = self.token.value
            self.t_next()
            if self.token.value == JavaSeparator.BRACKET_OPEN.value:
                self.t_next()
                self.create_class_method(modifier, static, final, abstract, type_or_name, _name)
                return
            self.create_class_attribute(modifier, static, final, type_or_name, _name)

    def create_class_constructor(self,
                                 modifier: JavaModifier,
                                 static: bool,
                                 final: bool,
                                 abstract: bool,
                                 name: str) -> None:
        """
        Create the class' constructor
        :param modifier: Access modifier
        :param static: Is static?
        :param final: Is final?
        :param abstract: Is abstract?
        :param name:
        :return:
        """
        body: list[JavaInstruction | JavaStatement | JavaLoop] = []
        throws: list[str] = []
        params = self.get_parameters()
        self.t_next()
        if self.token.value == JavaKeyword.THROWS.value:
            throws = self.get_method_throws()
        if not abstract:
            self.t_next()
            body = self.get_body(has_braces=True)
        self.current_class.constructors.append(JavaMethod(
            class_name=self.current_class.name,
            modifier=modifier,
            static=static,
            final=final,
            abstract=abstract,
            return_type=self.current_class.name,
            name=name,
            parameters=params,
            throws=throws,
            body=body,
            variables=[]
        ))
        self.current_method = self.current_class.constructors[len(self.current_class.constructors) - 1]

    def get_method_throws(self) -> list[str]:
        """
        Get what exceptions a method throws
        :return: List of exceptions
        """
        throws: list[str] = []
        if self.token.value != JavaKeyword.THROWS.value:
            return throws
        self.t_next()
        while (self.token.__class__.__name__ != javalang.tokenizer.Keyword.__name__
            and self.token.value not in [
                JavaSeparator.CURLY_BRACKET_OPEN.value,
                JavaSeparator.SEMICOLON.value
            ]):
            if self.token.value == JavaSeparator.COMMA.value:
                self.t_next()
                continue
            throws.append(self.token.value)
            self.t_next()
        return throws

    def get_body(self, has_braces: bool) -> list[JavaInstruction | JavaStatement | JavaLoop]:
        """
        Get body instructions of a scope
        :return: List of JavaInstruction data objects
        """
        body: list[JavaInstruction | JavaStatement | JavaLoop] = []
        while self.token.value != JavaSeparator.CURLY_BRACKET_CLOSED.value:
            instruction = self.get_instruction()
            body.append(instruction)
            if not has_braces: break
        return body

    def get_case_body(self) -> list[JavaInstruction | JavaStatement | JavaLoop]:
        """
        Get body instructions of a switch case
        :return: List of JavaInstruction data objects
        """
        body: list[JavaInstruction | JavaStatement | JavaLoop] = []
        while self.token.value not in [
            JavaKeyword.RETURN.value,
            JavaKeyword.BREAK.value
        ]:
            instruction = self.get_instruction()
            body.append(instruction)
        # Get terminating instruction
        instruction = self.get_instruction()
        body.append(instruction)
        return body

    def get_parameters(self) -> list[JavaParam]:
        """
        Get list of parameters of a method or catch statement
        :return: list of JavaParam data objects
        """
        params: list[JavaParam] = []
        is_array: bool
        while self.token.value != JavaSeparator.BRACKET_CLOSED.value:
            is_array = False
            param_type = self.token.value
            self.t_next()
            param_name = self.token.value
            self.t_next()
            array_dim = 0
            while self.token.value == JavaSeparator.SQUARE_BRACKET_OPEN.value:
                self.t_next() # ]
                self.t_next() # ) ?
                is_array = True
                array_dim += 1
            if self.token.value == JavaSeparator.COMMA.value:
                self.t_next()
            params.append(JavaParam(
                name=param_name,
                type=param_type,
                is_array=is_array,
                array_dim=array_dim
            ))
        return params

    def get_instruction(self) -> JavaInstruction | JavaStatement | JavaLoop:
        """
        Decode and return the current instruction of a method
        :return: JavaInstruction data object
        """
        instruction = JavaInstruction(
            type=JavaInstructionType.UNKNOWN,
            code=''
        )
        code = ''

        _type: JavaInstructionType | JavaStatementType | None = None
        if self.token.value == JavaKeyword.IF.value or self.token.value == JavaKeyword.ELSE.value:
            return self.create_ifelse_statement()

        if self.token.value == JavaKeyword.TRY.value:
            self.t_next()
            return self.create_trycatch_statement()

        if self.token.value == JavaKeyword.SWITCH.value:
            self.t_next()
            return self.create_switchcase_statement()

        if self.token.value == JavaKeyword.SYNCHRONIZED.value:
            self.t_next()
            return self.create_synchronized_statement()

        if self.token.value == JavaKeyword.FOR.value:
            self.t_next()
            return self.create_for_loop()

        if self.token.value == JavaKeyword.WHILE.value:
            self.t_next()
            return self.create_while_loop()

        if self.token.value == JavaKeyword.DO.value:
            self.t_next()
            return self.create_dowhile_loop()

        if self.token.__class__.__name__ == javalang.tokenizer.Identifier.__name__:
            temp = self.token.value
            self.t_next()
            if self.token.value == JavaSeparator.COLON.value:
                self.t_next()
                return JavaInstructionLabel(type=JavaInstructionType.LABEL, name=temp, code='')
            code += temp
            if self.token.__class__.__name__ == javalang.tokenizer.Identifier.__name__:
                code += ' '

        while self.token.value != JavaSeparator.SEMICOLON.value:
            spaced_operator = self.can_token_be_spaced()
            if self.token.value == JavaOperator.GT.value:
                self.t_next()
                if self.token.value == JavaOperator.GT.value:
                    code += f' {self.prev_token.value}{self.token.value}'
                    self.t_next()
                    if self.token.value == JavaOperator.GT.value:
                        code += f'{self.token.value} '
                        self.t_next()
                        continue
                    code += ' '
                    continue
                code += f' {self.prev_token.value} ' if spaced_operator else self.prev_token.value
            spaced_operator = self.can_token_be_spaced()
            code += f' {self.token.value} ' if spaced_operator else self.token.value
            if self.can_token_be_spaced_right():
                code += ' '
                self.t_next()
                continue
            l_ident_or_type = self.token.__class__.__name__ == javalang.tokenizer.Identifier.__name__
            l_ident_or_type |= self.token.__class__.__name__ == javalang.tokenizer.BasicType.__name__
            self.t_next()
            r_identifier = self.token.__class__.__name__ == javalang.tokenizer.Identifier.__name__
            if l_ident_or_type and r_identifier: code += ' '
        # print("GOT INSTRUCTION", code)
        self.t_next()
        instruction.code = code.strip()
        return instruction

    def create_ifelse_statement(self) -> JavaStatement:
        """
        Create a Java if-elseif-else statement instruction with its body instructions
        :return: JavaStatement data object
        """
        statement = JavaStatementIfElse(
            type=JavaStatementType.IF_ELSE,
            if_c=[],
            if_body=[],
            elseif_c=[],
            elseif_body=[],
            else_body=[]
        )
        eoi = False

        while not eoi:
            self.t_next()
            # Check the type of statement (if, else if, else)
            if self.token.value == JavaKeyword.IF.value:
                target = 'elseif'
                self.t_next() # (
                self.t_next() # First param
            elif self.token.value == JavaSeparator.BRACKET_OPEN.value:
                target = 'if'
                self.t_next()
            else:
                target = 'else'
            # Get the conditions
            conditions = [] if target == 'else' else self.get_conditions(end=JavaSeparator.BRACKET_CLOSED.value)

            # Get the body instructions
            has_braces = self.token.value == JavaSeparator.CURLY_BRACKET_OPEN.value
            if has_braces: self.t_next()
            body = self.get_body(has_braces=has_braces)
            if has_braces: self.t_next()
            if self.token.value != JavaKeyword.ELSE.value:
                eoi = True
            if target == 'if':
                statement.if_c = conditions
                statement.if_body = body
                continue
            if target == 'elseif':
                statement.elseif_c.append(conditions)
                statement.elseif_body.append(body)
                continue
            statement.else_body = body
        return statement

    def create_trycatch_statement(self) -> JavaStatement:
        """
        Create a Java try-catch-finally statement instruction with its body instructions
        :return: JavaStatement data object
        """
        statement = JavaStatementTryCatch(
            type=JavaStatementType.TRY_CATCH,
            try_body=[],
            catch_body=[],
            finally_body=[],
            catch_exceptions=[]
        )
        has_braces: bool = self.token.value == JavaSeparator.CURLY_BRACKET_OPEN.value
        if has_braces: self.t_next()
        statement.try_body = self.get_body(has_braces=has_braces)
        if has_braces: self.t_next()
        if self.token.value == JavaKeyword.CATCH.value:
            self.t_next()
            if self.token.value == JavaSeparator.BRACKET_OPEN.value:
                self.t_next()
                statement.catch_exceptions = self.get_parameters()
                self.t_next()
            has_braces = self.token.value == JavaSeparator.CURLY_BRACKET_OPEN.value
            if has_braces: self.t_next()
            statement.catch_body = self.get_body(has_braces=has_braces)
            if has_braces: self.t_next()
        if self.token.value == JavaKeyword.FINALLY.value:
            self.t_next()
            has_braces = self.token.value == JavaSeparator.CURLY_BRACKET_OPEN.value
            if has_braces: self.t_next()
            statement.finally_body = self.get_body(has_braces=has_braces)
            if has_braces: self.t_next()
        return statement

    def create_switchcase_statement(self) -> JavaStatement:
        """
        Create a Java switch-case statement instruction with its body instructions
        :return: JavaStatement data object
        """
        statement = JavaStatementSwitchCase(
            type=JavaStatementType.SWITCH_CASE,
            switch='',
            cases=[],
            default=[]
        )
        eoi = False
        eov = False
        depth = 0
        self.t_next()
        # Get the switch var, which has to exist
        while not eov:
            statement.switch += self.token.value
            self.t_next()
            if self.token.value == JavaSeparator.BRACKET_OPEN.value:
                depth += 1
            if self.token.value == JavaSeparator.BRACKET_CLOSED.value:
                if depth == 0: eov = True
                else: depth -= 1
        self.t_next() # {
        self.t_next() # case, default or '}'
        while not eoi:
            if self.token.value == JavaKeyword.CASE.value:
                self.t_next()
                case_value = self.token.value
                self.t_next() # :
                self.t_next()
                case_body = self.get_case_body()
                statement.cases.append(JavaCase(value=case_value, body=case_body))
            if self.token.value == JavaKeyword.DEFAULT.value:
                self.t_next() # :
                self.t_next()
                while self.token.value != JavaSeparator.CURLY_BRACKET_CLOSED.value:
                    statement.default.append(self.get_instruction())
            if self.token.value == JavaSeparator.CURLY_BRACKET_CLOSED.value:
                eoi = True
                self.t_next()
        return statement

    def create_synchronized_statement(self) -> JavaStatement:
        """
        Create a synchronized block statement with its body instructions
        :return: JavaStatement data object
        """
        statement = JavaStatementSynchronized(
            type=JavaStatementType.SYNCHRONIZED,
            object=None,
            body=[]
        )
        if self.token.value == JavaSeparator.BRACKET_OPEN.value:
            self.t_next()
        _object = ''
        while self.token.value != JavaSeparator.BRACKET_CLOSED.value:
            _object += self.token.value
            self.t_next()
        self.t_next()
        self.t_next()
        statement.object = _object
        statement.body = self.get_body(has_braces=True)
        self.t_next()
        return statement

    def create_for_loop(self) -> JavaLoop:
        """
        Create a for loop instruction with its body instructions
        :return: JavaLoop data object
        """
        loop = JavaLoopFor(
            type=JavaLoopType.FOR,
            iterator=None,
            conditions=[],
            step=None,
            body=[],
        )
        step = ''
        step_end = False
        depth = 0
        if self.token.value == JavaSeparator.BRACKET_OPEN.value:
            self.t_next()
        loop.iterator = self.get_instruction().code
        loop.conditions = self.get_conditions(end=JavaSeparator.SEMICOLON.value)
        while not step_end:
            if self.token.value == JavaSeparator.BRACKET_OPEN.value:
                depth += 1
            if self.token.value == JavaSeparator.BRACKET_CLOSED.value:
                if depth > 0: depth -= 1
                else:
                    step_end = True
                    continue
            spaced_operator = self.can_token_be_spaced()
            step += f' {self.token.value} ' if spaced_operator else self.token.value
            self.t_next()
        self.t_next()
        has_braces = self.token.value == JavaSeparator.CURLY_BRACKET_OPEN.value
        loop.step = step
        self.t_next()
        loop.body = self.get_body(has_braces=has_braces)
        if has_braces: self.t_next()
        return loop

    def create_while_loop(self) -> JavaLoop:
        """
        Create a while loop instruction with its body instructions
        :return: JavaLoop data object
        """
        if self.token.value == JavaSeparator.BRACKET_OPEN.value: self.t_next()
        conditions = self.get_conditions(end=JavaSeparator.BRACKET_CLOSED.value)
        has_braces = self.token.value == JavaSeparator.CURLY_BRACKET_OPEN.value
        if has_braces: self.t_next()
        body = [] if self.token.value == JavaSeparator.SEMICOLON.value else self.get_body(has_braces=has_braces)
        if has_braces or self.token.value == JavaSeparator.SEMICOLON.value: self.t_next()
        return JavaLoopWhile(
            type=JavaLoopType.WHILE,
            conditions=conditions,
            body=body,
        )

    def create_dowhile_loop(self) -> JavaLoop:
        """
        Create a do-while loop instruction with its body instructions
        :return: JavaLoop data object
        """
        if self.token.value == JavaSeparator.CURLY_BRACKET_OPEN.value: self.t_next()
        body = [] if self.token.value == JavaSeparator.SEMICOLON.value else self.get_body(has_braces=True)
        self.t_next()
        self.t_next()
        self.t_next()
        conditions = self.get_conditions(end=JavaSeparator.BRACKET_CLOSED.value)
        self.t_next()
        return JavaLoopDoWhile(
            type=JavaLoopType.DO_WHILE,
            conditions=conditions,
            body=body,
        )

    def get_conditions(self, end: str) -> list[JavaCondition | JavaConditionType]:
        """
        Get all Java if-elseif statement conditions
        :return: List of JavaCondition data objects
        """
        conditions: list[JavaCondition | JavaConditionType] = []
        condition = JavaCondition(lop=None, op=None, rop=None, depth=0)
        buffer = ''
        depth = 0
        eoc = False

        while not eoc:
            if self.token.value == end and depth == 0:
                if buffer:
                    condition.rop = buffer
                    conditions.append(condition)
                eoc = True
                continue
            if self.token.value == JavaSeparator.BRACKET_OPEN.value:
                depth += 1
            if self.token.value == JavaSeparator.BRACKET_CLOSED.value and depth > 0:
                if condition.op:
                    condition.rop = buffer
                    condition.depth = depth
                    conditions.append(condition)
                    condition = JavaCondition(lop=None, op=None, rop=None, depth=0)
                    buffer = ''
                depth -= 1

            if self.token.value == JavaOperator.IAND.value or self.token.value == JavaOperator.IOR.value:
                condition.rop = buffer
                condition.depth = depth
                conditions.append(condition)
                if self.token.value == JavaOperator.IAND.value:
                    conditions.append(JavaConditionType.AND)
                elif self.token.value == JavaOperator.IOR.value:
                    conditions.append(JavaConditionType.OR)
                elif self.token.value == JavaOperator.XOR.value:
                    conditions.append(JavaConditionType.XOR)
                condition = JavaCondition(lop=None, op=None, rop=None, depth=0)
                buffer = ''
                self.t_next()
                continue
            if self.token.value == JavaOperator.NOT.value:
                condition.op = JavaOperator.NOT.value
                self.t_next()
                continue
            if self.token.value in [
                JavaOperator.LT.value,
                JavaOperator.LE.value,
                JavaOperator.GT.value,
                JavaOperator.GE.value,
                JavaOperator.LT.value,
                JavaOperator.EQ.value,
                JavaOperator.NEQ.value,
                JavaKeyword.INSTANCEOF.value,
            ]:
                self.t_next()
                if self.token.value == JavaOperator.GT.value:
                    buffer += self.prev_token.value + self.token.value
                    self.t_next()
                    if self.token.value == JavaOperator.GT.value:
                        buffer += self.token.value
                        self.t_next()
                        continue
                    continue
                else:
                    condition.lop = buffer
                    condition.depth = depth
                    condition.op = self.prev_token.value
                    buffer = ''
                continue
            if ((self.token.value == JavaOperator.SUB.value
            and self.prev_token.__class__.__name__ != javalang.tokenizer.Operator.__name__)
            or (self.token.__class__.__name__ == javalang.tokenizer.Operator.__name__ and self.token.value not in [
            JavaOperator.INC.value, JavaOperator.DEC.value, JavaOperator.SUB.value
            ])):
                buffer += f' {self.token.value} '
            else: buffer += self.token.value

            if (self.token.__class__.__name__ == javalang.tokenizer.Keyword.__name__
                    or self.token.value == JavaSeparator.COMMA.value):
                buffer += ' '

            self.t_next()
        self.t_next()
        return conditions

    def can_token_be_spaced(self) -> bool:
        """
        Return True if token can be spaced on both sides
        :return: bool
        """
        return (
            self.token.__class__.__name__ == javalang.tokenizer.Operator.__name__
            and self.token.value not in [
                JavaOperator.INC.value,
                JavaOperator.DEC.value,
                JavaOperator.NOT.value
            ]
            and self.prev_token.__class__.__name__ != javalang.tokenizer.Operator.__name__
            and self.prev_token.value not in [
                JavaSeparator.CURLY_BRACKET_OPEN.value,
                JavaSeparator.CURLY_BRACKET_CLOSED.value,
            ] or self.token.value == JavaKeyword.INSTANCEOF.value
        )

    def can_token_be_spaced_right(self) -> bool:
        """
        Return True if token can be spaced on the right
        :return: bool
        """
        return ((self.token.__class__.__name__ in [
            javalang.tokenizer.Modifier.__name__,
            javalang.tokenizer.BasicType.__name__,
            javalang.tokenizer.Keyword.__name__,
        ] and self.token.value not in [
            JavaKeyword.SUPER.value,
            JavaKeyword.THIS.value,
            JavaKeyword.CLASS.value
        ] and self.prev_token.value not in [
            JavaSeparator.BRACKET_OPEN.value,
            JavaKeyword.NEW.value,
        ]) or self.token.value in [
            JavaKeyword.NEW.value,
            JavaSeparator.COMMA.value,
        ])
