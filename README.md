# Compiler_Kotlin
Implementation of a scanner (for the lexical analysis phase) and a parser (for the syntactic analysis phase) for a simple subset of the Kotlin programming language.
The parser returns the abstract syntax tree (AST) representing the Kotlin
program given as input
This subset of Kotlin consists of one function main and the following expressions and commands:

• Expressions: arithmetic expressions; boolean expressions; the print function; the readln fuction;

• Commands: variable declarations and assignments; conditional expressions (if then else); while loops;


Explanation on the parser key commponents:

AST Data Types: Data types represent various elements in the Kotlin subset, such as NumberNode, VariableNode, BinaryOpNode, etc.
Parser Type: A monadic parser that processes tokens and returns AST nodes.
Parser Combinators: Functions like match, consume, and many manage token consumption and error handling.
Expressions: parseExpression, parseTerm, and parseFactor handle arithmetic expressions, including precedence.
Statements: Functions like parseVarDeclaration, parseAssignment, parseIf, parseWhile, etc., create AST nodes for statements.
Main Entry Point: parseMain processes multiple statements and returns the final AST
