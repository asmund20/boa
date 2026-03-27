#set document(title: "Implementation of the Boa language")
#set heading(numbering: "1.1")
#set par(justify: true)
#set page(numbering: "1")

#align(center)[
  #title()
  #datetime.display(datetime.today())
]

#outline(depth: 2)

= Introduction
This report documents my solution to the first home exam in IN5630.

The task was to make an interpretation for Boa, a programming language that is a small subset of Python. It is to my knowledge implemented correctly.

= Testing
I do testing both with some unit tests and with a generator that generates a program string that is then parsed and executed, which output is compared to the Python output. The unit tests are the handed out ones testing the interpreter, as well as some positive and negative parse cases. In general, the positive cases are attempted to be covered by the generator. 

== Testing against Python
I use two different generators, one for full programs with parenthesized expressions (`tests/Gen/BoaProgram.hs`) and one for expressions testing associativity and the application of the operators to all the types (`test/Gen/BoaExpression.hs`).

The program generator attempts to generate type-correct programs that never reference undefined variables. It generates both types of statements, that is, assignments and stand-alone expressions. The main logic is in `sizedProgram`, which generates a sized program by generating `size + 1` statements. It generates expressions using `typedSizedExpression`, passing a special type representing an arbitrary type. Assignments are done by generating an identifier (`identGen`) and an expression, and inserting a random number of white space between the identifier, the assignment symbol and the expression. The arbitrary white space is any number of spaces, optionally followed by a newline. A newline is only used where it is allowed in Python, so not between an identifier and the assignment symbol among others.

The expression generator generates very simple expressions, with single-digit numbers, strings of single-digit numbers, booleans, none-values and singleton lists. It does not care about types, so most of the expressions get runtime errors and therefore do not print anything. It is therefore ran 200 times, to get some non-trivial cases.

=== Python and Boa differences
There are some differences between the semantics and syntax of Boa and Python, in the sense that not all valid Boa programs will run the same (or even run) with the Python interpreter. First, there is a syntax difference with respect to the white space, which is completely ignored in Boa, and can lead to syntax errors in Python. This is handled by never generating a space character `' '` directly after a line break. In a similar manner, identifier names are checked to not be any reserved Python keyword, meaning that they will neither be a reserved Boa keyword. A third issue is the `range` function in Boa, which is an iterator in Python. `print(range(2))` will thus output a list in Boa and a range object representation in Python. I solve this by adding a function called `listrange` that calls `list(range(*a))` to the Python version of the source and replacing `range(*a)` with calls to `listrange(*a)` in the code.

A not expression after an arithmetic operator is not syntactically allowed in Python unless it is within parentheses, but is in Boa.

In order to match the operator behaviour of Python in my Boa implementation, I extended the operator functionality to match Python's for all the types we have in Boa. I also made sure to print a list of strings the same as in Python, with logic for choosing the string delimiter.

== Test limitations
The program generator is able to test that correct programs get the same output as in Python, and quite a few of them normally get runtime errors. These runtime errors are not generated on purpose, and there is therefore no reason to believe that they cover most of the cases where a Boa program should result in a runtime error. They do test, however, that for the errors generated, Boa and Python output the same before detecting the error and exiting.

The expression generator is able to test that operators fail when they should, but there is always the possibility that there is another operator that makes the Python program crash than the Boa program.

The program generator only generates syntactically correct programs, except for some possible edge cases that I have not gotten. It does therefore not aid in testing negative cases for the parser. That is due to difficulty in determining whether the parsers fail for the same reason, as well as increased complexity of the generator. Some negative unit tests are included in the parser tests.

If I were to write the generator again, I would not worry about the types. That would mean a lot more errors, but could be solved by tweaking the length of the programs. The benefit would be a simpler generator, and testing properly that all operator and type combinations behave the same as in Python, also the ones that are not allowed.

Given more time, I would implement some more negative test cases for the parser.

= Completeness & Correctness
My implementation is complete, parsing and interpreting all valid Boa programs. It is to my knowledge correct as well, as it passes all the tests except the rare case when they time out. I have not had the opportunity to comprehensively test negative parsing cases, so there could be some programs that should be rejected by the parser that will be attempted to be executed leading to unexpected results.

== `%` operator not fully supported
The `%` operator is used only as a modulo operator in my implementation, and does not format strings like in Python. That is assumed to be out of scope for this home exam, which it also appears to be as it is referenced to as an "arithmetic operator" in the task description.

= Implementation
For the implementation of the interpreter, I simply filled in the missing definitions, adding some helper functions.
For the implementation of the parser, the biggest decision I made was not using `buildExpressionParser` and rather combining expressions manually. The main reason was that I did not know about the feature from the beginning, and did not want to change once I had implemented it. It was a nice learning opportunity to manually specify the precedence.

= Efficiency
The time complexity of the parser is linear. That is because try is only used for keywords, which have a finite length and there is a finite number of them leading to a constant amount of backtracking and linear time.

The time complexity if the interpreter is more involved to analyze. Variable lookup is linear because the environment is a list, which is kept as it was in the precode. The time complexity of assignment is the same as the time complexity for the expression. The time complexity of executing all expressions except the aforementioned variable lookup, function calls, equality and comparison of lists and list comprehensions directly inherit the complexity of performing the same operations in Haskell. The time complexity of print calls is the sum of the complexity of the arguments. The time complexity of range calls is linear with respect to the difference between start and stop. The time complexity of list expressions is the sum of the complexity of the sub-expressions. The time complexity of list comprehensions is exponential with respect to the number of for clauses.

The space complexity of the parser is linear, as it stores the entire program in memory as a syntax tree.

The space complexity of the interpreter is constant for all expressions except range calls and list comprehensions. Range calls are linear in space with respect to the parameters and list comprehensions are exponential in space with respect to the number of for clauses.


= Maintainability
The `operate` function is nearly 100 lines long, and contains duplicated code. In large part, that is due to manually pattern matching both booleans and numbers in both arguments for arithmetic operators. If any changes are to be made to the function, a full rewrite is to be preferred.

= Conclusion
I have implemented a Boa interpreter and parser, and comprehensively tested it against Python with two different program generators.
