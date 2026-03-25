#set document(title: "Report on the Boa interpreter implementation")
#set heading(numbering: "1.1")
#set par(justify: true)
#set page(numbering: "1")
#show "Boa": raw("Boa", lang: none)

#align(center)[
  #title()
  #datetime.display(datetime.today())
]

= Introduction
This report documents my solution to the _Project 1_ assignment.

The task was to make an interpretation for Boa, a programming language that is a small subset of python. I have followed the instructions of only changing the `undefined` implementations and passed all tests, so it should be implemented correctly to my knowledge.

= Completeness & Correctness
The supplied pre-code has been completely filled in, so the solution is complete. It also passes all the supplied tests, and should therefore be correct. I have not made any additional test cases, nor had the time to check what parts of the language might not be covered by the supplied tests, so it is possible that there are errors.

= Efficiency
The space and time complexity should match the expected complexity of a program, except some caveats. Variable lookup is linear with respect to the number of bindings. Operations are linear with respect to the number of operations, as expected. The print function is linear with respect to the number of arguments, and the range function is linear with respect to the length of the resulting list. List expressions are also linear with respect to the size of the expressions, assuming that the expressions are of a constant size. The same applies to list comprehensions, they are linear with respect to the length of the resulting list, including the items filtered out, assuming constant expression size.

The space complexity is also linear with respect to the space used by the program.

= Maintainability
The `operate` function matches against approximately 44 patterns, so some extra care might be necessary if updates or fixes are to be made there. The case for `eval ListComprehension` defines some nontrivial functions in `let` statements, that have implicit types. The types should probably be defined explicitly for increased readability.

= Known limitations/errors

The following program does not give the same output in my Boa implementation as python:
```py
[None
    for z_ in range(2)
    if (print(1) not in range(-1, -1, 7))
    for _ in [1]
    if (False % False)
 ]
```
Python will output
```
1
Traceback (most recent call last):
  File "/home/asmund/Documents/IN5630/boa/testpy.py", line 5, in <module>
    if (False % False)
        ~~~~~~^~~~~~~
ZeroDivisionError: division by zero
```
and boa will output
```
1
1
*** Runtime error: BadArgument "Modulo by zero"
```
The difference is due to a difference in when the if clause is executed for the first time, and thus when the runtime error is discovered. Boa will run longer than python, and will thus output at least as much.
