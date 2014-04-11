
Usage of each interpreter is of the form:

<interpreter name> <program file>




Fractran file format is one integer followed by an arbitrary number of
fractions of the form a/b where both a and b are integers and b is not 0.
Whitespace is not allowed inside a fraction, but is otherwise ignored.




Thue file format is of a list of thue rules followed by the initial program
state, as per the thue specification. Note that whitespace before and after
the lhs or rhs of a rule is counted in that rule, and that a rule consisting
of only whitespace (eg "   ::=   ") is treated as any other rule, not the
termination of the rule list.




Unlambda files should contain a single unlambda term, usually an application.
All characters on a line after a # will be ignored, unless the # was part of
a . or ? function. All whitespace is ignored.




Brainfuck files should contain a number of brainfuck commands. All other
characters are treated as whitespace and summarily ignored. The only
restriction is that all [ loops must be closed with a corresponding ].

