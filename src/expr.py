# Integer Expression AST/Evaluator/Parser: Python

import sys

# I didn't have internet access in the airport to research Python
#   parser combinator libraries (it turns out there's a Parsec clone),
#   so I wrote my own small custom set of parsers and combinators

# parsers are functions from input to (input, result)
#   where result may be None or the parsed value (this is ugly because
#   we can't distinguish 'optional parser didn't produce a value' from
#   'parser failed to match' from 'parser matched and produced None';
#   dynamic typing and the lack of algebraic result types makes these sorts
#   of things trickier in general
#   but there are better if more tedious options to disambiguate these cases)

# a parser which recognizes one or more whitespace characters
def ws(input):
    # empty string; doesn't match
    if not input:
        return (input, None)

    # no whitespace; doesn't match
    stripped = input.lstrip()
    stripped_chars = len(input) - len(stripped)
    if stripped_chars == 0:
        return (input, None)

    return (stripped, input[:stripped_chars])

# a parser which recognizes an integer
def integer(input):
    if input[0] == '-':
        sign = -1
        remaining = input[1:]
    elif input[0] == '+':
        sign = 1
        remaining = input[1:]
    else:
        sign = 1
        remaining = input

    digits = ''
    while remaining and remaining[0].isdigit():
        digits += remaining[0]
        remaining = remaining[1:]

    if digits:
        return (remaining, sign * int(digits))
    return (input, None)

# parser generators are functions which take some value(s) and produce a parser
#   (that is, a function from input to (input, result))

# produce a parser recognizing the single character ch
def char(ch):
    def parser(input):
        if input and input[0] == ch:
            return (input[1:], ch)
        return (input, None)
    return parser

# parser combinators take parsers and produce new parsers

# produce a parser which runs the input parsers in sequences and
#   succeeds if and only if they all succeed, returning a list of their results
def chain(*parsers):
    def parser(input):
        remaining = input
        results = []
        for p in parsers:
            (remaining, res) = p(remaining)
            if res is None:
                return (input, None)
            results.append(res)
        return (remaining, results)
    return parser

# produce a parser which runs the input parsers in sequences and
#   succeeds if any succeeds, returning the first succeesful result
def alt(*parsers):
    def parser(input):
        for p in parsers:
            (remaining, res) = p(input)
            if res is not None:
                return (remaining, res)
        return (input, None)
    return parser

# produce a parser which runs the input parser and returns a 'successful'
#   default result if it fails, and passes the result through otherwise
def opt(parser, default=True):
    def opt_parser(input):
        (remaining, res) = parser(input)
        if res is not None:
            return (remaining, res)
        return (input, default)
    return opt_parser

# produce a parser which runs the input parser and applies fn to its result
#   if it succeeds (this operation is called `fmap` for applicative functors,
#   which a parser is, in functional programming, which is what we're doing)
def fmap(fn, parser):
    def mapped_parser(input):
        (remaining, res) = parser(input)
        if res is not None:
            return (remaining, fn(res))
        return (input, None)
    return mapped_parser

# produce a parser that matches whatever the input parser matches,
#   optionally surrounded by whitespace
def token(parser):
    return fmap(lambda xs: xs[1], chain(opt(ws), parser, opt(ws)))

# produce a parser that matches whatever the input parser matches,
#   enclosed by whatever is matched by the begin and end parsers
def enclosed(contents, begin, end):
    return fmap(lambda xs: xs[1], chain(begin, contents, end))

# produce a parser that matches whatever the input parser matches,
#   surrounded by '(' ')' parens
def form(parser):
    return enclosed(parser, token(char('(')), token(char(')')))

# define the concrete types of our AST; notice there is no Expr type
#   (what would we do with it, if we had it?)

class Const:
    def __init__(self, val):
        self.val = val

    def eval(self):
        return self.val

# here we define a 'class generator' function to build our
#   binary operator node types from the operation they implement
def BinOp(fn):
    class BinOpClass:
        def __init__(self, lhs, rhs):
            self.lhs = lhs
            self.rhs = rhs

        def eval(self):
            return fn(self.lhs.eval(), self.rhs.eval())

    return BinOpClass

# and use it to fill out the remaining AST node types
Plus = BinOp(int.__add__)
Sub = BinOp(int.__sub__)
Mul = BinOp(int.__mul__)
Div = BinOp(int.__floordiv__)

# parse an expression, as a set of alternatives, using fmap
#   to pass intermediate results through to AST node constructors
def expr(input):
    return alt(
        fmap(Const, token(integer)), # const, or
        fmap(lambda xs: Plus(xs[1], xs[2]),
            form(chain(token(char('+')), expr, expr))),
        fmap(lambda xs: Sub(xs[1], xs[2]),
            form(chain(token(char('-')), expr, expr))),
        fmap(lambda xs: Mul(xs[1], xs[2]),
            form(chain(token(char('*')), expr, expr))),
        fmap(lambda xs: Div(xs[1], xs[2]),
            form(chain(token(char('/')), expr, expr))),
    )(input)

def main(argv):
    source_expr = '(* 3 (+ 5 2))'
    (_, e) = expr(source_expr)

    if e is None:
        print('Invalid expression.', file=sys.stderr)
        return 0

    print('{} = {}'.format(source_expr, e.eval()))

if __name__ == '__main__':
    sys.exit(main(sys.argv))
