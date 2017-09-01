// Integer Expression AST/Evaluator/Parser: C#
// requires Sprache parser combinator library
// (https://www.nuget.org/packages/Sprache) or
// (https://github.com/sprache/Sprache)

using Sprache;

// define an abstract class representing the abstract syntax of an expression...
abstract class Expr {
    // (including an 'evaluate' operation)
    public abstract int Eval();

    // ... and concrete types representing each alternative AST node
    public class Const : Expr {
        public int Val;
        public override int Eval() => Val;
    }

    public class Plus : Expr {
        public Expr Lhs;
        public Expr Rhs;
        public override int Eval() => Lhs.Eval() + Rhs.Eval();
    }

    public class Sub : Expr {
        public Expr Lhs;
        public Expr Rhs;
        public override int Eval() => Lhs.Eval() - Rhs.Eval();
    }

    public class Mul : Expr {
        public Expr Lhs;
        public Expr Rhs;
        public override int Eval() => Lhs.Eval() * Rhs.Eval();
    }

    public class Div : Expr {
        public Expr Lhs;
        public Expr Rhs;
        public override int Eval() => Lhs.Eval() / Rhs.Eval();
    }
}

static class Parser {
    // a Parser<T> is a Sprache parser which parses a string to produce
    // either the remaining string and a T,
    // or a parse error

    // Form is a parser combinator: it takes a parser as input,
    //   and produces a parser which matches what that parser matches,
    //   surrounded by '(' ')' parens
    static Parser<T> Form<T>(Parser<T> contents)
    {
        return contents.Contained(
                Parse.Char('(').Token(),
                Parse.Char(')').Token());
    }

    // the odd LINQ syntax here reflects the fact that LINQ is a "secret"
    //   syntax sugar for arbitrary monads (and Sprache is a monadic parser
    //   combinator library)
    // in short: to parse a Const, parse a number, then if that succeeded,
    //   return a new Expr.Const wrapping that value; otherwise fail
    // (this is the implicit control flow described by the LINQ notation below)
    static Parser<Expr> Const =
        from val in Parse.Number.Token()
        select new Expr.Const { Val = int.Parse(val) };

    static Parser<Expr> Plus = Form(
        from _ in Parse.Char('+')
        from lhs in Expr
        from rhs in Expr
        select new Expr.Plus { Lhs = lhs, Rhs = rhs }
    );

    static Parser<Expr> Sub = Form(
        from _ in Parse.Char('-')
        from lhs in Expr
        from rhs in Expr
        select new Expr.Sub { Lhs = lhs, Rhs = rhs }
    );

    static Parser<Expr> Mul = Form(
        from _ in Parse.Char('*')
        from lhs in Expr
        from rhs in Expr
        select new Expr.Mul { Lhs = lhs, Rhs = rhs }
    );

    static Parser<Expr> Div = Form(
        from _ in Parse.Char('/')
        from lhs in Expr
        from rhs in Expr
        select new Expr.Div { Lhs = lhs, Rhs = rhs }
    );

    public static Parser<Expr> Expr = Const.Or(Plus).Or(Sub).Or(Mul).Or(Div);
}

class ExprEvaluator {
    public static void Main()
    {
        var expr = Parser.Expr.Parse("(* 3 (+ 5 2))");
        System.Console.WriteLine(expr.Eval());
    }
}
