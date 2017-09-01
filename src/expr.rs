// Integer Expression AST/Evaluator: Rust


/* define an algebraic type representing the abstract syntax tree
 * for our simple expression language:
 * an Expr is either a Const (containing an integer value),
 * or a Plus (of two Exprs),
 * or a Sub (of two Exprs),
 * or a Mul (of two Exprs),
 * or a Div (of two Exprs)
 *
 * we need to Box the Exprs in the recursive types (that is, put
 *   them behind a pointer): otherwise we'd have an infinite-sized structure!
 */
pub enum Expr {
    Const(i32),
    Plus(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
}

impl Expr {
    // pattern-match the top-level and recurse to evaluate
    pub fn eval(&self) -> i32 {
        match *self {
            Expr::Const(n) => n,
            Expr::Plus(ref e1, ref e2) => e1.eval() + e2.eval(),
            Expr::Sub(ref e1, ref e2) => e1.eval() + e2.eval(),
            Expr::Mul(ref e1, ref e2) => e1.eval() * e2.eval(),
            Expr::Div(ref e1, ref e2) => e1.eval() / e2.eval(),
        }
    }
}

fn main() {
    let e = Expr::Mul(
        Box::new(Expr::Const(3)),
        Box::new(
            Expr::Plus(Box::new(Expr::Const(5)), Box::new(Expr::Const(2))))
    );

    println!("{}", e.eval());
}
