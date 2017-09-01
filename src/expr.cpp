// Integer Expression AST/Evaluator: C++

// we could do this the same way as in C#, but modern C++ has some nice tricks
// we can use the C++17 std::variant type to build a not-quite-so-ergonomic
//   algebraic sum type and "pattern match" on it

#include <iostream>
#include <memory>
#include <utility>
#include <variant>

namespace exprs {

// concrete types representing AST nodes
struct konst;
struct plus;
struct sub;
struct mul;
struct div;

// the top-level expr type is a std::variant of the alternatives
using expr = std::variant<konst, plus, sub, mul, div>;

// these need to use unique_ptr for the same reason we used Box
//   in Rust; we need pointers to represent recursive structure in a
//   finite-sized type
struct konst { int value; };
struct plus { std::unique_ptr<expr> lhs, rhs; };
struct sub { std::unique_ptr<expr> lhs, rhs; };
struct mul { std::unique_ptr<expr> lhs, rhs; };
struct div { std::unique_ptr<expr> lhs, rhs; };

// define a suite of overloaded functions to evaluate each concrete node type,
//   and abstract expressions
int eval(const expr& e);
int eval(const konst& e) { return e.value; }
int eval(const plus& e) { return eval(*e.lhs) + eval(*e.rhs); }
int eval(const sub& e) { return eval(*e.lhs) - eval(*e.rhs); }
int eval(const mul& e) { return eval(*e.lhs) * eval(*e.rhs); }
int eval(const div& e) { return eval(*e.lhs) / eval(*e.rhs); }

// to evaluate an expr, we use std::visit to invoke the correct
//   function for the type stored in the variant; we have to "lambda-lift"
//   here because "set of overloaded functions" isn't a "thing" that can
//   be a valid template parameter for the std::visit function;
//   "anonymous type with template operator()" (what a generic lambda
//   creates) is
int eval(const expr& e)
{
    return std::visit([](const auto& e) { return eval(e); }, e);
}

}

int main()
{
    using namespace exprs;

    auto e = mul {
        std::make_unique<expr>(konst { 3 }),
        std::make_unique<expr>(plus {
            std::make_unique<expr>(konst { 5 }),
            std::make_unique<expr>(konst { 2 })
        })
    };

    std::cout << eval(e) << '\n';
}
