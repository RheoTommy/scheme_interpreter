module Scheme.AST (
    -- * Top-level forms
    Toplevel (TDefine, TExpr, TLoad),

    -- * Definitions
    Define (Define),

    -- * Expressions
    Expr (
        ELit,
        EVar,
        ELambda,
        EApp,
        EQuote,
        ESet,
        ELet,
        ELetStar,
        ELetrec,
        EIf,
        ECond,
        EAnd,
        EOr,
        EBegin,
        EDo
    ),

    -- * Constants
    Literal (LNum, LBool, LStr, LNil),

    -- * Supporting types
    Params (Params),
    Body (Body),
) where

import Scheme.SExpr (SExpr)

{- | Top-level form: expression, definition, or load directive.

> Toplevel ::= Exp | Define | (load String)
-}
data Toplevel
    = TDefine Define
    | TExpr Expr
    | TLoad Text
    deriving stock (Show, Eq)

{- | Variable or function definition.

> Define ::= (define Id Exp)

The sugar @(define (f x y) body)@ is desugared to
@(define f (lambda (x y) body))@ by the analyzer.
-}
data Define = Define Text Expr
    deriving stock (Show, Eq)

{- | Expression.

Each constructor corresponds to a production in the spec grammar.
-}
data Expr
    = -- | Literal constant: number, boolean, string, or nil.
      ELit Literal
    | -- | Variable reference.
      EVar Text
    | -- | Lambda abstraction: @(lambda Arg Body)@
      ELambda Params Body
    | -- | Function application: @(Exp Exp*)@
      EApp Expr [Expr]
    | -- | Quoted S-expression. The contents remain as SExpr, not converted to AST.
      EQuote SExpr
    | -- | Assignment: @(set! Id Exp)@
      ESet Text Expr
    | {- | Let binding: @(let [Id] Bindings Body)@
      The optional Text is the name for named let.
      -}
      ELet (Maybe Text) [(Text, Expr)] Body
    | -- | Sequential let: @(let* Bindings Body)@
      ELetStar [(Text, Expr)] Body
    | -- | Recursive let: @(letrec Bindings Body)@
      ELetrec [(Text, Expr)] Body
    | -- | Conditional: @(if Exp Exp [Exp])@
      EIf Expr Expr (Maybe Expr)
    | {- | Multi-way conditional: @(cond (Exp Exp+)* [(else Exp+)])@
      Each clause is (test, body). The else branch is separate.
      -}
      ECond [(Expr, NonEmpty Expr)] (Maybe (NonEmpty Expr))
    | -- | Short-circuit and: @(and Exp*)@
      EAnd [Expr]
    | -- | Short-circuit or: @(or Exp*)@
      EOr [Expr]
    | -- | Sequencing: @(begin Exp*)@
      EBegin [Expr]
    | {- | Iteration: @(do ((Id Exp Exp)*) (Exp Exp*) Body)@
      (bindings, (test, results), body)
      -}
      EDo [(Text, Expr, Expr)] (Expr, [Expr]) Body
    deriving stock (Show, Eq)

{- | Literal constant.

> Const ::= Num | Bool | String | ()
-}
data Literal
    = LNum Integer
    | LBool Bool
    | LStr Text
    | LNil
    deriving stock (Show, Eq)

{- | Lambda parameter specification.

> Arg ::= Id | (Id* [Id . Id])

@Params fixedArgs restArg@ where restArg captures remaining arguments.
Examples:
  @(lambda (x y) ...)@       → @Params ["x", "y"] Nothing@
  @(lambda (x . rest) ...)@  → @Params ["x"] (Just "rest")@
  @(lambda args ...)@        → @Params [] (Just "args")@
-}
data Params = Params [Text] (Maybe Text)
    deriving stock (Show, Eq)

{- | Body of lambda, let, etc.

> Body ::= Define* Exp+

Internal definitions followed by one or more expressions.
The last expression's value is the result.
-}
data Body = Body [Define] (NonEmpty Expr)
    deriving stock (Show, Eq)
