namespace Ast

type Expr =
    | Float of float
    | Boolean of bool
    | String of string
    | Negate of Expr
    | Eq of Expr * Expr 
    | NotEq of Expr * Expr
    | Lt of Expr * Expr
    | Lte of Expr * Expr
    | Gt of Expr * Expr
    | Gte of Expr * Expr
    | Concat of Expr * Expr
    | Add of Expr * Expr
    | Sub of Expr * Expr    
    | Mul of Expr * Expr
    | Div of Expr * Expr
    | Pow of Expr * Expr
    | Cell of string
    | Range of string * string
    | SheetRef of string * string
    | SheetRange of string * string * string
    //| Ref of Ref
    | Function of string * Expr List
    | Error of string
//
//and Ref =
//    | Cell of string
//    | Range of string * string
//    | SheetRef of string * string
//    | SheetRange of string * string * string
