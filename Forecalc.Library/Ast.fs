namespace Ast

type Expr =
    | Float of float
    | Boolean of bool
    | String of string
    | EscapedString of string
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
    | UnresolvedRef of UnresolvedRef
    | Ref of Ref
    | Fun of string * Expr List
    | Error of string

and UnresolvedRef =
    | A1Cell of string
    | A1Range of string * string
    | A1SheetRef of string * string
    | A1SheetRange of string * string * string
    | R1C1Cell of string
    | R1C1Range of string * string
    | R1C1SheetRef of string * string
    | R1C1SheetRange of string * string * string

and Cell = { Row : int ; RowAbs : bool ; Col : int ; ColAbs : bool }

and CellRef = { Sheet : string ; Cell : Cell }

and RangeRef = { Sheet : string ; TopLeft : Cell ; BottomRight : Cell }

and Ref =
    | CellRef of CellRef
    | RangeRef of RangeRef