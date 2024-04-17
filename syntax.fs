module Syntax

type funcname = string
type varname  = string
type exp =
    | INT of int
    | VAR of string
    | ADD of exp * exp
    | SUB of exp * exp
    | MUL of exp * exp
    | DIV of exp * exp
    | MOD of exp * exp
    | EQ of exp * exp
    | NEQ of exp * exp
    | LT of exp * exp
    | LTE of exp * exp
    | GT of exp * exp
    | GTE of exp * exp
    | AND of exp * exp
    | OR of exp * exp
    | NEG of exp     // Define unary negation
    | CALL of string * exp list
    | LET of string * exp * exp
    | IF of exp * exp * exp
    | BOOL of bool
    | READ              // Adding READ
    | WRITE  of exp            // Adding WRITE
type funcdef  = funcname * (varname list * exp)
type program  = funcdef list * exp