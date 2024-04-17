module Interpreter

type varname = string

type typ =
    | TINT
    | TFUN of typ * typ
    | TSTRING
    | TFLOAT
    | TBOOL

type exp =
    | INT of int
    | FLOAT of float
    | BOOL of bool
    | VAR of varname
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
    | IF of exp * exp * exp

let rec check env = function
    | INT i         -> TINT
    | FLOAT c       -> TFLOAT
    | BOOL b        -> TBOOL
    | ADD (e1, e2)  -> match (check env e1, check env e2) with
                            | (TINT, TINT)      -> TINT
                            | (TFLOAT, TFLOAT)  -> TFLOAT
                            | (TFLOAT, TINT)    -> TFLOAT
                            | (TINT, TFLOAT)    -> TFLOAT
                            | (_, _)            -> failwith "Type error: Addition operation expects numeric operands"
    | SUB (e1, e2)  -> match (check env e1, check env e2) with
                            | (TINT, TINT)      -> TINT
                            | (TFLOAT, TFLOAT)  -> TFLOAT
                            | (TFLOAT, TINT)    -> TFLOAT
                            | (TINT, TFLOAT)    -> TFLOAT
                            | (_, _)            -> failwith "Type error: Subtraction operation expects numeric operands"
    | MUL (e1, e2)  -> match (check env e1, check env e2) with
                            | (TINT, TINT)      -> TINT
                            | (TFLOAT, TFLOAT)  -> TFLOAT
                            | (TFLOAT, TINT)    -> TFLOAT
                            | (TINT, TFLOAT)    -> TFLOAT
                            | (_, _)            -> failwith "Type error: Multiplication operation expects numeric operands"
    | DIV (e1, e2)  -> match (check env e1, check env e2) with
                            | (TINT, TINT)      -> TINT
                            | (TFLOAT, TFLOAT)  -> TFLOAT
                            | (TFLOAT, TINT)    -> TFLOAT
                            | (TINT, TFLOAT)    -> TFLOAT
                            | (_, _)            -> failwith "Type error: Division operation expects numeric operands"
    | MOD (e1, e2)  -> match (check env e1, check env e2) with
                            | (TINT, TINT)      -> TINT
                            | (_, _)            -> failwith "Type error: Modulus operation expects integer operands"
    | EQ (e1, e2)   -> match (check env e1, check env e2) with
                            | (TINT, TINT)      -> TBOOL
                            | (TFLOAT, TFLOAT)  -> TBOOL
                            | (TBOOL, TBOOL)    -> TBOOL
                            | (TSTRING, TSTRING) -> TBOOL
                            | (_, _)             -> failwith "Type error: Equality comparison expects operands of the same type"
    | NEQ (e1, e2)   -> match (check env e1, check env e2) with
                            | (TINT, TINT)       -> TBOOL
                            | (TFLOAT, TFLOAT)   -> TBOOL
                            | (TBOOL, TBOOL)     -> TBOOL
                            | (TSTRING, TSTRING) -> TBOOL
                            | (_, _)             -> failwith "Type error: Inequality comparison expects operands of the same type"
    | LT (e1, e2)  -> match (check env e1, check env e2) with
                            | (TINT, TINT)      -> TINT
                            | (TFLOAT, TFLOAT)  -> TFLOAT
                            | (TFLOAT, TINT)    -> TFLOAT
                            | (TINT, TFLOAT)    -> TFLOAT
                            | (_, _)            -> failwith "Type error: Less than comparison expects numeric operands"
    | LTE (e1, e2)  -> match (check env e1, check env e2) with
                            | (TINT, TINT)      -> TINT
                            | (TFLOAT, TFLOAT)  -> TFLOAT
                            | (TFLOAT, TINT)    -> TFLOAT
                            | (TINT, TFLOAT)    -> TFLOAT
                            | (_, _)            -> failwith "Type error: Less than or equal comparison expects numeric operands"
    | GT (e1, e2)  -> match (check env e1, check env e2) with
                            | (TINT, TINT)      -> TINT
                            | (TFLOAT, TFLOAT)  -> TFLOAT
                            | (TFLOAT, TINT)    -> TFLOAT
                            | (TINT, TFLOAT)    -> TFLOAT
                            | (_, _)            -> failwith "Type error: Greater than comparison expects numeric operands"
    | GTE (e1, e2)  -> match (check env e1, check env e2) with
                            | (TINT, TINT)      -> TINT
                            | (TFLOAT, TFLOAT)  -> TFLOAT
                            | (TFLOAT, TINT)    -> TFLOAT
                            | (TINT, TFLOAT)    -> TFLOAT
                            | (_, _)            -> failwith "Type error: Greater than or equal comparison expects numeric operands"
    | AND (e1, e2)  -> match (check env e1, check env e2) with
                            | (TBOOL, TBOOL)    -> TBOOL
                            | (_, _)            -> failwith "Type error: Logical AND operation expects boolean operands"
    | OR (e1, e2)   -> match (check env e1, check env e2) with
                            | (TBOOL, TBOOL)    -> TBOOL
                            | (_, _)            -> failwith "Type error: Logical OR operation expects boolean operands"
    | IF (e1, e2, e3) ->  match check env e1 with
                            | TBOOL ->
                                let type_e2 = check env e2
                                let type_e3 = check env e3
                                if type_e2 = type_e3 then
                                    type_e2  // Return the type of the branches
                                else
                                    failwith "Type error: Branches of the IF expression have different types"
                            | _ -> failwith "Type error: Condition of IF expression must be boolean"
