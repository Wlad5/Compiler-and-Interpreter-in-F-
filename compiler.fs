                                                                // Defining a module to encapsulate the functions
module Compiler
                                                                // Find the position of variable x on the runtime stack:
let rec varpos x = function                                     // varpos - performs recursive searches for the position of a variable x
    | []       -> failwith ("unbound: " + x)                    // in the environment env (which is a stack of variable names)
    | y :: env -> if x = y then 0 else 1 + varpos x env         // if the variable x is found at the head of the environment 'y' it returns 0
                                                                // otherwise the position is incremented by 1 and the function continues the recursive search
                                                                // Generating fresh labels:
let mutable labelCounter = 0                                    // mutable integer used to generate unique labels
let newLabel() =                                                // when newLabel() is called it generates a label in the format "L0", "L1",...
    let this = labelCounter
    labelCounter <- this + 1;
    "L" + string(this)
                                                                // Compile an expression:
let rec comp env = function                                     // compiling the expressions from the custom syntax tree with the 'compile' functions
                                                                // and generating a list of assembly instructions (Asm)
    | Syntax.INT i           -> [Asm.IPUSH i]                   // pushing the integer into the stack
    | Syntax.VAR x           -> [Asm.ILOAD (varpos x env)]      // loading the value of the variable from the stack
    | Syntax.ADD (e1, e2)    -> comp env e1 @                   // compiling the sub-expressions and performing the operation
                                comp ("" :: env) e2 @
                                [Asm.IADD]
    | Syntax.MUL (e1, e2)    -> comp env e1 @                   // compiling the sub-expressions and performing the operation
                                comp ("" :: env) e2 @
                                [Asm.IMUL]
    | Syntax.SUB (e1, e2)    -> comp env e1 @                   // compiling the sub-expressions and performing the operation
                                comp ("" :: env) e2 @
                                [Asm.ISUB]
    | Syntax.EQ (e1, e2)     -> comp env e1 @                   // compiling the sub-expressions and performing the operation
                                comp ("" :: env) e2 @
                                [Asm.IEQ]
    | Syntax.AND (e1, e2)    ->                                 // compiling both operands, performing the logical operation, and possibly adjusts the result 
                                comp env e1 @             
                                comp env e2 @             
                                [Asm.IPUSH (-1)] @        
                                [Asm.IMUL] @              
                                [Asm.IADD]                
    | Syntax.OR (e1, e2)    ->                                  // compiling both operands, performing the logical operation, and possibly adjusts the result 
                                comp env e1 @            
                                comp env e2 @           
                                [Asm.IPUSH (-1)] @      
                                [Asm.IMUL] @            
                                [Asm.IADD]               
    | Syntax.NEG e          -> comp env e @             
                                [Asm.IPUSH (-1)] @   
                                [Asm.IMUL]           

    | Syntax.LET (x, e1, e2) -> comp env e1 @                   // binding a variable in the environment and then compile the body expression
                                comp (x :: env) e2 @
                                [Asm.IPOP] 
    | Syntax.IF (cond, e1, e2) ->                               // compiling the condition, generating labels for branching, compiling both branches
                                let l1 = newLabel() 
                                let l2 = newLabel() 
                                comp env cond @
                                [Asm.IJMP l1] @
                                comp env e1 @
                                [Asm.IJMP l2; Asm.ILAB l1] @
                                comp env e2 @
                                [Asm.ILAB l2] @ 
                                []
    | Syntax.READ            -> [Asm.IREAD]                     // handle input and output instructions
    | Syntax.WRITE e         -> comp env e @                    // handle input and output instructions
                                [Asm.IWRITE] 
                                                                // Compiling a program by first compiling the list of function
                                                                // definitions and then compiling the "main" expression e1:
let rec compProg = function
    | ([], e1) ->
        comp [] e1 @
        [Asm.IHALT]
        
    | ((f, ([x1], e)) :: funcs, e1) ->
        compProg (funcs, e1) @
        [Asm.ILAB f] @
        comp [""; x1] e @                                       // Expect return address and (one) variable (x) on stack
        [Asm.ISWAP] @
        [Asm.IRETN]