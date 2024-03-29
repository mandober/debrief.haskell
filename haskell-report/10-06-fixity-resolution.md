# 10.6 Fixity Resolution


The following is an example implementation of fixity resolution for Haskell expressions. Fixity resolution also applies to Haskell patterns, but patterns are a subset of expressions so in what follows we consider only expressions for simplicity.

The function resolve takes a list in which the elements are expressions or operators, i.e. an instance of the infixexp non-terminal in the context-free grammar. It returns either Just e where e is the resolved expression, or Nothing if the input does not represent a valid expression. In a compiler, of course, it would be better to return more information about the operators involved for the purposes of producing a useful error message, but the Maybe type will suffice to illustrate the algorithm here.

import Control.Monad  

 type Prec   = Int    
type Var    = String  

 data Op = Op String Prec Fixity    
  deriving (Eq,Show)  

 data Fixity = Leftfix | Rightfix | Nonfix    
  deriving (Eq,Show)  

 data Exp = Var Var | OpApp Exp Op Exp | Neg Exp    
  deriving (Eq,Show)  

 data Tok = TExp Exp | TOp Op | TNeg    
  deriving (Eq,Show)  

 resolve :: \[Tok\] -> Maybe Exp    
resolve tokens = fmap fst $ parseNeg (Op "" (-1) Nonfix) tokens    
  where    
    parseNeg :: Op -> \[Tok\] -> Maybe (Exp,\[Tok\])    
    parseNeg op1 (TExp e1 : rest)    
       = parse op1 e1 rest    
    parseNeg op1 (TNeg : rest)    
       = do guard (prec1 < 6)    
            (r, rest') <- parseNeg (Op "-" 6 Leftfix) rest    
            parse op1 (Neg r) rest'    
       where    
          Op \_ prec1 fix1 = op1  

     parse :: Op -> Exp -> \[Tok\] -> Maybe (Exp, \[Tok\])    
    parse \_   e1 \[\] = Just (e1, \[\])    
    parse op1 e1 (TOp op2 : rest)    
       -- case (1): check for illegal expressions    
       | prec1 == prec2 && (fix1 /= fix2 || fix1 == Nonfix)    
       = Nothing  

        -- case (2): op1 and op2 should associate to the left    
       | prec1 > prec2 || (prec1 == prec2 && fix1 == Leftfix)    
       = Just (e1, TOp op2 : rest)  

        -- case (3): op1 and op2 should associate to the right    
       | otherwise    
       = do (r,rest') <- parseNeg op2 rest    
            parse op1 (OpApp e1 op2 r) rest'    
       where    
         Op \_ prec1 fix1 = op1    
         Op \_ prec2 fix2 = op2

The algorithm works as follows. At each stage we have a call

      parse op1 E1 (op2 : tokens)

which means that we are looking at an expression like

      E0 'op1' E1 'op2' ...     (1)

(the caller holds E0). The job of parse is to build the expression to the right of op1, returning the expression and any remaining input.

There are three cases to consider:

1.  if op1 and op2 have the same precedence, but they do not have the same associativity, or they are declared to be nonfix, then the expression is illegal.
2.  If op1 has a higher precedence than op2, or op1 and op2 should left-associate, then we know that the expression to the right of op1 is E1, so we return this to the caller.
3.  Otherwise, we know we want to build an expression of the form E1 'op2' R. To find R, we call parseNeg op2 tokens to compute the expression to the right of op2, namely R (more about parseNeg below, but essentially if tokens is of the form (E2 : rest), then this is equivalent to parse op2 E2 rest). Now, we have E0 'op1' (E1 'op2' R) 'op3' ... where op3 is the next operator in the input. This is an instance of (1) above, so to continue we call parse, with the new E1 == (E1 'op2' R).

To initialise the algorithm, we set op1 to be an imaginary operator with precedence lower than anything else. Hence parse will consume the whole input, and return the resulting expression.

The handling of the prefix negation operator, \-, complicates matters only slightly. Recall that prefix negation has the same fixity as infix negation: left-associative with precedence 6. The operator to the left of \-, if there is one, must have precedence lower than 6 for the expression to be legal. The negation operator itself may left-associate with operators of the same fixity (e.g. +). So for example \-a + b is legal and resolves as (-a) + b, but a + -b is illegal.

The function parseNeg handles prefix negation. If we encounter a negation operator, and it is legal in this position (the operator to the left has precedence lower than 6), then we proceed in a similar way to case (3) above: compute the argument to \- by recursively calling parseNeg, and then continue by calling parse.

Note that this algorithm is insensitive to the range and resolution of precedences. There is no reason in principle that Haskell should be limited to integral precedences in the range 1 to 10; a larger range, or fractional values, would present no additional difficulties.
