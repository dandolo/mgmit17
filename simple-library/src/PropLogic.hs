module PropLogic where

import Data.List

-----------------------------------------------------------
-- Syntax
-----------------------------------------------------------

-- | Atomic formulae
data Atom = P Int

-- | Propositional formulae
data Formula 
    = Atomic Atom
    | Not Formula
    | Or [Formula]
    | And [Formula]


-----------------------------------------------------------
-- Semantics
-----------------------------------------------------------

-- | Our own boolean values
data TruthValue 
    = Oh
    | Meh
    deriving Show

-- | Definition of 'assignment'
data Assignment = Assignment (Atom -> TruthValue)

-- | Interpreting formulae as truth values under a given assigment
eval :: Assignment -> Formula -> TruthValue
eval a@(Assignment assignment) fml =
    case fml of
        Atomic p ->
            assignment p
        Not f ->
            case eval a f of
                Oh -> Meh
                Meh -> Oh
        And (f:fs) ->
            case eval a f of
                Oh -> eval a (And fs)
                Meh -> Meh
        And [] ->
            Oh
        Or (f:fs) -> 
            case eval a f of
                Meh -> eval a (Or fs)
                Oh -> Oh
        Or [] ->
            Oh


-----------------------------------------------------------
-- Normal forms
-----------------------------------------------------------
nnf :: Formula -> Formula
nnf formula =
    case formula of
        Not (Not f) ->
            nnf f
        Not (And fs) ->
            nnf $ Or (map Not fs)
        Not (Or fs) ->
            nnf $ And (map Not fs)
        And fs ->
            And $ map nnf fs
        Or fs ->
            Or $ map nnf fs
        _ -> formula

depth :: Formula -> Int
depth formula =
    case formula of
        Atomic _ ->
            0
        And [] ->
            0
        Or [] ->
            0
        Not f ->
            1 + (depth f)
        And fs -> 
            1 + (maximum $ map depth fs)
        Or fs ->
            1 + (maximum $ map depth fs)

-- | Reomves unecessary nesting in formulae
{--
flatten :: Formula -> Formula
flatten formula =
    case formula of
        And fs ->

        _ -> formula
    where
        fAnd :: Formula
--}

-----------------------------------------------------------
-- Pretty printing and IO
-----------------------------------------------------------


-- | Simplyfied contstructor function for atomic formulae
p = Atomic . P

-- | Symbols to be printed (and, or, not)
data Symbol
    = AndS
    | OrS
    | NotS
    
data Printer = Printer
    { atomicPrinter :: Atom -> String
    , symbolPrinter :: Symbol -> String} 

pretty :: Printer -> Formula -> String
pretty p@(Printer atomic symbol) formula =
    case formula of
        Atomic a ->
            atomic a
        And fs ->
            "(" 
            ++ (intercalate (symbol AndS) $ map (pretty p) fs)
            ++")"
        Or fs ->
            "("
            ++ (intercalate (symbol OrS) $ map (pretty p) fs)
            ++ ")"
        Not f -> 
            (symbol NotS) ++ (pretty p f)

write :: Printer -> Formula -> IO ()
write printer = putStrLn . (pretty printer)


-----------------------------------------------------------
-- LaTeX
-----------------------------------------------------------

texPrinter :: Printer
texPrinter = 
    Printer
        (\(P n) -> "p_{" ++ (show n) ++ "}")
        (\symbol ->
            case symbol of
                AndS -> "\\land "
                OrS  -> "\\lor "
                NotS -> "\\neg ")

-----------------------------------------------------------
-- Unicode
-----------------------------------------------------------

uniPrinter :: Printer
uniPrinter = 
    Printer
        (\(P n) -> "p" ++ (map subsDigit (show n)))
        (\symbol ->
            case symbol of
                AndS -> " ∧ "
                OrS  -> " ∨ "
                NotS -> "¬")
    where
        subsDigit c =
            case c of
                '0' -> '₀'
                '1' -> '₁'
                '2' -> '₂'
                '3' -> '₃'
                '4' -> '₄'
                '5' -> '₅'
                '6' -> '₆'
                '7' -> '₇'
                '8' -> '₈'
                '9' -> '₉'
                _   -> c