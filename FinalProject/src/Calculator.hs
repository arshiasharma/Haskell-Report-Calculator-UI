module Calculator (State(),
             processCommand, parseInput, populate, display, Operation(..), Command(..), Digit(..), labels, def) where

import           Data.Default (Default (def))

-- operations calculator can handle
data Operation = Add | Sub | Mul | Div | Mod | Exp deriving (Show, Eq)

-- data type of the numbers on the Calculator
data Digit = Zero | One | Two | Three | Four | Five | Six | Seven | Eight | Nine deriving (Show, Eq)

-- data type representing commands executed by calculator
data Command = Digit Digit         -- number
             | Dot                 -- decimal point
             | Operation Operation -- operation
             | Equal               -- =
             | Clear               -- C
             | ClearError          -- CE
             deriving (Show, Eq)


data State = FormulaA     Formula                   -- Formula register A
           | EnteredAandOp Double  Operation          -- A, Op
           | FormulaB     Double  Operation Formula -- A, Op, Formula register B
           | Calculated    Double  Operation Double   -- A, Op, B
           | Error         Double  String             -- A, Error Message
           deriving (Show, Eq)


-- removing leading zeros in the app
trim :: String -> String
trim "0"           = "0"
trim s@('0':'.':_) = s
trim ('0':tl)      = tl
trim x             = x

-- the digit as a string & boolean if there's a float number (dot is pressed)
type Formula = (String, Bool)

-- default Formula 
instance Default State where
  def = FormulaA ("0", False)

-- different cases of States
display :: State -> String
display s =
  case s of
    FormulaA     a     -> trim $ fst a
    EnteredAandOp a _   -> show a
    FormulaB     _ _ b -> trim $ fst b
    Calculated    a _ _ -> show a
    Error         _ msg -> msg

-- formula tuple to a double conversion 
fromFormula :: Formula -> Double
fromFormula = read . fst

-- double conversion to a formula tuple 
asFormula :: Double -> Formula
asFormula x = (show x, x /= (fromInteger . truncate) x) 

-- taking a string and parsing to a command type
parseInput :: String -> Command
parseInput x = case x of
  "0"  -> Digit Zero
  "1"  -> Digit One
  "2"  -> Digit Two
  "3"  -> Digit Three
  "4"  -> Digit Four
  "5"  -> Digit Five
  "6"  -> Digit Six
  "7"  -> Digit Seven
  "8"  -> Digit Eight
  "9"  -> Digit Nine
  "."  -> Dot
  "+"  -> Operation Add
  "-"  -> Operation Sub
  "*"  -> Operation Mul
  "/"  -> Operation Div
  "%"  -> Operation Mod
  "^"  -> Operation Exp
  "="  -> Equal
  "C"  -> Clear
  "CE" -> ClearError
  _    -> undefined

-- labels for the buttons on the app10
labels :: Command -> String
labels c = case c of
  Digit Zero    -> "0"
  Digit One     -> "1"
  Digit Two     -> "2"
  Digit Three   -> "3"
  Digit Four    -> "4"
  Digit Five    -> "5"
  Digit Six     -> "6"
  Digit Seven   -> "7"
  Digit Eight   -> "8"
  Digit Nine    -> "9"
  Dot           -> "."
  Operation Add -> "+"
  Operation Sub -> "-"
  Operation Mul -> "*"
  Operation Div -> "/"
  Operation Mod -> "%"
  Operation Exp -> "^"
  Equal         -> "="
  Clear         -> "C"
  ClearError    -> "CE"

-- compute the calculation/operation 
processCommand :: Command -> State -> State
processCommand cmd = case cmd of
  Digit x      -> addDigit x
  Dot          -> addDot
  Operation op -> applyOp op
  command      -> applyCommand command

-- helper function to parse a command from a string
populate :: String -> State -> State
populate = processCommand . parseInput

addDigit :: Digit -> State -> State
addDigit x s =
  case s of
    (FormulaA a)        -> FormulaA (update a)
    (FormulaB a op b)   -> FormulaB a op (update b)
    (EnteredAandOp a op) -> FormulaB a op (num x, False)
    Calculated {}        -> FormulaA (num x, False)
    _ -> s
  where
    update (a, False) = (ccc a (num x), False)
    update (a, True)  = (ccc a (num x), True)   
    num i = labels (Digit i)
    ccc "0" "0" = "0" -- avoid to create leading 0 digits
    ccc a b     = a ++ b

-- addDot function to handle floats
addDot :: State -> State
addDot s =
  case s of
    (FormulaA a)            -> FormulaA (dotted a)
    (FormulaB a op b)       -> FormulaB a op (dotted b)
    _                        -> s
  where
    dotted (a, False) = (a ++ ".", True)
    dotted (a, True) = (a, True)

-- adding a modulo function to the calculator 
modu :: Double -> Double -> Double
modu a b = fromIntegral $ mod (round a) (round b)

-- setting up formula & getting output of an operation
performCalc :: Double -> Operation -> Double -- A & operation & B
          -> (String -> a) -- error 
          -> (Double -> a) -- result
          -> a 
performCalc _ Div b calcError _  | b == 0 = calcError "Division by Zero!"
performCalc _ Mod b calcError _  | b == 0 = calcError "Mod by Zero!"
performCalc a op  b _ calcResult =
  let f = case op of
            Add -> (+)
            Sub -> (-)
            Mul -> (*)
            Div -> (/)
            Exp -> (**)
            Mod -> modu
  in calcResult $ f a b

-- operation 
applyOp :: Operation -> State -> State
applyOp op s =
  case s of
    (FormulaA a) -> EnteredAandOp (fromFormula a) op
    (FormulaB a op' b) -> performCalc a op' (fromFormula b)
                                     (Error a)
                                     (`EnteredAandOp` op)
    (EnteredAandOp a _) -> Error a "Invalid Operation"
    (Calculated a _ _)  -> EnteredAandOp a op
    _ -> s


-- unary command (C, CE, or =)
applyCommand :: Command -> State -> State
applyCommand cmd s =
  case (cmd, s) of
    (ClearError, Error a _)         -> FormulaA (asFormula a)
    (Clear,      _)                 -> def
    (_,          Error _ _)         -> s
    (Equal,      FormulaA _)       -> s
    (Equal,      EnteredAandOp a _) -> Error a "Invalid Operation"
    (Equal,      FormulaB  a op b) -> calc a op (fromFormula b)
    (Equal,      Calculated a op b) -> calc a op b
    _                               -> s
  where
    calc a op b = performCalc a op b
                  (Error a)
                  (\a' -> Calculated a' op b)