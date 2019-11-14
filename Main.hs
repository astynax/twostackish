{-# LANGUAGE LambdaCase #-}

module Main where

main :: IO ()
main = do
  -- nil 2 +
  try [nil, n 2, add]
  -- 1 2 ' * call
  try [n 1, n 2, q, add, call]
  -- nil 2 3 4 ' * fold
  try [nil, n 2, n 3, n 4, q, mul, fold]
  -- nil 4 nil 2 3 ' + fold ' * fold
  try [nil, n 4, nil, n 2, n 3, q, add, fold, q, mul, fold]
  --
  print $ populate $ State []
    $ nil : map n [1..1000000] ++ [q, add, fold]
  where
    add = Op $ Action "+" $ binaryOp (+)
    mul = Op $ Action "*" $ binaryOp (*)
    try s = do
      print s
      putStrLn "---"
      print $ populate $ State [] s
      putStrLn ""

data StackItem
  = Number Double
  | Op Op

type Stack = [StackItem]

data State = State
  { dataStack :: Stack
  , opStack   :: Stack
  } deriving (Show)

type Action = State -> State

data Op
  = Nil
  | Quote
  | Halt String
  | Action String Action

type Result = Either (String, State) Stack

instance Show StackItem where
  show (Number x)        = show x
  show (Op Nil)          = "Nil"
  show (Op Quote)        = "'"
  show (Op (Halt s))     = "!" ++ show s
  show (Op (Action s _)) = s

populate :: State -> Result
populate   (State ds [])                     = Right ds
populate   (State ds (x@(Number _)    : os)) = populate (State (x : ds) os)
populate   (State ds (Op (Halt m)     : os)) = Left (m, State ds os)
populate   (State ds (Op Nil          : os)) = populate (State (Op Nil : ds) os)
populate   (State ds (Op (Action _ f) : os)) = populate $ f $ State ds os
populate s@(State ds (Op Quote        : os)) =
  case os of
    (Op x : xs) -> populate $ State (Op x : ds) xs
    _           -> Left ("Bad quotation!", s)

haltWith :: String -> Action
haltWith msg (State ds os) = State ds $ Op (Halt msg) : os

dsOnly :: (Stack -> Stack) -> Action
dsOnly f (State ds os) = State (f ds) os

osOnly :: (Stack -> Stack) -> Action
osOnly f (State ds os) = State ds (f os)

popDS :: (StackItem -> Action) -> Action
popDS _ s@(State [] _)    = haltWith "DS underflow!" s
popDS f (State (x:xs) os) = f x (State xs os)

pushDS :: StackItem -> Action
pushDS x (State ds os) = State (x:ds) os

pushOS :: StackItem -> Action
pushOS x (State ds os) = State ds (x:os)

pushEachOS :: Stack -> Action
pushEachOS = flip (foldr pushOS)

dupDS :: Action
dupDS = popDS $ \x -> pushDS x . pushDS x

dropDS :: Action
dropDS = popDS $ const id

peekDS :: (StackItem -> Action) -> Action
peekDS f = popDS f . dupDS

popNumber :: (Double -> Action) -> Action
popNumber f = peekDS $ \case
  Number x -> f x . dropDS
  _        -> haltWith "Non-number!"

popOp :: (Op -> Action) -> Action
popOp f = peekDS $ \case
  Op x -> f x . dropDS
  _    -> haltWith "Non-op!"

unaryOp :: (Double -> Double) -> Action
unaryOp f = popNumber $ pushDS . Number . f

binaryOp :: (Double -> Double -> Double) -> Action
binaryOp f = popNumber $ \x -> popNumber $ \y -> pushDS (Number $ f x y)

callOp :: Action
callOp = popOp $ pushOS . Op

foldOp :: Action
foldOp = popOp $ \op -> popNumber $ \x -> peekDS $ \case
  Number y ->
    pushEachOS [n y, n x, Op op, q, Op op, fold] . dropDS
  Op Nil   ->
    pushDS (Number x) . dropDS
  _        -> haltWith "Bad operand for folding!"

-- shortcuts
n    = Number
nil  = Op Nil
q    = Op Quote
call = Op $ Action "call" callOp
fold = Op $ Action "fold" foldOp
