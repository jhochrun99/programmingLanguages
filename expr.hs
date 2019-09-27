-- |Represents the two binary operations
-- understood by our toy language, add and
-- subtract

data Operation = Add | Subtract
    deriving (Eq)

-- |Represents an abstract syntax tree
-- of an expression. Each of these
-- constructors represents a different
-- kind of expression.
data Expr = ExprInt Int
          -- ^ an integer literal, such as 5
          | ExprBool Bool
          -- ^ a boolean literal, such as True
          | ExprOp Operation Expr Expr
          -- ^ an binary arithmetic expression,
          -- such as 3+4
          | ExprIsZero Expr
          -- ^ a "comparison-to-zero" test, such
          -- as (3-4)==0
          | ExprIf Expr Expr Expr
          -- ^ a conditional expression, consisting
          -- of a condition, an if clause and an
          -- else clause, such as:
          --  if(3-4==0){9}else{4}
          deriving (Eq)

data Type = TypeInt | TypeBool
    deriving (Eq,Show)
-- |Perform typecheck on a given expression
-- and returns Just TypeInt or Just TypeBool
-- in the event of consistent typing, and
-- returns Nothing in the case of a typing
-- error. For example,
--   ExprOp Add (ExprInt 3) (ExprBool True)
-- is a type error, because you can't add
-- an int to a bool.

typecheck (ExprInt _) =
    {-An integer literal typechecks as an integer-}
    Just TypeInt
typecheck (ExprBool _) =
    {-A boolean literal typechecks as a bool-}
    Just TypeBool
typecheck (ExprOp op e1 e2) =
    {-An arithmetic expression typechecks as an int,
    if both operands are integer expressions, otherwise
    it's badly typed-}
    case typecheck e1 of
        Just TypeInt ->
            case typecheck e2 of
                Just TypeInt -> Just TypeInt
                _ -> Nothing
        _ -> Nothing
typecheck (ExprIsZero expr) =
    case typecheck expr of
        Just TypeInt -> Just TypeBool
        _ -> Nothing
    {-Perform type check on an IsZero expressions, whose argument must evaluate to integer-}
typecheck (ExprIf condition ifexpr elseexpr) =
    case typecheck condition of
        Just TypeBool ->
            case typecheck ifexpr of
                Just TypeInt ->
                    case typecheck elseexpr of
                        Just TypeInt -> Just TypeInt
                        _ -> Nothing
                Just TypeBool ->
                    case typecheck elseexpr of
                        Just TypeBool -> Just TypeBool
                        _ -> Nothing
                _ -> Nothing
        _ -> Nothing
    {-Perform type check on an If expressions. The condition must be a boolean
    expression, and both the if clause and else clause must be of the same type -}

-- |Call this function to test your typecheck function
test_typecheck :: String
test_typecheck =
    case filter (passes . snd) (zip [0..] tests) of
        [] -> "All tests passed"
        x -> "Failed tests: " ++ concatMap showResult x
    where
        showResult (num, (expr, expect)) =
            "in test: "++show num++", got " ++ show (typecheck expr) ++ " but expected " ++ show expect++"; "
        passes (expr, expected) =
            typecheck expr /= expected
        tests = {-test 0-} [(ExprInt 0, Just TypeInt),
                {-test 1-} (ExprOp Add (ExprInt 0) (ExprBool False), Nothing),
                {-test 2-} (ExprOp Add (ExprInt 0) (ExprInt 1), Just TypeInt),
                {-test 3-} (ExprIsZero (ExprInt 0), Just TypeBool),
                {-test 4-} (ExprIf (ExprInt 0) (ExprInt 0) (ExprInt 0), Nothing),
                {-test 5-} (ExprIf (ExprBool True) (ExprIsZero (ExprInt 0)) (ExprInt 0), Nothing),
                {-test 6-} (ExprIf (ExprIf (ExprBool True) (ExprBool True) (ExprBool True)) (ExprIf (ExprBool True) (ExprInt 4) (ExprInt 3)) (ExprIf (ExprBool True) (ExprInt 0) (ExprInt 1)), Just TypeInt)
                ]

data EvalResult = ResultInt Int | ResultBool Bool | ResultError
    deriving (Eq,Show)


-- |Given an expression, produce a result (either an int or a bool)
-- or ResultError if the expression is invalid.
-- This functions does a typecheck first, so you can assume that
-- the expressions are well-typed

eval :: Expr -> EvalResult
eval ev =
    case typecheck ev of
        Nothing -> ResultError
        _ -> case step ev of
                ExprInt i -> ResultInt i
                ExprBool b -> ResultBool b
                _ -> ResultError
    where
        step (ExprInt i) = ExprInt i
        step (ExprBool b) = ExprBool b
        step (ExprOp op e1 e2) =
            let ExprInt e1' = step e1
                ExprInt e2' = step e2
            in case op of
                Add -> ExprInt (e1' + e2')
                Subtract -> ExprInt (e1' - e2')
        step (ExprIsZero expr) =
            case expr of
                ExprInt 0 -> ExprBool True
                _ -> ExprBool False
            {-Produce an ExprBool True if the integer expression
               is equal to zero, or ExprBool False if it isn't -}
        step (ExprIf condition ifpart elsepart) =
            let cond = step condition
            in case cond of
                ExprBool True -> ifpart
                ExprBool False -> elsepart
                ExprIsZero _ -> ExprInt 99
            {-Returns the value of the ifpart expression if
               cond evaluates to true, or elsepart otherwise-}

-- |Call this function to test your eval function
test_eval :: String
test_eval =
    case filter (passes . snd) (zip [0..] tests) of
        [] -> "All tests passed"
        x -> "Failed tests: " ++ concatMap showResult x
    where
        showResult (num, (expr, expect)) =
            "in test: "++show num++", got " ++ show (eval expr) ++ " but expected " ++ show expect++"; "
        passes (expr, expected) =
            eval expr /= expected
        tests = {-test 0-} [(ExprInt 0, ResultInt 0),
                {-test 1-} (ExprOp Add (ExprInt 0) (ExprBool False), ResultError),
                {-test 2-} (ExprOp Add (ExprInt 2) (ExprInt 1), ResultInt 3),
                {-test 3-} (ExprIsZero (ExprInt 0), ResultBool True),
                {-test 4-} (ExprIf (ExprIsZero (ExprInt 0)) (ExprOp Subtract (ExprInt 6) (ExprInt 1)) (ExprInt 9), ResultInt 5)]
