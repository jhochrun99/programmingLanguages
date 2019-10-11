import qualified Data.Map as Map
import Data.List (intercalate)

data Operation = Add | Subtract
    deriving (Eq)

-- |A program is just a sequence of statements
type Program = [Statement]

-- |Variable naems are represented as strings
type VarName = String

-- |We distinguish three kinds of statements:
-- assignment statements, print statements, and
-- if statements. Note that if statements are
-- different from if exprssions, in that the
-- the legs of the former are themselves groups
-- groups of satements, whereas the legs of the
-- the latter are expressions (that must be of the
-- same type)
data Statement = StatementAssign VarName Expr
               | StatementPrint Expr
               | StatementIf Expr [Statement] [Statement]

-- |The Expr type has been augmented with a ExprVar
-- constructor for referring to variables by name
-- in the envionrment.
data Expr = ExprInt Int
          | ExprBool Bool
          | ExprOp Operation Expr Expr
          | ExprIsZero Expr
          | ExprIf Expr Expr Expr
          | ExprVar VarName
          deriving (Eq)

data Type = TypeInt | TypeBool
    deriving (Eq)

data VarVal = VarInt Int | VarBool Bool

-- |The Environment type is really a Map that maps
-- variable names to variable values. See the documentation
-- of the Data.Map module for info on accessing this type.
type Environment = Map.Map VarName VarVal

-- |Typecheck the given expression, in the context
-- of the variables defined in the environment.
-- See the tests for an idea of how variables
-- can be referred to from expressions.
typecheck :: Environment -> Expr -> Maybe Type
typecheck _ (ExprInt _) = Just TypeInt
typecheck _ (ExprBool _) = Just TypeBool
typecheck env (ExprOp op e1 e2) =
    case (typecheck env e1, typecheck env e2) of
        (Just TypeInt, Just TypeInt) -> Just TypeInt
        _ -> Nothing
typecheck env (ExprIsZero expr) =
    case typecheck env expr of
        Just TypeInt -> Just TypeBool
        _ -> Nothing
typecheck env (ExprIf cond ifexpr elseexpr) =
    case (typecheck env cond, typecheck env ifexpr, typecheck env elseexpr) of
        (Just TypeBool, Just a, Just b) ->
            if a == b then Just a else Nothing
        _ ->
            Nothing
typecheck env (ExprVar vname) =
    case Map.lookup vname env of
        Just (VarInt _) -> Just TypeInt
        Just (VarBool _) -> Just TypeBool
        _ -> Nothing

test_typecheck :: String
test_typecheck =
    case map fst $ filter (not . passes . snd) (zip [0..] tests) of
        [] -> "All tests passed"
        x -> "Failed tests: " ++ show x
    where
        passes (expr, expected) =
            typecheck env expr == expected
        env = Map.fromList [("x", VarInt 3), ("y", VarBool True)]
        tests = {-test 0-} [(ExprInt 0, Just TypeInt),
                {-test 1-} (ExprOp Add (ExprInt 0) (ExprBool False), Nothing),
                {-test 2-} (ExprOp Add (ExprInt 0) (ExprInt 1), Just TypeInt),
                {-test 3-} (ExprIsZero (ExprInt 0), Just TypeBool),
                {-test 4-} (ExprVar "z", Nothing),
                {-test 5-} (ExprVar "x", Just TypeInt),
                {-test 6-} (ExprIsZero (ExprVar "x"), Just TypeBool)
                ]  

data EvalResult = ResultInt Int | ResultBool Bool | ResultError
    deriving (Eq)

-- |Evaluate the given expression, in the context
-- of the variables defined in the environment.
-- See the tests for an idea of how variables
-- can be referred to from expressions.
eval :: Environment -> Expr -> EvalResult
eval env ev =
    case typecheck env ev of
        Nothing -> ResultError
        _ -> step ev
    where 
        step (ExprInt i) = ResultInt i
        step (ExprBool b) = ResultBool b
        step (ExprOp op e1 e2) =
            let ResultInt e1' = step e1
                ResultInt e2' = step e2
            in case op of
                Add -> ResultInt (e1' + e2')
                Subtract -> ResultInt (e1' - e2')
        step (ExprIsZero e) =
            let ResultInt e' = step e
             in ResultBool (e' == 0)
        step (ExprIf cond ifcond elsecond) =
            case step cond of
                ResultBool True ->
                    step ifcond
                ResultBool False ->
                    step elsecond
        step (ExprVar vname) =
            case Map.lookup vname env of
                Just (VarInt i) -> ResultInt i
                Just (VarBool b) -> ResultBool b
                _ -> error "This shouldn't happen"

test_eval :: String
test_eval =
    case map fst $ filter (not . passes . snd) (zip [0..] tests) of
        [] -> "All tests passed"
        x -> "Failed tests: " ++ show x
    where
        passes (expr, expected) =
            eval env expr == expected
        env = Map.fromList [("x", VarInt 3), ("y", VarBool True)]
        tests = {-test 0-} [(ExprInt 0, ResultInt 0),
                {-test 1-} (ExprOp Add (ExprInt 0) (ExprBool False), ResultError),
                {-test 2-} (ExprOp Add (ExprInt 2) (ExprInt 1), ResultInt 3),
                {-test 3-} (ExprIsZero (ExprInt 0), ResultBool True),
                {-test 4-} (ExprIf (ExprIsZero (ExprInt 0)) (ExprOp Subtract (ExprInt 6) (ExprInt 1)) (ExprInt 9), ResultInt 5),
                {-test 5-} (ExprIf (ExprIsZero (ExprVar "x")) (ExprBool False) (ExprVar "y"), ResultBool True),  
                {-test 6-} (ExprIf (ExprIsZero (ExprVar "q")) (ExprOp Subtract (ExprInt 6) (ExprInt 1)) (ExprVar "y"), ResultError)]  

-- |Represents the total output of a program, produced by StatementPrint
type Output = [String]

-- |An error message. At the moment, we only have one possible kind of error
-- but we might add more later.
data Error = GeneralError

-- |The state of the interpreter: a possible error, the symbol environment, and the output
data InterpreterStatus = InterpreterStatus (Maybe Error) Environment Output

-- |Execute the given program, with the given environment, and produce an output
exec :: Environment -> Program -> Output
exec initial_env prog =
    case foldl step (InterpreterStatus Nothing initial_env []) prog of
        InterpreterStatus (Just errormsg) _ _ -> ["error"]
        InterpreterStatus _ _ output -> output        
    where
        step :: InterpreterStatus -> Statement -> InterpreterStatus
        step (InterpreterStatus (Just err) env out) _ = (InterpreterStatus (Just err) env out)
        
         step (InterpreterStatus err env out) (StatementAssign vname expr) =
            case eval env expr of
                ResultInt int -> InterpreterStatus err (Map.insert vname (VarInt int) env) out
                ResultBool bool -> InterpreterStatus err (Map.insert vname (VarBool bool) env)
                _ -> InterpreterStatus (Just GeneralError) env out

        step (InterpreterStatus err env out) (StatementPrint expr) =
            case eval env expr of
                ResultInt int -> InterpreterStatus err env (out ++ [show int])
                ResultBool bool -> InterpreterStatus err env (out ++ [show bool])
                _ -> InterpreterStatus (Just GeneralError) env out

        step (InterpreterStatus err env out) (StatementIf condition ifpart elsepart) =
            case eval env condition of
                ResultBool True -> step2 (InterpreterStatus err env out) ifpart
                ResultBool False -> step2 (InterpreterStatus err env out) elsepart
                _ -> InterpreterStatus (Just GeneralError) env out
            where step2 :: InterpreterStatus -> [Statement] -> InterpreterStatus
                  step2 status [] = status
                  step2 status (h:t) = case step status h of
                      (InterpreterStatus (Just err) env out) -> (InterpreterStatus (Just err)
                      status2 -> step2 status2 t

-- |Call this function to test your exec function
test_exec :: String
test_exec =
    case filter (passes . snd) (zip [0..] tests) of
        [] -> "All tests passed"
        x -> "Failed tests: " ++ concatMap showResult x
    where
        env = Map.fromList [("x", VarInt 3), ("y", VarBool True)]
        showResult (num, (expr, expect)) =
            "in test: "++show num++", got " ++ showEach (exec env expr) ++ " but expected " ++ showEach expect++"; "
        passes (expr, expected) =
            exec env expr /= expected
        showEach = (++"]") . ("["++) . intercalate ","
        tests = {-test 0-} [(program0, ["12"]),
                {-test 1-}  (program1, ["error"]),
                {-test 2-}  (program2, ["True", "11"]),
                {-test 3-}  (program3, ["False"]),
                {-test 4-}  (program4, ["50","21","False"]),
                {-test 5-}  (program5, ["error"])]



program0 = [StatementPrint (ExprOp Add (ExprInt 3) (ExprInt 9))]
program1 = [StatementAssign "x" (ExprOp Add (ExprInt 3) (ExprInt 9)),
            StatementAssign "z" (ExprIsZero (ExprVar "x")),
            StatementIf (ExprVar "z") [(StatementPrint (ExprInt 1))] [(StatementAssign "z" (ExprOp Subtract (ExprVar "z") (ExprInt 1)))]]
program2 = [StatementPrint (ExprVar "y"),
            StatementAssign "x" (ExprOp Add (ExprInt 3) (ExprInt 9)),
            StatementAssign "z" (ExprIsZero (ExprVar "x")),
            StatementIf (ExprVar "z") [(StatementPrint (ExprInt 1))] [(StatementAssign "x" (ExprOp Subtract (ExprVar "x") (ExprInt 1)))],
            StatementPrint (ExprVar "x")]
program3 = [StatementIf (ExprIsZero (ExprVar "x")) 
                [(StatementPrint (ExprBool True))]
                [(StatementPrint (ExprBool False))]]
program4 = [StatementAssign "foo" (ExprOp Add (ExprInt 1) (ExprInt 2)),
            StatementIf (ExprIsZero (ExprOp Subtract (ExprVar "foo") (ExprInt 3))) 
                [StatementPrint (ExprInt 50),
                StatementAssign "foo" (ExprOp Add (ExprVar "foo") (ExprInt 1))] 
                [StatementPrint (ExprInt 99),
                StatementAssign "foo" (ExprOp Subtract (ExprVar "foo") (ExprInt 1))],
            StatementIf (ExprIsZero (ExprOp Subtract (ExprVar "foo") (ExprInt 2))) 
                [StatementPrint (ExprInt 44), StatementPrint (ExprBool True),
                StatementAssign "foo" (ExprOp Add (ExprVar "foo") (ExprInt 3))] 
                [StatementPrint (ExprInt 21), StatementPrint (ExprBool False),  
                StatementAssign "foo" (ExprOp Subtract (ExprVar "foo") (ExprInt 4))]
                ]
program5 = [StatementPrint (ExprInt 22),
            StatementPrint (ExprVar "whatever"),
            StatementIf (ExprBool True) [StatementPrint (ExprInt 44)] []]
