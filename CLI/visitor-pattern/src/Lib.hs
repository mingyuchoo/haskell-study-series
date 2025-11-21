module Lib
    where

import           Data.Kind (Type)

-- $ ./Visitor
-- 1 + (2 + 3)^2
-- 26
--
type Expr :: Type -> Type
data Expr a = Plus (Expr a) (Expr a) -- 덧셈 식
            | Square (Expr a) -- 제곱 식
            | Number a

-- | 식을 평가하는 함수
--
evalExpr :: Expr Int -> Int
evalExpr (Plus e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (Square e)   = evalExpr e ^ (2 :: Int)
evalExpr (Number n)   = n

-- | 식을 문자열로 변환하는 함수
--
showExpr :: Expr Int -> String
showExpr (Plus e1 e2) = showExpr e1 ++ " + " ++ showExpr e2
showExpr (Square e)   = "(" ++ showExpr e ++ ")^2"
showExpr (Number n)   = show n

-- |
--
someFunc :: IO ()
someFunc = do
    -- e = 1 + (2 + 3)^2
    -- 실제로는 구문 분석 등에 더 크고 복잡한 것을 가정
    let e = Plus (Number 1) (Square (Plus (Number 2) (Number 3)))
    putStrLn (showExpr e)
    print (evalExpr e)

