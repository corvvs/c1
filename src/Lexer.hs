module Lexer (Token (..), lexEquation, tokenRange) where

import qualified Data.Text as T
import Exception
import Data.Char (isDigit, isSpace)
import MyPrint
type TokenRange = (Int, Int)

data Token
  = TokNum Double TokenRange -- 数値
  | TokIdent T.Text TokenRange -- 識別子
  | TokEqual TokenRange -- '='
  | TokPlus TokenRange -- '+'
  | TokMinus TokenRange -- '-'
  | TokMul TokenRange -- '*'
  | TokDiv TokenRange -- '*'
  | TokPow TokenRange -- '^'
  | TokLParen TokenRange -- '('
  | TokRParen TokenRange -- ')'
  deriving (Show, Eq)

data Context = Context
  { expression :: T.Text,
    idx :: Int
  }
  deriving (Show)

nc :: Context -> Int -> Context
nc ctx n = ctx {idx = idx ctx + n}

sayError :: Context -> String -> ExceptTT a
sayError ctx msg =
  throwError $ T.concat [T.pack "TokenizeError: ", T.pack msg, T.pack "\n", emphasized]
  where
    i = idx ctx
    emphasized = MyPrint.emphasis (expression ctx) (i, i + 1)

tokenRange :: Token -> TokenRange
tokenRange (TokNum _ r) = r
tokenRange (TokIdent _ r) = r
tokenRange (TokEqual r) = r
tokenRange (TokPlus r) = r
tokenRange (TokMinus r) = r
tokenRange (TokMul r) = r
tokenRange (TokDiv r) = r
tokenRange (TokPow r) = r
tokenRange (TokLParen r) = r
tokenRange (TokRParen r) = r

lexEquation :: T.Text -> ExceptTT [Token]
lexEquation cs = do
  (_, tokens) <- lex_ (Context {expression = cs, idx = 0}) cs
  return tokens

-- lexer 本体
lex_ :: Context -> T.Text -> ExceptTT (Context, [Token])
lex_ ctx givenInput = case T.uncons givenInput of
  Nothing -> return (ctx, []) -- 入力文字列が空だったら, 空のトークン列を返す
  Just (headChar, restChars) -> lex__ ctx (headChar, restChars, givenInput) -- 入力文字列が空でなければ字句解析を行う
    where
      i = idx ctx

      isSymbol :: Char -> Bool
      isSymbol c' = c' `elem` ['=', '+', '-', '*', '/', '^', '(', ')', '.', '%', '&', '@']

      isIdentifierChar :: Char -> Bool
      isIdentifierChar c' = not (isSpace c' || isDigit c' || isSymbol c')

      -- 数字を解析するヘルパー関数
      lexNum :: Context -> T.Text -> ExceptTT (Context, [Token])
      lexNum ctx_ restChars_ = do
        let (prefixNumText, rest) = T.span (\c' -> isDigit c' || c' == '.') restChars_
        -- NOTE: prefixNumText が空でないことが呼び出し元で保証される
        let prefixNumStr = T.unpack prefixNumText
        
        case findIndexOf2nd prefixNumStr '.' of
          Just i_ ->
            sayError (nc ctx_ i_) $ "Unexpected character: " ++ [prefixNumStr !! i_]
          Nothing -> do
            case last prefixNumStr of
              dot@'.' ->
                sayError (nc ctx_ (T.length prefixNumText - 1)) $ "Unexpected character: " ++ [dot]
              _ -> do
                -- . が存在する場合は高々1つ, かつ位置が先頭/末尾以外であればOK
                let ctx' = nc ctx_ (T.length prefixNumText)
                (ctx'', tokens) <- lex_ ctx' rest
                return (ctx'', TokNum (read prefixNumStr) (idx ctx_, idx ctx') : tokens)
        where
          findIndexOf2nd :: String -> Char -> Maybe Int
          findIndexOf2nd str c' = do
            let idxs = filter (\i_ -> str !! i_ == c') [0 .. length str - 1]
            if length idxs >= 2
              then Just $ idxs !! 1
              else Nothing

      -- 識別子を解析するヘルパー関数
      lexIdentifier :: Context -> T.Text -> ExceptTT (Context, [Token])
      lexIdentifier ctx_ restChars_ = do
        let (identStr, rest) = T.span isIdentifierChar restChars_
        let ctx' = nc ctx_ (T.length identStr)
        (ctx'', tokens) <- lex_ ctx' rest
        return (ctx'', TokIdent identStr (idx ctx_, idx ctx') : tokens)

      lex__ :: Context -> (Char, T.Text, T.Text) -> ExceptTT (Context, [Token])
      lex__ ctx_ (headChar_, restChars_, given_input_)
        | isSpace headChar_ = lex_ (nc ctx_ 1) restChars_ -- 空白は無視
        | isIdentifierChar headChar_ = lexIdentifier ctx_ given_input_ -- 識別子のトークン化
        | isDigit headChar_ = lexNum ctx_ given_input_ -- 数値のトークン化
        | headChar_ == '=' = addOpToken TokEqual -- 各種演算子のトークン化
        | headChar_ == '+' = addOpToken TokPlus
        | headChar_ == '-' = addOpToken TokMinus
        | headChar_ == '*' = addOpToken TokMul
        | headChar_ == '/' = addOpToken TokDiv
        | headChar_ == '^' = addOpToken TokPow
        | headChar_ == '(' = addOpToken TokLParen
        | headChar_ == ')' = addOpToken TokRParen
        | otherwise = sayError ctx_ $ "Unexpected character: " ++ [headChar_]
        where
          addOpToken tokCon = do
            (ctx', tokens) <- lex_ (nc ctx_ 1) restChars_
            return (ctx', tokCon (i, i + 1) : tokens)
