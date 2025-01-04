module Exception(ExceptTT, throwError) where

import Control.Monad.Except
import qualified Data.Text as T

-- 失敗時はエラーメッセージ(T.Text)を返し,
-- 成功時は成果物(IO a)を返すようなExceptTモナドを定義
type ExceptTT a = ExceptT T.Text IO a
