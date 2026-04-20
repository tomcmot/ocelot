{-# LANGUAGE TypeFamilies #-}
module Token 
( LexToken (..)
, WithPos(..)
, Parser
, withPos
)
where

import Text.Megaparsec
import Data.Data
import Data.List qualified as DL
import Data.List.NonEmpty qualified as NE
import Data.List.NonEmpty (NonEmpty (..))
import Data.Void (Void)

data LexToken
    = Op String
    | Var String
    | Int Int
    | Let
    | LBracket | RBracket | LBrace | RBrace | LParen | RParen
    | Comma | Semicolon
    | WOpen | WSep | WClose
    deriving (Eq, Ord, Show)

data WithPos a = WithPos
  { startPos :: SourcePos
  , endPos :: SourcePos
  , tokenLength :: Int
  , tokenVal :: a
  } deriving (Eq, Ord, Show)

data TokenStream
    = TokenStream
        { streamInput :: String
        , unStream :: [WithPos LexToken]
        }

instance Stream TokenStream where
    type Token TokenStream = WithPos LexToken
    type Tokens TokenStream = [WithPos LexToken]

    tokenToChunk Proxy x = [x]
    tokensToChunk Proxy xs = xs
    chunkToTokens Proxy = id
    chunkLength Proxy = length
    chunkEmpty Proxy = null
    take1_ (TokenStream _ []) = Nothing
    take1_ (TokenStream str (t:ts)) = Just
        ( t
        , TokenStream (drop (tokensLength pxy (t:|[])) str) ts
        )
    takeN_ n (TokenStream str s)
        | n <= 0    = Just ([], TokenStream str s)
        | null s    = Nothing
        | otherwise =
            let (x, s') = splitAt n s
            in case NE.nonEmpty x of
            Nothing -> Just (x, TokenStream str s')
            Just nex -> Just (x, TokenStream (drop (tokensLength pxy nex) str) s')
    takeWhile_ f (TokenStream str s) =
        let (x, s') = DL.span f s
        in case NE.nonEmpty x of
        Nothing -> (x, TokenStream str s')
        Just nex -> (x, TokenStream (drop (tokensLength pxy nex) str) s')

instance VisualStream TokenStream where
  showTokens Proxy = unwords
    . NE.toList
    . fmap (show . tokenVal)
  tokensLength Proxy xs = sum (tokenLength <$> xs)

instance TraversableStream TokenStream where
  reachOffset o PosState {..} =
    ( Just (prefix ++ restOfLine)
    , PosState
        { pstateInput = TokenStream
            { streamInput = postStr
            , unStream = post
            }
        , pstateOffset = max pstateOffset o
        , pstateSourcePos = newSourcePos
        , pstateTabWidth = pstateTabWidth
        , pstateLinePrefix = prefix
        }
    )
    where
      prefix =
        if sameLine
          then pstateLinePrefix ++ preLine
          else preLine
      sameLine = sourceLine newSourcePos == sourceLine pstateSourcePos
      newSourcePos =
        case post of
          [] -> case unStream pstateInput of
            [] -> pstateSourcePos
            xs -> endPos (last xs)
          (x:_) -> startPos x
      (pre, post) = splitAt (o - pstateOffset) (unStream pstateInput)
      (preStr, postStr) = splitAt tokensConsumed (streamInput pstateInput)
      preLine = reverse . takeWhile (/= '\n') . reverse $ preStr
      tokensConsumed =
        case NE.nonEmpty pre of
          Nothing -> 0
          Just nePre -> tokensLength pxy nePre
      restOfLine = takeWhile (/= '\n') postStr

pxy :: Proxy TokenStream
pxy = Proxy

type Parser = Parsec Void TokenStream

withPos :: (TraversableStream s) => Parsec Void s a -> Parsec Void s (WithPos a)
withPos p = do
    s <- getSourcePos
    i <- getOffset
    v <- p
    f <- getSourcePos
    e <- getOffset
    pure (WithPos s f (e - i) v)