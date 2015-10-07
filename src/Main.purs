module Main where

import           Control.Monad.Eff
import           Data.Array (replicate, (!!), updateAt)
import           Data.Maybe (Maybe(..))
import           Data.Maybe.Unsafe (fromJust)
import           Data.Nullable (toMaybe)
import           Prelude

import           DOM (DOM())
import           DOM.HTML (window)
import           DOM.HTML.Document (body)
import           DOM.HTML.Types (htmlElementToElement)
import           DOM.HTML.Window (document)
import           DOM.Node.Types (Element())

import           React
import qualified React.DOM as D
import qualified React.DOM.Props as P

import qualified Signal.Channel as C
import qualified Signal as S

data Token = X | O | E

instance showToken :: Show Token where
  show X = "X"
  show O = "O"
  show E = ""

classForToken X = "cell x"
classForToken O = "cell o"
classForToken E = "cell"

type Board = Array Token

get :: Int -> Int -> Board -> Token
get x y board = fromJust (board !! (3 * x + y))

set :: Int -> Int -> Token -> Board -> Board
set x y token board = fromJust (updateAt (3 * x + y) token board)

newGameState = {currentPlayer: X, board: replicate 9 E}

boardComponent :: Environment -> ReactClass Unit
boardComponent env = createClass $ spec unit \_ -> return (game env)

newGameButton c =
  D.button [P.onClick (\_ -> C.send c NewGame)] [D.text "New Game"]

game :: Environment -> ReactElement
game env = D.div'
  [newGameButton env.channel
  , grid env
  , D.text (show (env.currentPlayer) ++ "'s turn.")]

grid :: Environment -> ReactElement
grid env = D.table' (map (row env) [0,1,2])

row :: Environment -> Int -> ReactElement
row env x = D.tr' (map (cell env x) [0,1,2])

cell :: Environment -> Int -> Int -> ReactElement
cell env x y = D.td
  [P.className (classForToken token)
  , P.onClick (\_ -> C.send env.channel (Click x y))]
  [D.text (show token)]
  where
    token = get x y env.board

-----------------------------------------------------------
type State a = { board :: Board, currentPlayer :: Token | a }
type GameState = State ()
type Environment = State (channel :: C.Channel Action)

mkEnv :: C.Channel Action -> GameState -> Environment
mkEnv channel gameState = {
  board: gameState.board,
  currentPlayer: gameState.currentPlayer,
  channel: channel
}

data Action = NewGame | Click Int Int

nextPlayer :: Token -> Token
nextPlayer X = O
nextPlayer O = X
nextPlayer E = E

step :: Action -> GameState -> GameState
step NewGame _ = newGameState
step (Click x y) gameState =
  case get x y gameState.board of
    E -> gameState {
      board = set x y gameState.currentPlayer gameState.board,
      currentPlayer = nextPlayer gameState.currentPlayer
    }
    _ -> gameState

-----------------------------------------------------------

main = do
  body' <- getBody
  channel <- C.channel NewGame
  let actions = C.subscribe channel

  let gameState = S.foldp step newGameState actions

  let game = gameState S.~>
    mkEnv channel >>> (\env -> render (ui env) body') >>> void

  S.runSignal game

  where
    ui env = D.div' [ createFactory (boardComponent env) unit ]

    getBody = do
      win <- window
      doc <- document win
      elm <- fromJust <$> toMaybe <$> body doc
      return $ htmlElementToElement elm
