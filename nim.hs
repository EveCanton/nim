data Player = A | B deriving (Eq, Show)

other :: Player -> Player
other A = B
other B = A

type Pile = Int
type Game = [Pile]

isGameOver :: Game -> Bool
isGameOver piles = all (== 0) piles


allNextMoves :: Player -> Game -> [Game]
allNextMoves _ piles =
  [ replacePile i newVal piles
  | (i, val) <- zip [0..] piles
  , newVal <- [0..val-1]
  ]
  where
    replacePile i newVal xs =
      take i xs ++ [newVal] ++ drop (i+1) xs


solveGameFor :: Player -> Game -> Player
solveGameFor me game
  | isGameOver game = other me
  | otherwise =
      let boardsAfterIPlayed = allNextMoves me game
          opponent = other me
          endgames = map (solveGameFor opponent) boardsAfterIPlayed
          iCanWin  = me `elem` endgames
      in if iCanWin then me else opponent


main :: IO ()
main = do
  putStrLn "Configuración inicial de Nim (ej: 3 4 5):"
  input <- getLine
  let game = map read (words input) :: [Int]
  let winner = solveGameFor A game
  putStrLn $ "El ganador (jugando perfectamente) será: " ++ show winner
