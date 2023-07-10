{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where


import Data.Aeson
import Data.ByteString.Lazy             ( readFile, ByteString )

import Options.Applicative

import System.Random                    ( uniformR, initStdGen, mkStdGen, RandomGen )
import System.Directory                 ( doesFileExist, getPermissions, Permissions( readable ) )

import Control.Concurrent.Thread.Delay  ( delay )
import Control.Monad                    ( replicateM, unless )
import Control.Monad.IO.Class           ( MonadIO )

import Data.List                        ( unfoldr )

import Prelude hiding                   ( readFile )
import GHC.Generics (Generic)


main :: IO ()
main = argHandler =<< execParser opts
    where
    opts = info (argPars <**> helper)
        ( fullDesc
        <> progDesc "Conway's Game of Life in the terminal"
        )

argHandler :: Args -> IO ()
argHandler (Args 0 _ _ _ _)         = return () -- because... the board is not even there
argHandler (Args _ 0 _ _ _)         = return () -- ^
argHandler (Args y x c s "none")    = standard (Args y x c s "none")
argHandler (Args y x c s p)         = fromFile (Args y x c s p)

standard :: Args -> IO ()
standard args = do
    board <- generateRandomBoard (sizeY args, sizeX args) (fromIntegral (chance args) / 100)
    putStr "\x1b[2J"
    runCycle board (stepDelay args)


fromFile :: Args -> IO ()
fromFile args = do
    -- checking if file exists + if we have read permissions

    fileExists <- doesFileExist (file args)
    -- file doesnt exist
    unless fileExists $ putStrLn "[error] file doesnt exist"

    perm <- getPermissions (file args)
    -- do not have read permissions
    unless (readable perm) $ putStrLn "[error] cant read file (permissions)"

    contents <- readFile (file args)
    let mBoard = decode contents
    case mBoard of
        Nothing     -> putStrLn "[error] could not parse file (incorrect format?)"
        Just board  -> runCycle board (stepDelay args)

printBoard :: Board -> IO ()
printBoard = mapM_ (\(cord, cell) -> putStrLn $ point cord <> show cell)
    where point :: Cord -> String
          point (Cord y x) = "\x1b[" <> show (y + 1) <> ";" <> show (x * 2 + 1) <> "H"

runCycle :: Board -> Integer -> IO ()
runCycle board stDelay = mapM_ (\board -> do
        printBoard board
        delay $ stDelay * 10^5
        ) $ iterate stepCycle board

stepCycle :: Board -> Board
stepCycle board = map (\(cord, cell) -> do
    let n = neighbours cord board
    if n == 3 then
        (cord, Cell True)
    else if n < 2 || n > 3 then
        (cord, Cell False)
    else
        (cord, cell)
    ) board

generateEmptyBoard :: (Integer, Integer) -> Board
generateEmptyBoard (y, x) = [(Cord y x, Cell False) | y <- [0..y-1], x <- [0..x-1]]

-- dimensions (y, x), value between 0 and 1 saying how often an alive cell should be placed
generateRandomBoard :: MonadIO m => (Integer, Integer) -> Double -> m Board
generateRandomBoard (y, x) chance = do
    r <- initStdGen

    let cords = [Cord yc xc | yc <- [0..y-1], xc <- [0..x-1]]
    let cells = map (\re -> Cell (re < chance)) $ rolls (y*x) (0.0, 1.0) r

    return (zip cords cells)

-- get `n` random numbers between range `r` (a tuple of 2 Doubles, low <-> high)
rolls :: RandomGen g => Integer -> (Double, Double) -> g -> [Double]
rolls n r = take (fromInteger n) . unfoldr (Just . uniformR r)

-- get number of alive neighbours
-- basically filters the board to cells which (y, x) distance is equal to 1, so a ring around the given cords
-- thats why i decided to use a coordinate grid instead of two dimensional array, the math is way prettier 
neighbours :: Cord -> Board -> Int  
neighbours (Cord y1 x1) = length . filter (\(Cord y2 x2, Cell alive) -> max (diff x1 x2) (diff y1 y2) == 1 && alive)
    where diff :: Integer -> Integer -> Integer
          diff x y = abs (x - y)


data Cord = Cord {y :: Integer, x :: Integer}
    deriving ( Show )

instance FromJSON Cord where
    parseJSON (Object v) =
        Cord <$> v .: "y"
             <*> v .: "x"

newtype Cell = Cell Bool
    deriving ( Generic )

instance Show Cell where
    show (Cell False) = "  "
    show (Cell True)  = "██"

instance FromJSON Cell


-- this one is basically just for prettier type definitions
type Board = [(Cord, Cell)]

-- and this one is for aeson (JSON)
data FileBoard = FileBoard
    { fileY         :: Integer
    , fileX         :: Integer
    , fileBoard     :: Board
    } deriving ( Show )

instance FromJSON FileBoard where
    parseJSON (Object v) =
        FileBoard <$> v .: "size-y"
                  <*> v .: "size-x"
                  <*> v .: "board"


-- command line arguments
data Args = Args
    { sizeY         :: Integer
    , sizeX         :: Integer
    , chance        :: Integer
    , stepDelay     :: Integer
    , file          :: String
    }

-- options for those arguments
argPars :: Parser Args
argPars = Args
    <$> option auto
        ( long "sizeY"
       <> short 'y'
       <> help "size of the board in Y dimension"
       <> value 25
       <> metavar "INTEGER" 
        )
    <*> option auto
        ( long "sizeX"
       <> short 'x'
       <> help "size of the board in X dimension"
       <> value 25
       <> metavar "INTEGER"
        )
    <*> option auto
        ( long "chance"
       <> short 'c'
       <> help "how often to randomly place a cell 0-100"
       <> value 50
       <> metavar "INTEGER"
        )
    <*> option auto
        ( long "stepDelay"
       <> short 's'
       <> help "delay bewteen steps in 1/10 of a second"
       <> value 2
       <> metavar "INTEGER"
        )
    <*> option auto
        ( long "file"
       <> short 'f'
       <> help "path to file containing a board"
       <> value "none"
       <> metavar "STRING"
        )
