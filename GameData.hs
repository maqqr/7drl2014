module GameData where


data Game = Game

newGame :: IO Game
newGame = return Game
