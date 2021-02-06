module Reader
where
    import Control.Monad.Reader

-------------------------THE READER MONAD----------------------------

-- What is the Reader monad?

-- it's some abstract type
-- data Reader env a

--such that Reader is a monad
-- instance Monad (Reader env)

--we have a function to get its environment
-- ask :: Reader env env

-- --and we can run a Reader
-- runReader :: Reader env a -> env -> a

data GameState = NotOver | FirstPlayerWin | SecondPlayerWin | Tie

data Game position = Game 
                        {
                            getNext :: position -> [position]
                            getState :: position -> GameState
                        }