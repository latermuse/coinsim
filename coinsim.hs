    --On Monday, you flip a coin all day. You start flipping it until you see the pattern Head, Tail, Head. You record the number of flips required to reach this pattern, and start flipping again (and counting up from 1 again) until you see that pattern again, you record the second number, and start again. At the end of the day you average all of the numbers you’ve recorded. On Tuesday you do the EXACT same thing except you flip until you see the pattern Head, Tail, Tail.
    --
    --Will Monday’s number be higher than Tuesday’s, equal to Tuesday’s, or lower than Tuesday’s?
    --
    
    module Main where
    
    import System.Random
    
    -- 1 is head, 0 is tail
    
    -- amount of total flips in a day
    oneDay ::  Integer
    oneDay = 80000
    
    getCoinFlip :: IO Int
    getCoinFlip = randomRIO (0,1)
    
    results :: (Num a) => (a, a, a) -> a -> (a, a, a) 
    results (x,y,z) new = (y, z, new)
    
    flipCoin sheet = do
        coinFlip <- getCoinFlip
        return $ results sheet coinFlip
    
    main = do
        countSim 1 [] mondayCombo >>= (\x -> return $ average (toInteger . length $ x) (sum x)) >>= (\y -> putStrLn $ "Monday's average is: " ++ show y)
        countSim 1 [] tuesdayCombo >>= (\x -> return $ average (toInteger . length $ x) (sum x)) >>= (\y -> putStrLn $ "Tuesdays's average is: " ++ show y)
        
    countSim i list' winningCombo =
        if i > oneDay 
            then return list'
            else do
                simResult <- playSim 0 (oneDay - i) blankSheet winningCombo
                if simResult == (-1)
                    then return list'
                    else countSim (i + 1 + simResult) (simResult : list') winningCombo
            
    playSim flips max sheet winningCombo = do
        newSheet <- flipCoin sheet
        if flips > max
            then return (-1)
            else if newSheet == winningCombo
                then return flips
                else playSim (flips + 1) max newSheet winningCombo
    
    average :: Integer -> Integer -> Integer
    average entries sum = sum `div` entries
    
    mondayCombo = (1,0,1)
    tuesdayCombo = (1,0,0)
    blankSheet = (5,5,5)
