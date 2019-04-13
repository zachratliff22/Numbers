import Data.Bits
import System.Random
import Math.NumberTheory.Powers.Modular
import System.IO
import System.Environment

-- This function outputs the largest power of 2 that is a multiple of a given integer
-- Example: getLargestPowerTwo 48 = 16
getLargestPowerTwo k = (.&.) k (negate (k - 1))

-- Returns a random integer
randomInt k = randomRIO(2, k - 1)

checkCompositeness r x n | r > 0 = if (powMod x 2 n) == (n - 1) then False else checkCompositeness (r - 1) (powMod x 2 n) n
                         | otherwise = True

witnessLoop z r d n | z > 0 = do
                        g <- randomInt n
                        let x = powMod g d n
                        if x == 1 || x == (n - 1) 
                            then witnessLoop (z - 1) r d n 
                            else do 
                                let check = checkCompositeness r x n
                                if check
                                    then putStrLn $ "composite"
                                        else witnessLoop (z - 1) r d n
                    | otherwise = putStrLn $ "probably prime with probability 99.9999%"


millerRabin n | n <= 1 = do
                        putStrLn "Pick an integer greater than 1"
              | n == 2 = do
                        putStrLn "2 is prime"
              | otherwise = do
                    let p = getLargestPowerTwo (n - 1)
                    let d = div (n - 1) p
                    let r = floor (logBase 2 (fromIntegral p))
                    witnessLoop 10 r d n
                    

printUsage = do
    print "USAGE: ./millerRabin [number]"
    print "Example: ./millerRabin 1351325153123532153143141234125393851519823334135151"
    
main :: IO()
main = do
        args <- getArgs
        if (length args) /= 1
            then do
                printUsage
        else if null args
            then printUsage
        else do    
            let num = read (args !! 0) :: Integer
            millerRabin num
