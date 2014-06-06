module ElementryAutomaton where

import System.Cmd
import Data.List
import Control.Monad
import System.IO
import System.Environment
import qualified Data.Sequence
import Data.Sequence ((|>),(><))
import qualified Data.Foldable

initState = [True]

rotateList' n xs  = Data.Foldable.toList $ rotateList n $ Data.Sequence.fromList xs

rotateList n ss | Data.Sequence.null ss = Data.Sequence.empty
                | otherwise = tail >< head
  where (head,tail) = Data.Sequence.splitAt n ss
    
rotateList'' _ [] = []
rotateList'' 0 xs = xs
rotateList'' n xs =  t ++ h
    where (h,t) = splitAt n xs

hanging_list hang_size list = end ++ map Just list ++ end
  where end = replicate hang_size Nothing

hanging_list' total_size list = hanging_list ((total_size - length list) `div` 2) list 
  
maybeToBool state = map helper state
  where helper (Just x) = x
        helper _ = False        
        
windowify window_size list | length list < window_size = []
                           | otherwise = take window_size list : windowify window_size (tail list)


windowMap' window_size f list = map f $ windowify' window_size list
windowify' window_size list = map Data.Foldable.toList $ fmap (Data.Sequence.take window_size) $ (\x -> last x : init x) $ take (length list) $ iterate (rotateList 1) (Data.Sequence.fromList list)
eg' rule = iterate (windowMap' 3 rule) (maybeToBool $ hanging_list' 600 initState)

--rendering

windowMap window_size f list = map f $ windowify window_size list
eg rule = mapM_  (\x -> do { putStrLn (renderIterations''' x)} ) (iterate (windowMap' 3 rule) (maybeToBool $ hanging_list 64 [True]))

renderIterations num iterations = renderIterations' $ map (\x -> hanging_list' num x) iterations
 
renderIterations' = renderIterations'' . map maybeToBool
renderIterations'' list = map renderIterations''' list
renderIterations''' list = (map convertChar) list
  where convertChar True = '#'
        convertChar False = '.'

-- commands

stateHelper'''' f state = windowMap 3 f $ stateHelper''' state
    where stateHelper''' state = maybeToBool $ hanging_list 2 state

renderRule iterations rule = do
  mapM_ putStrLn (renderIterations iterations $ take iterations $ iterateRule rule initState)
  where iterateRule rule state = iterate (stateHelper'''' rule) state        
    
            
main = do 
  renderRule 10 rule30
  putStrLn ""
  renderRule 10 rule90
  putStrLn ""
  renderRule 30 rule110       
       
-- rules
rule30 [True ,True ,True] = False
rule30 [True ,True ,False] = False
rule30 [True ,False,True] = False
rule30 [True ,False,False] = True
rule30 [False,True ,True] = True
rule30 [False,True ,False] = True
rule30 [False,False,True] = True
rule30 [False,False,False] = False

rule90 [True ,True ,True] = False
rule90 [True ,True ,False] = True
rule90 [True ,False,True] = False
rule90 [True ,False,False] = True
rule90 [False,True ,True] = True
rule90 [False,True ,False] = False
rule90 [False,False,True] = True
rule90 [False,False,False] = False


rule110 [True,True,True] = False
rule110 [_,False,False] = False     
rule110 _ = True
{-
rule110 [True ,True ,True] = False
rule110 [True ,False,False] = False
rule110 [False,False,False] = False       

rule110 [False,True ,True] = True
rule110 [True ,True ,False] = True
rule110 [False,True ,False] = True
rule110 [False,False,True] = True
rule110 [True ,False,True] = True
-}

-- use rule30 as encrytion
encrypt :: [Bool] -> [Bool]
encrypt y = iterate (\x -> stateHelper'''' rule30 x) y !! 3

decrypt :: [Bool] -> [Bool]
decrypt y = (\(Just x)->x) $ find (\x -> encrypt x == y) $ concatMap (\x -> replicateM x [True,False]) [1..]

--other
rolling_update f xs  = init' ++ (last:[f last])
 where (last:init) = reverse (tail xs)
       init' = reverse init