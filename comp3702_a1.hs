import System.IO
import System.Environment
import System.IO.Error
import Data.List
import Control.Monad
import Numeric
import System.Random
import Data.Fixed

--Main do loop to run the program; extracts arguments, processes inputs and sends to map evaluation
--functions.
main = do
        (args:xs) <- getArgs
        handle <- openFile args ReadMode
        nASV <- hGetLine handle
        xs <- hGetLine handle
        xy <- hGetLine handle
        xz <- hGetContents handle
        let startXY = makeConfig $ words xs
            endXY = makeConfig $ words xy
            obstacles = processMap xz
            nASV' = read nASV :: Int
        roadMap startXY endXY obstacles

--PRIMARY DO STUFF FUNCTIONS.
--Function to sample a set of points on the input map, process paths from start to finish in regards to
--obstacle positions then initiates a UCS on the paths.
roadMap :: [(Float, Float)] -> [(Float, Float)] -> [[(Float, Float)]] -> IO ()
roadMap start goal obstacles = do
                        {-Start by randomly generating an amount of points on the map = 20*number os ASVs-}
                        let seedn = (length start * length obstacles)
                            points = take (seedn*50) $ randomRs (1, 100) (mkStdGen (seedn)) :: [Integer]
                            points' = map ((/100) . fromInteger) points
                            points'' = randomPairs points'

                            {-Filter points that would be in the forbidden zone-}
                            legalPoints = filter (checkCollision obstacles) points''

                            {-Begin processing paths, first check that the path to a point
                             is possible-}
                            failBus = paveRoads (head start : legalPoints ++ [head goal]) (legalPoints ++ [head goal]) obstacles
                            failBus'= group $ sort failBus
                            noRepeat = map head failBus'
--                        print start
--                        print goal
--                        print failBus
--                        mapM_ print noRepeat
                        uCS noRepeat [] [start !! 0] (head goal)

--Forms paths from the sampled points based on obstacle locations and allowable proximity.
paveRoads :: [(Float, Float)] -> [(Float, Float)] -> [[(Float, Float)]] -> [((Float, Float), (Float, Float), Float)]
paveRoads points1 points2 obstacles
                | length points1 == 2 = [z]
                | x == True = z : paveRoads points' points2' obstacles
                | otherwise = paveRoads points' points2' obstacles
                    where x = checkPath a b obstacles
                          a = head points1
                          points' = tail points1
                          points2' = filter (/=a) points2
                          b = nodeSniffer a (filter (/=a) points2)
                          z = makePath a b

--This is a uniform cost search. Duh.
uCS :: [((Float, Float), (Float, Float), Float)] -> [((Float, Float), (Float, Float), Float)] -> [(Float, Float)] -> (Float, Float) -> IO()
uCS paths output start finish =
                if (elem finish start') == True
                    then do
                           let x = group . sort $ foldl breakNodes [] outputx
                               x'= map head $ cullNodes x
                               y = takeWhile (/= finish) $ tail x'
                               y' = filter (pathNotContain y) outputx
                               tCost = sum $ map pull3 y'
                           print y'
                           print tCost
                    else do
                         uCS newmap'' outputx start' finish
                        where   cheapest = shortPath $ filter (pathContains start) paths
                                adjacent = filter (pathContains [(backDoor start cheapest)]) paths
                                newCosts' = map (updateCost (pull3 cheapest)) newCosts
                                newCosts = filter (notPath [cheapest]) adjacent
                                newmap' = filter (notPath adjacent) newmap
                                newmap = filter (/= cheapest) paths
                                newmap'' = newmap' ++ newCosts'
                                outputx = cheapest : output
                                start' = (backDoor start cheapest) : start
                                startx = start' !! (length start' - 1)

--AUXILLARY FUNCTIONS TO HANDLE VARIOUS PROCESSING TASKS FOR ASV AND MAP DATA.
--Function to break contents of a file (assuming it has been
--extracted into a string using hGetContents).
stringBreaker :: (Char -> Bool) -> String -> [String]
stringBreaker p s = case dropWhile p s of
                    "" -> []
                    s' -> w : stringBreaker p s''
                        where (w, s'') = break p s'

--Function to form a list of pairs for the co-ordinates of the
--ASVs.
makeConfig :: [String] -> [(Float, Float)]
makeConfig input = case input of [] -> []
                                 xs:xy:xz' -> (x, y) : makeConfig xz'
                                    where x = read xs :: Float
                                          y = read xy :: Float

processMap :: String -> [[(Float, Float)]]
processMap input = map (makeConfig . words) (drop 1 out)
                        where out = stringBreaker (== '\n') input

isCollision :: (Float, Float) -> [(Float, Float)] -> Bool
isCollision posXY fZone
            | (( a < x) && ( x < b ) && (c < y) && (y < d)) = True
            | otherwise = False
                where x = fst posXY
                      y = snd posXY
                      a = fst (fZone !! 0) - 0.05
                      b = fst (fZone !! 1) + 0.05
                      c = snd (fZone !! 1) - 0.05
                      d = snd (fZone !! 2) + 0.05

checkCollision :: [[(Float, Float)]] -> (Float, Float) -> Bool
checkCollision obstacles node
                    | (notElem True x) = True
                    | otherwise = False
                        where x = map (isCollision node) obstacles

randomPairs :: [Float] -> [(Float, Float)]
randomPairs input = case input of [] -> []
                                  xs -> (x,y) : randomPairs xs'
                                    where x = head xs
                                          y = head (drop 1 xs)
                                          xs' = drop 2 xs

--Boolean check of whether the vector between two points passes through forbidden zone.
checkPath :: (Float, Float) -> (Float, Float) -> [[(Float, Float)]] -> Bool
checkPath pointA pointB obstacles
    | (length lineEq) == (length canPass) = True
    | otherwise = False
        where xlist = [fst pointA, (fst pointA + 0.05) .. fst pointB]
              ylist = [snd pointA, (snd pointA + 0.05) .. snd pointB]
              grad = (snd pointB - snd pointA)/(fst pointB - fst pointA)
              beta = (snd pointA) - grad*(fst pointA)
              lineEq = [ ((y - beta)/grad, grad*x + beta) | x <- xlist, y <- ylist]
              canPass = filter (checkCollision obstacles) lineEq

--Returns the closest node to a specific reference node, from an list of unexplored nodes.
nodeSniffer :: (Float, Float) -> [(Float, Float)] -> (Float, Float)
nodeSniffer start points
                   | length points == 1 = head points
                   | prox1 > prox2 = nodeSniffer start (drop 1 points)
                   | otherwise  = nodeSniffer start (head points : drop 2 points)
                        where prox1 = abs(fst start - fst (points !! 0)) + abs(snd start - snd (points !! 0))
                              prox2 = abs(fst start - fst (points !! 1)) + abs(snd start - snd (points !! 1))

--Returns the cost of travelling between two nodes.
getCost :: (Float, Float) -> (Float, Float) -> Float
getCost pointA pointB = sqrt((fst pointA - fst pointB)**2 + (snd pointA - snd pointB)**2)

--Function to asses paths between points, and return a list with costs in it.
makePath :: (Float, Float) -> (Float, Float) -> ((Float, Float), (Float, Float), Float)
makePath pointA pointB
                | fst pointA < fst pointB = (pointA, pointB, getCost pointA pointB)
                | otherwise = (pointB, pointA, getCost pointA pointB)

---Small expansion on the fst and snd functions to extract element
--three of a triple.
pull3 :: (a, b, c) -> c
pull3 (_,_,c) = c

--Small expansion on the fst and snd functions to extract element
--two of a triple.
pull2 :: (a, b, c) -> b
pull2 (_,b,_) = b

--Small expansion on the fst and snd functions to extract element
--one of a triple.
pull1 :: (a, b, c) -> a
pull1 (a,_,_) = a

pathContains :: [(Float, Float)] -> ((Float, Float), (Float, Float), Float) -> Bool
pathContains pointXY pathXYC
                | x == True = True
                | y == True = True
                | otherwise  = False
                    where x = elem (pull1 pathXYC) pointXY
                          y = elem (pull2 pathXYC) pointXY

pathNotContain :: [(Float, Float)] -> ((Float, Float), (Float, Float), Float) -> Bool
pathNotContain pointXY pathXYC
                | x == True = False
                | y == True = False
                | otherwise  = True
                    where x = elem (pull1 pathXYC) pointXY
                          y = elem (pull2 pathXYC) pointXY

updateCost :: Float -> ((Float, Float), (Float, Float), Float) -> ((Float, Float), (Float, Float), Float)
updateCost pathCost pathEnd = (x, y, z) where x = pull1 pathEnd
                                              y = pull2 pathEnd
                                              z = pathCost + pull3 pathEnd

backDoor :: [(Float, Float)] -> ((Float, Float), (Float, Float), Float) -> (Float, Float)
backDoor input node
            | input' == True = pull2 node
            | otherwise = pull1 node
                where input' = elem (pull1 node) input

shortPath :: [((Float, Float), (Float, Float), Float)] -> ((Float, Float), (Float, Float), Float)
shortPath paths
            | length paths < 2 = head paths
            | x > y = shortPath (head paths : drop 2 paths)
            | otherwise = shortPath (drop 1 paths)
                    where x = pull3 (paths !! 0)
                          y = pull3 (paths !! 1)

notPath :: [((Float, Float), (Float, Float), Float)] -> ((Float, Float), (Float, Float), Float) -> Bool
notPath outList input
            | head outList == input = False
            | length outList == 1 = True
            | otherwise = notPath outList' input
                where outList' = tail outList

breakNodes :: [(Float, Float)] -> ((Float, Float), (Float, Float), Float) -> [(Float, Float)]
breakNodes initList node = pull1 node : pull2 node : initList

cullNodes :: [[(Float, Float)]] -> [[(Float, Float)]]
cullNodes inList
            | length inList < 2 = [z]
            | length z == 1 = z : cullNodes inList'
            | otherwise = cullNodes inList'
                where z = head inList
                      inList' = tail inList

--formConfig :: [(Float, Float)] -> (Float, Float) -> [[(Float, Float)]] -> [(Float, Float)]
--formConfig input target obstacles
                --See how close to obstacle a path is, then adjust the width of the config
                --if bestMove !isCollision then make that move with the first one.
                --bestMove if !isCollision and dist2ASV (n - 1) <= 0.05 then make that move.
                --

dist2ASV :: (Float, Float) -> (Float, Float) -> Float
dist2ASV pointA pointB = sqrt (a**2 + b**2) where a = abs(fst pointA - fst pointB)
                                                  b = abs(snd pointA - snd pointB)

bestMove :: (Float, Float) -> (Float, Float) -> (Float, Float)
bestMove pointA pointB
        | (x == y) && (x == 0) = pointA
        | x' > y' && x < 1     = (fst pointA - 0.001, snd pointA)
        | x' > y' && x > 1     = (fst pointA + 0.001, snd pointA)
        | x' < y' && y < 1     = (fst pointA, snd pointA - 0.001)
        | otherwise            = (fst pointA, snd pointA + 0.001)
            where x  = fst pointA - fst pointB
                  y  = snd pointA - snd pointB
                  x' = abs x
                  y' = abs y

findCentre :: [(Float, Float)] -> (Float, Float)
findCentre config
        | odd x == True  = config !! (x - (x + 1)/2)
        | otherwise      = (a, b)
                where x = length config
                      x' = x - (x+1)/2
                      a = {-config x/2, config x/2 + 1 -}
                      b = {-Something to do with that ^^^^^ -}
