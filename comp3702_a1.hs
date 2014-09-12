import System.IO
import System.Environment
import System.IO.Error
import Data.List
import Control.Monad
import Numeric
import System.Random
import Data.Fixed
import GHC.Float.RealFracMethods

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
                            nASV = length start
                            limitsX = getBounds nASV
                            start' = findCentre start
                            end' = findCentre goal
                            nASV' = int2Float nASV
--                            pointsx = take (seedn*seedn + 10) $ randomRs (1, 100) (mkStdGen (seedn)) :: [Integer]
--                            points' = map ((/100) . fromInteger) pointsx
--                            points'' = randomPairs points'
--
--                            {-Filter points that would be in the forbidden zone-}
--                            legalPoints = filter (checkCollision obstacles) points''
                            perths = pathBeast [start'] [end'] bounds' obstacles
                            perths' = filter (checkPath'' obstacles) perths
                            bounds = filter (isLegal) $ foldl (++) [] (map fixBounds obstacles)
                            bounds' = compressNodes $ filter (checkCollision obstacles) bounds

                            configs = configDuck bestPath start' nASV' relevant obstacles
                            configs' = tail $ findConvexity configs
--
--                            {-Begin processing paths, first check that the path to a point
--                             is possible-}
--                            failBus = paveRoads (start' : legalPoints ++ [end']) (legalPoints ++ [end']) obstacles
--                            failBus = group $ sort perths'
--                            noRepeat = map head failBus

                            bestPath = uCS perths' [] [start'] end' obstacles
                            tCost = sum $ map pull3 bestPath
                            tCostStr = show tCost
                            widthsX = map (plotPathWidths obstacles) bestPath
                            widthsX' = map (setBounds (fst limitsX)) widthsX
                            slopeslol = getGradients start' bestPath
                            angles = processAngles slopeslol start' bestPath
                            xTraverse = xTravel' start' bestPath
                            relevant = pathDeets widthsX' xTraverse slopeslol

--                            startx = rotateASV start (head configs)
                            theRealDeal = start: filthWizard start goal (configs' ++ [goal])
                            theRealDeal' = map formatOut theRealDeal
                            stepCount = length theRealDeal - 1
                            stepCountStr = show stepCount

                            fileHead = stepCountStr ++ " " ++ tCostStr

                        mapM_ print perths'
                        output <- openFile "out.txt" WriteMode
                        hPutStrLn output fileHead
                        mapM_ (hPutStrLn output) theRealDeal'
                        hClose output

--Forms paths from the sampled points based on obstacle locations and allowable proximity.
paveRoads :: [(Float, Float)] -> [(Float, Float)] ->
             [[(Float, Float)]] ->
             [((Float, Float), (Float, Float), Float)]
paveRoads points1 points2 obstacles
                | length points1 == 2 = [z]
                | x == True = z : paveRoads points' points2' obstacles
                | otherwise = paveRoads points' points2' obstacles
                    where x = checkPath' a b obstacles
                          a = head points1
                          points' = tail points1
                          points2' = filter (/=a) points2
                          b = nodeSniffer a (filter (/=a) points2)
                          z = makePath a b

--This is a uniform cost search. Duh.
uCS :: [((Float, Float), (Float, Float), Float)] ->
       [((Float, Float), (Float, Float), Float)] ->
       [(Float, Float)] ->
       (Float, Float) ->
       [[(Float, Float)]] ->
       [((Float, Float), (Float, Float), Float)]
uCS paths output start finish obstacles =
                if (elem finish start') == True
                    then do
                           let x = group . sort $ foldl breakNodes [] outputx
                               x'= map head $ cullNodes x
                               y = filter (/= start' !! (length start' - 1)) $ takeWhile (/= finish) $ x'
                               y' = reverse $ filter (pathNotContain y) outputx
                           y'

                    else do
                         uCS newmap'' outputx start' finish obstacles
                        where   cheapest = shortPath $ filter (pathContains start) paths
--                                useful = filter (usefulFilter 0.01) paths
                                adjacent = filter (pathContains [(backDoor start cheapest)]) paths
                                newCosts' = map (updateCost (pull3 cheapest)) newCosts
                                newCosts = filter (notPath [cheapest]) adjacent
                                newmap' = filter (notPath adjacent) newmap
                                newmap = filter (/= cheapest) paths
                                newmap'' = newmap' ++ newCosts'
                                outputx = cheapest : output
                                start' = (backDoor start cheapest) : start

usefulFilter :: Float -> ((Float, Float), (Float, Float), Float) -> Bool
usefulFilter delim path = if pull3 path < delim then False else True

compressNodes :: [(Float, Float)] -> [(Float, Float)]
compressNodes input
       | length input < 2       = [z]
       | (dist2ASV z y) < 0.01   = compressNodes input'
       | otherwise              = z : compressNodes input''
            where z = head input
                  input'' = tail input
                  y = head input''
                  input' = safest y z : drop 2 input

safest :: (Float, Float) -> (Float, Float) -> (Float, Float)
safest pointA pointB = (a, b) where a = (fst pointA - (fst pointA - fst pointB)/2)
                                    b = (snd pointA - (snd pointA - snd pointB)/2)

findConvexity :: [[(Float, Float)]] -> [[(Float, Float)]]
findConvexity nodes
    | x > y     = nodes
    | otherwise = map (reverse) nodes
        where x = (((dist2ASV delta' (0.0, 0.0)) - (dist2ASV (0.0, 0.0) (head delta))))
              y = (((dist2ASV delta' (0.0, 0.0)) - (dist2ASV (0.0, 0.0) (last delta))))
              delta = head nodes
              delta' = delta !! num
              num = ceilingFloatInt (int2Float (length delta) / 2) - 1

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

pathBeast :: [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)] -> [[(Float, Float)]] -> [((Float, Float), (Float, Float), Float)]
pathBeast start end bounds obstacles
                | length bounds' < 2 = z
                | canPath x end obstacles == True = z
                | otherwise                   = z' : (pathBeast start' end' bounds' obstacles) ++ [z'']
                    where z = makePath' x end obstacles
                          x = last start
                          start' = start ++ [navigate x bounds obstacles]
                          end' = (navigate (head end) bounds obstacles) : end
                          bounds' = filter (/=(last start')) $ filter (/=(head end')) bounds
                          z' = makePath x (last start')
                          z'' = makePath (head end) (head end')

purgeBacon :: [(Float ,Float)] -> [(Float, Float)] -> [[(Float, Float)]] -> [((Float ,Float), (Float, Float), Float)]
purgeBacon startL endL obs
        | length startL < 2   = z
        | otherwise = z ++ purgeBacon startL' endL obs
            where z = makePath' (last startL) endL obs
                  startL' = init startL

fixBounds :: [(Float, Float)] -> [(Float, Float)]
fixBounds input
    | length input < 2 = [z]
    | length input < 3 = y : fixBounds input'
    | length input < 4 = x : fixBounds input'
    | otherwise        = l : fixBounds input'
            where l = (fst a - 0.01, snd a - 0.01)
                  x = (fst a + 0.01, snd a - 0.01)
                  y = (fst a + 0.01, snd a + 0.01)
                  z = (fst a - 0.01, snd a + 0.01)
                  a = head input
                  input' = tail input

isCollision :: (Float, Float) -> [(Float, Float)] -> Bool
isCollision posXY fZone
            | (( a < x) && ( x < b ) && (c < y) && (y < d)) = True
            | otherwise = False
                where x = fst posXY
                      y = snd posXY
                      a = fst (fZone !! 0) - 0.001
                      b = fst (fZone !! 1) + 0.001
                      c = snd (fZone !! 1) - 0.001
                      d = snd (fZone !! 2) + 0.001

dist2Obstacle :: [[(Float, Float)]] -> (Float, Float)-> [Float]
dist2Obstacle fZone node
                | length fZone < 2 = [minDist]
                | otherwise        = minDist : dist2Obstacle fZone' node
                      where z = map (dist2ASV node) $ head fZone
                            fZone' = tail fZone
                            minDist = minimum z

getPathwidth :: [[(Float, Float)]] -> (Float, Float) -> (Float, Float) -> Float
getPathwidth fZone pointA pointB
                    | length xs == 0 = 0.01
                    | otherwise =     minimum xs
                            where xs = map (minimum . dist2Obstacle fZone) lineEq
                                  xlist = [fst x1, (fst x1 + dx) .. fst x2]
                                  x1 = (\x y -> if fst x > fst y then y else x) pointA pointB
                                  x2 = (\x y -> if fst x <= fst y then y else x) pointA pointB
                                  ylist = [snd x1, (snd x1 + dy) .. snd x2]
                                  dx = (fst x2 - fst x1)/30
                                  dy = (snd x2 - snd x1)/30
                                  lineEq = zip xlist ylist

plotPathWidths :: [[(Float, Float)]] -> ((Float,Float), (Float, Float), Float) -> Float
plotPathWidths fZone path = getPathwidth fZone (pull1 path) (pull2 path)

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
checkPath' :: (Float, Float) -> (Float, Float) -> [[(Float, Float)]] -> Bool
checkPath' pointA pointB obstacles
    | (length lineEq) == (length canPass) = True
    | otherwise = False
        where xlist = [fst x1, (fst x1 + dx) .. fst x2]
              x1 = (\x y -> if fst x > fst y then y else x) pointA pointB
              x2 = (\x y -> if fst x <= fst y then y else x) pointA pointB
              ylist = [snd x1, (snd x1 + dy) .. snd x2]
              dx = (fst x2 - fst x1)/50
              dy = (snd x2 - snd x1)/50
              lineEq = zip xlist ylist
              canPass = filter (checkCollision obstacles) lineEq

checkPath'' :: [[(Float, Float)]] -> ((Float, Float), (Float, Float), Float) -> Bool
checkPath'' obs path = checkPath' (pull1 path) (pull2 path) obs

--Returns the closest node to a specific reference node, from an list of unexplored nodes.
nodeSniffer :: (Float, Float) -> [(Float, Float)] -> (Float, Float)
nodeSniffer start points
                   | length points == 1 = head points
                   | prox1 > prox2 = nodeSniffer start (tail points)
                   | otherwise  = nodeSniffer start (head points : drop 2 points)
                        where prox1 = abs(fst start - fst (head points)) +
                                      abs(snd start - snd (head points))
                              prox2 = abs(fst start - fst (points !! 1)) +
                                      abs(snd start - snd (points !! 1))

--Returns the closest node to a specific reference node, from an list of unexplored nodes.
navigate :: (Float, Float) -> [(Float, Float)] -> [[(Float, Float)]] -> (Float, Float)
navigate start points obstacles
                   | length points < 2 = head points
                   | (canReach == False) = navigate start (tail points) obstacles
                   | (canReach' == True) && (prox1 > prox2) = navigate start (tail points) obstacles
                   | otherwise  = navigate start (head points : drop 2 points) obstacles
                        where prox1 = dist2ASV start (head points)
                              prox2 = dist2ASV start (head $ tail points)
                              canReach = checkPath' start (head points) obstacles
                              canReach' = (checkPath' start (head points) obstacles) &&
                                           (checkPath' start (head $ tail points) obstacles)

isLegal :: (Float, Float) -> Bool
isLegal vertex
    | ((x' > 0) && (x < 1) && (y' > 0) && (y < 1)) == True = True
    | otherwise = False
        where x = fst vertex + 0.05
              x' = fst vertex - 0.05
              y = snd vertex + 0.05
              y' = snd vertex - 0.05

--Returns the cost of travelling between two nodes.
getCost :: (Float, Float) -> (Float, Float) -> Float
getCost pointA pointB = sqrt((fst pointA - fst pointB)**2 + (snd pointA - snd pointB)**2)

--Function to asses paths between points, and return a list with costs in it.
makePath :: (Float, Float) -> (Float, Float) -> ((Float, Float), (Float, Float), Float)
makePath pointA pointB
                | fst pointA < fst pointB = (pointA, pointB, getCost pointA pointB)
                | otherwise = (pointB, pointA, getCost pointA pointB)

makePath' :: (Float, Float) -> [(Float, Float)] -> [[(Float, Float)]] -> [((Float, Float), (Float, Float), Float)]
makePath' start endList obs
            | length endList < 2      = [z]
            | x == True && y == False = [z]
            | otherwise               = makePath' start endList' obs
                where x = checkPath' start (head endList) obs
                      z = makePath start (head endList)
                      y = checkPath' start (head endList') obs
                      endList' = tail endList

canPath :: (Float, Float) -> [(Float, Float)] -> [[(Float, Float)]] -> Bool
canPath node list obs
       | x = True
       | length list < 2  = False
       | otherwise     = canPath node list' obs
            where x = checkPath' node (head list) obs
                  list' = tail list

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
                    where x = pull3 $ head paths
                          y = pull3 $ head (tail paths)

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

dist2ASV :: (Float, Float) -> (Float, Float) -> Float
dist2ASV pointA pointB = sqrt (a**2 + b**2) where a = abs(fst pointA - fst pointB)
                                                  b = abs(snd pointA - snd pointB)

gradient :: (Float, Float) -> (Float, Float) -> Float
gradient pointA pointB =  b/a where a = (fst pointB - fst pointA)
                                    b = (snd pointB - snd pointA)

getGradients :: (Float, Float) -> [((Float, Float), (Float, Float), Float)] -> [Float]
getGradients start paths = case paths of [] -> []
                                         x -> x' : getGradients start' paths'
                                            where x' = gradient start start'
                                                  start' = backDoor [start] $ head paths
                                                  paths' = tail paths

xTravel' :: (Float, Float) -> [((Float, Float), (Float, Float), Float)] -> [Float]
xTravel' start paths = case paths of [] -> []
                                     x -> x' : xTravel' start' paths'
                                         where x' = xTravel start start'
                                               start' = backDoor [start] $ head paths
                                               paths' = tail paths

bestMove :: (Float, Float) -> (Float, Float) -> (Float, Float)
bestMove pointA pointB
        | (0.001 > y') && (x' < 0.001) = pointA
        | (x' > y') && (x < 0)     = (fst pointB + 0.001, snd pointB)
        | (x' > y') && (x > 0)     = (fst pointB - 0.001, snd pointB)
        | (x' < y') && (y < 0)     = (fst pointB, snd pointB + 0.001)
        | (x' < y') && (y > 0)     = (fst pointB, snd pointB - 0.001)
        | otherwise            = (fst pointB + 0.0005, snd pointB + 0.0005)
            where x  = fst pointB - fst pointA
                  y  = snd pointB - snd pointA
                  x' = abs x
                  y' = abs y

bestMove' :: [(Float, Float)] -> [(Float, Float)] -> [(Float, Float)]
bestMove' pointsA pointsB
        | length pointsA < 2 = [z]
        | otherwise          = z : bestMove' pointsA' pointsB'
                where z = bestMove (head pointsA) (head pointsB)
                      pointsA' = tail pointsA
                      pointsB' = tail pointsB

bestMoveR :: [(Float, Float)] -> [(Float, Float)] -> (Float, Float) -> Float -> [(Float, Float)]
bestMoveR pointsA pointsB centre total
        | lr < 2       = [z]
        | otherwise    = z : bestMoveR pointsA' pointsB' centre total
                  where z = bestMoveR' (head pointsA) (head pointsB) centre angle
                        angle = lr' - total
                        pointsA' = tail pointsA
                        pointsB' = tail pointsB
                        lr = int2Float $ length pointsA
                        lr' = int2Float $ floorFloatInt (total/2)

bestMoveRX :: [(Float, Float)] -> [(Float, Float)] -> (Float, Float) -> Float -> [(Float, Float)]
bestMoveRX pointsA pointsB centre total
        | lr < 2       = [z]
        | otherwise    = z : bestMoveRX pointsA' pointsB' centre total
                  where z = bestMoveX (head pointsA) (head pointsB) centre angle
                        angle = lr' - total
                        pointsA' = tail pointsA
                        pointsB' = tail pointsB
                        lr = int2Float $ length pointsA
                        lr' = int2Float $ floorFloatInt (total/2)

bestMoveR' :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Float -> (Float, Float)
bestMoveR' pointA pointB ref angle
        | pointA == ref = pointA
        | angle > 0   = (fst pointA + 0.001, snd pointA)
        | otherwise   = (fst pointA - 0.001, snd pointA)

bestMoveX :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Float -> (Float, Float)
bestMoveX pointA pointB ref angle
        | pointA == ref = pointA
        | (angle > 0) = (fst pointA, snd pointA + 0.001)
        | otherwise   = (fst pointA, snd pointA - 0.001)

calcAngle :: Float -> Float -> Float
calcAngle nASV ratioRad
        | nASV < 2 = 90/z
        | otherwise = z + calcAngle nASV' ratioRad
            where nASV' = nASV - 1
                  z = ratioRad**nASV'

formatOut :: [(Float, Float)] -> String
formatOut points = foldl (++) [] (map (\(a, b) -> show a ++ " " ++ show b ++ " ") points)

findCentre :: [(Float, Float)] -> (Float, Float)
findCentre config
        | odd x == True  = config !! (x - z')
        | otherwise      = (a, b)
                where x = length config
                      z = floor $ (int2Float x)/2
                      z' = ceiling $ (int2Float x)/2
                      deltax = fst (config !! z) - fst (config !! z')
                      deltay = snd (config !! z) - snd (config !! z')
                      a = fst (config !! z) + deltax/2
                      b = snd (config !! z) + deltay/2

xTravel :: (Float, Float) -> (Float, Float) -> Float
xTravel pointA pointB = snd pointB - snd pointA

getAnglePos :: (Float, Float) -> (Float, Float) -> Float
getAnglePos pointA pointB = 57.29 * atan (y/x) where x = abs(fst pointB - fst pointA)
                                                     y = abs(snd pointB - snd pointA)

getAnglePos' :: ((Float, Float), (Float, Float), Float) -> Float
getAnglePos' path = getAnglePos (pull1 path) (pull2 path)

getAngleNeg :: (Float, Float) -> (Float, Float) -> Float
getAngleNeg pointA pointB = 57.29 * atan (x/y) where x = abs(fst pointB - fst pointA)
                                                     y = abs(snd pointB - snd pointA)

getAngleNeg' :: ((Float, Float), (Float, Float), Float) -> Float
getAngleNeg' path = getAnglePos (pull1 path) (pull2 path)

processAngles :: [Float] -> (Float, Float) -> [((Float, Float), (Float, Float), Float)] -> [Float]
processAngles slopes start paths
            | length paths < 2 = [z]
            | head slopes > 0  = z : processAngles slopes' lead paths'
            | otherwise        = y : processAngles slopes' lead paths'
                where z = getAnglePos start lead
                      y = getAngleNeg start lead
                      lead = backDoor [start] $ head paths
                      slopes' = tail slopes
                      paths' = tail paths

configDuck :: [((Float, Float), (Float, Float), Float)] ->
                (Float, Float) ->
                Float ->
                [(Float, Float, Float)] ->
                [[(Float, Float)]] ->
                [[(Float, Float)]]
configDuck paths start nASV pathInfo obs
        | length paths < 2 = [n']
        | otherwise        = n' : m' : configDuck paths' start' nASV pathInfo' obs
            where paths' = tail paths
                  pathInfo' = tail pathInfo
                  y = head pathInfo
                  x = head paths
                  num = int2Float $ floorFloatInt (nASV/2)
                  n = configBane start start nASV nASV y obs
                  m = configBane lordJesus lordJesus nASV nASV y obs
                  m' = octopusSort m
                  n' = octopusSort n
                  start' = backDoor [start] x
                  halfx = fst start - ((fst start - fst start')/2)
                  halfy = snd start - ((snd start - snd start')/2)
                  lordJesus = (halfx, halfy)

configBane :: (Float, Float) ->
              (Float, Float) ->
              Float ->
              Float ->
              (Float, Float, Float) ->
              [[(Float, Float)]] ->
              [(Float, Float)]
configBane centre point nASV total pathInfo obs
        | (gradY < 0) && (travX < 0)   = n
        | (gradY > 0) && (travX > 0)   = m
        | (gradY < 0) && (travX > 0)   = n
        | (gradY > 0) && (travX < 0)   = m
        | otherwise                    = m
                    where gradY = pull3 pathInfo
                          travX = pull2 pathInfo
                          z = reflectConfigC centre point nASV total pathInfo
                          j = reflectConfigA centre point nASV total pathInfo
                          k = reflectConfigB centre point nASV total pathInfo
                          l = reflectConfigD centre point nASV total pathInfo
                          m = reflectConfigZ centre point nASV total pathInfo obs
                          n = reflectConfigY centre point nASV total pathInfo obs

--Negative X-Travel with a negative gradient.
reflectConfigA :: (Float, Float) -> (Float, Float) -> Float -> Float -> (Float, Float, Float) -> [(Float, Float)]
reflectConfigA centre point nASV total pathinfo
    | nASV < 2 = [(c, d)]
    | nASV > num = (a, b) : reflectConfigA centre point' nASV' total pathinfo
    | nASV > (num - 1) = centre : reflectConfigA centre centre nASV' total pathinfo
    | otherwise  = (c, d) : reflectConfigA centre point'' nASV' total pathinfo
        where pWidth = (pull1 pathinfo)
              grad = pull3 pathinfo
              point' = (a, b)
              point'' = (c, d)
              theta = atan((grad))
              phi = (nASV - num)
              phi' = nASV
              num = int2Float $ ceilingFloatInt (total/2)
              a = fst point - (0.05 * cos (-phi + theta))
              b = snd point + (0.05 * sin (-phi + theta))
              c = fst point + (0.05 * cos (phi' + theta))
              d = snd point + (0.05 * sin (phi' + theta))
              nASV' = nASV - 1

--Positive X-Travel and positive gradient
reflectConfigB :: (Float, Float) -> (Float, Float) -> Float -> Float -> (Float, Float, Float) -> [(Float, Float)]
reflectConfigB centre point nASV total pathinfo
    | nASV < 2 = [(c, d)]
    | nASV > num = (a, b) : reflectConfigB centre point' nASV' total pathinfo
    | nASV > (num - 1) = centre : reflectConfigB centre centre nASV' total pathinfo
    | otherwise  = (c, d) : reflectConfigB centre point'' nASV' total pathinfo
        where pWidth = pull1 pathinfo
              grad = pull3 pathinfo
              point' = (a, b)
              point'' = (c, d)
              theta = atan((grad))
              phi = (nASV - num)
              phi' = nASV
              num = int2Float $ ceilingFloatInt (total/2)
              a = fst point + (0.05 * cos (phi + theta))
              b = snd point + (0.05 * sin (phi + theta))
              c = fst point - (0.05 * cos (-phi' + theta))
              d = snd point - (0.05 * sin (-phi' + theta))
              nASV' = nASV - 1

--Negative X-Travel ,positive gradient.
reflectConfigC :: (Float, Float) -> (Float, Float) -> Float -> Float -> (Float, Float, Float) -> [(Float, Float)]
reflectConfigC centre point nASV total pathinfo
    | nASV < 2 = [(c, d)]
    | nASV > num = (a, b) : reflectConfigC centre point' nASV' total pathinfo
    | nASV > (num - 1) = centre : reflectConfigC centre centre nASV' total pathinfo
    | otherwise  = (c, d) : reflectConfigC centre point'' nASV' total pathinfo
        where pWidth = pull1 pathinfo
              grad = pull3 pathinfo
              point' = (a, b)
              point'' = (c, d)
              theta = atan((grad))
              phi = (nASV - num)
              phi' = nASV
              num = int2Float $ ceilingFloatInt (total/2)
              a = fst point - (0.05 * cos (phi + theta))
              b = snd point - (0.05 * sin (phi + theta))
              c = fst point - (0.05 * cos (phi' + theta))
              d = snd point + (0.05 * sin (phi' + theta))
              nASV' = nASV - 1

--Positive X-travel, negative gradient.
reflectConfigD :: (Float, Float) -> (Float, Float) -> Float -> Float -> (Float, Float, Float) -> [(Float, Float)]
reflectConfigD centre point nASV total pathinfo
    | nASV < 2 = [(c, d)]
    | nASV > num = (a, b) : reflectConfigD centre point' nASV' total pathinfo
    | nASV > (num - 1) = centre : reflectConfigD centre centre nASV' total pathinfo
    | otherwise  = (c, d) : reflectConfigD centre point'' nASV' total pathinfo
        where pWidth = pull1 pathinfo
              grad = pull3 pathinfo
              point' = (a, b)
              point'' = (c, d)
              theta = atan((grad))
              phi = 90 - (total - nASV)
              phi' = 90 - nASV
              num = int2Float $ ceilingFloatInt (total/2)
              a = fst point - (0.05 * cos phi)
              b = snd point + (0.05 * sin phi)
              c = fst point + (0.05 * cos phi')
              d = snd point + (0.05 * sin phi')
              nASV' = nASV - 1

reflectConfigZ :: (Float, Float) ->
                  (Float, Float) ->
                  Float ->
                  Float ->
                  (Float, Float, Float) ->
                  [[(Float, Float)]] ->
                  [(Float, Float)]
reflectConfigZ centre point nASV total pathinfo obs
    | nASV < 2 = [h2]
    | nASV > num = h1 : reflectConfigZ centre h1 nASV' total pathinfo obs
    | nASV > (num - 1) = centre : reflectConfigZ centre centre nASV' total pathinfo obs
    | otherwise  = h2 : reflectConfigZ centre h2 nASV' total pathinfo obs
        where pWidth = (pull1 pathinfo)*100
              grad = pull3 pathinfo
              point' = (a, b)
              point'' = (c, d)
              phi = 3*(total - nASV)*0.174
              phi' = 3*(num - nASV)*0.174
              num = int2Float $ ceilingFloatInt (total/2)
              h1 = checkSqualor point' point obs
              h2 = checkSqualor point'' point obs
              a = fst point - (0.05 * cos phi)
              b = snd point + (0.05 * sin phi)
              c = fst point + (0.05 * cos phi')
              d = snd point + (0.05 * sin phi')
              nASV' = nASV - 1

reflectConfigY :: (Float, Float) ->
                  (Float, Float) ->
                  Float ->
                  Float ->
                  (Float, Float, Float) ->
                  [[(Float, Float)]] ->
                  [(Float, Float)]
reflectConfigY centre point nASV total pathinfo obs
    | nASV < 2 = [h2]
    | nASV > num = h1 : reflectConfigY centre h1 nASV' total pathinfo obs
    | nASV > (num - 1) = centre : reflectConfigY centre centre nASV' total pathinfo obs
    | otherwise  = h2 : reflectConfigY centre h2 nASV' total pathinfo obs
        where pWidth = (pull1 pathinfo)
              grad = pull3 pathinfo
              point' = (a, b)
              point'' = (c, d)
              phi = 3*(nASV - num)*0.174
              phi' = 3*(nASV)*0.174
              num = int2Float $ ceilingFloatInt (total/2)
              h1 = checkSqualor point' point obs
              h2 = checkSqualor point'' point obs
              a = fst point + (0.05 * cos phi)
              b = snd point + (0.05 * sin phi)
              c = fst point + (0.05 * cos phi')
              d = snd point - (0.05 * sin phi')
              nASV' = nASV - 1

checkConfig :: [(Float, Float)] -> [[(Float, Float)]] -> [(Float, Float)]
checkConfig input obs
    | length input < 2 = [z]
    | otherwise        = z : checkConfig input' obs
            where z = checkSqualor (head input') (head input) obs
                  input' = tail input

checkSqualor :: (Float, Float) -> (Float, Float) -> [[(Float, Float)]] -> (Float, Float)
checkSqualor pointA pointB obstacles
        | length obstacles < 2 = pointA
        | z == True            = fixASV pointA pointB n
        | otherwise            = checkSqualor pointA pointB obstacles'
            where z = isCollision pointA n
                  n = head obstacles
                  obstacles' = tail obstacles

fixASV :: (Float, Float) -> (Float, Float) -> [(Float, Float)] -> (Float, Float)
fixASV posXY centre fZone
            | (px == dx) = (a - 0.001, refy + ysign*sqrt (0.0025 - (a - refx - 0.001)**2))
            | (px == dx') = (b + 0.001, refy + ysign*sqrt (0.0025 - (refx - b + 0.001)**2))
            | (px == dy) = (refx + xsign*sqrt (0.0025 - (c - refy - 0.001)**2), c - 0.001)
            | (px == dy') = (refx + xsign*sqrt (0.0025 - (refy - d + 0.001)**2), d + 0.001)
            | otherwise = posXY
                where refx = fst centre
                      refy = snd centre
                      x = fst posXY
                      y = snd posXY
                      xsign = (\x y -> if x - y > 0 then 1 else -1) refx x
                      ysign = (\x y -> if x - y > 0 then 1 else -1) refy y
                      a = fst (fZone !! 0) - 0.001
                      b = fst (fZone !! 1) + 0.001
                      c = snd (fZone !! 1) - 0.001
                      d = snd (fZone !! 2) + 0.001
                      dx = abs (refx - a)
                      dx' = abs (refx - b)
                      dy = abs (refy - c)
                      dy' = abs (refy - d)
                      spaces = [dx] ++ [dx'] ++ [dy] ++ [dy']
                      px = minimum spaces

filthWizard :: [(Float, Float)] -> [(Float, Float)] -> [[(Float, Float)]] -> [[(Float, Float)]]
filthWizard startState endState configs
        | z == endState      = [z]
        | length configs < 2 = z : filthWizard z endState configs
        | z == alpha         = z : filthWizard z endState configs'
--        | x == True         = z : filthWizard z endState configs'
--        | x == True          = z' ++ filthWizard (last z') endState configs'
        | otherwise          = z : filthWizard z endState configs
                where alpha = head configs
                      z = bestMove' alpha startState
                      configs' = tail configs
                      tarJET = head configs'
                      z' = rotateASV z tarJET
                      x = asvContact z alpha


asvContact :: [(Float, Float)] -> [(Float, Float)] -> Bool
asvContact input target
    | length input < 2 = False
    | x == True        = True
    | otherwise        = asvContact input' target
        where a = head input
              x = elem a target
              input' = tail input

rotateASV :: [(Float, Float)] -> [(Float, Float)] -> [[(Float, Float)]]
rotateASV start target
    | gradT > 0.9 = [z]
    | otherwise    = y : z : rotateASV z target
        where dy1 = abs((snd $ head start) - (snd $ last start))
              dy2 = abs((snd $ head target) - (snd $ last target))
              dy = dy1 - dy2
              dx1 =  abs((fst $ head start) - (fst $ last start))
              dx2 = abs((fst $ head target) - (fst $ last target))
              dx = dx1 - dx2
              d1 = dist2ASV (head start) (head target)
              d2 = dist2ASV (last start) (last target)
              delta = gradT - gradS
              delim = (snd $ head target) - (snd $ head start)
              fx = int2Float $ length start
              cent = start !! (ceilingFloatInt (fx/2) - 1)
              y = bestMoveR start target cent fx
              z = bestMoveRX y target cent fx
              gradT = abs $ gradient (head target) (head start)
              gradS = gradient (head start) (start !! num)
              num = ceilingFloatInt (int2Float (length target) / 2) - 1

octopusSort :: [(Float, Float)] -> [(Float, Float)]
octopusSort inList = xs ++ xy where xy = drop num inList
                                    num = ceilingFloatInt (int2Float (length inList) / 2) - 1
                                    xz = take num inList
                                    xs = reverse xz

angleCreep :: Float -> Float -> Float -> Float
angleCreep minAngle delta mult
        | mult < 1 = (1 + z)*minAngle
        | otherwise = z + angleCreep minAngle delta mult'
            where z = delta**mult
                  mult' = mult - 1

pathDeets :: [Float] -> [Float] -> [Float] -> [(Float, Float, Float)]
pathDeets widths angles grads
            | length widths < 2 = [(a, b, c)]
            | otherwise         = (a, b, c) : pathDeets widths' angles' grads'
                       where a = head widths
                             b = head angles
                             c = head grads
                             widths' = tail widths
                             angles' = tail angles
                             grads' = tail grads

getBounds :: Int -> (Float, Float)
getBounds nASV = (rMax,  bMin) where rMax = (int2Float $ 15*nASV) / 1000
                                     bMin = (int2Float $ 2*nASV*nASV - 4*nASV) / 1000

setBounds :: Float -> Float -> Float
setBounds a b
        | a > b = b
        | otherwise = a
