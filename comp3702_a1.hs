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
                            points = take (seedn*20) $ randomRs (1, 100) (mkStdGen (seedn)) :: [Integer]
                            points' = map ((/100) . fromInteger) points
                            points'' = randomPairs points'

                            {-Filter points that would be in the forbidden zone-}
                            legalPoints = filter (checkCollision obstacles) points''

                            nASV = length start
                            limitsX = getBounds nASV
                            start' = findCentre start
                            end' = findCentre goal
                            nASV' = int2Float nASV

                            tester = configD start' nASV' nASV' (head relevant)

                            {-Begin processing paths, first check that the path to a point
                             is possible-}
                            failBus = paveRoads (start' : legalPoints ++ [end']) (legalPoints ++ [end']) obstacles
                            failBus'= group $ sort failBus
                            noRepeat = map head failBus'

                            bestPath = uCS noRepeat [] [start'] end' obstacles
                            tCost = sum $ map pull3 bestPath
                            widthsX = map (plotPathWidths obstacles) bestPath
                            widthsX' = map (setBounds (fst limitsX)) widthsX
                            slopeslol = getGradients start' bestPath
                            angles = processAngles slopeslol start' bestPath
                            xTraverse = xTravel' start' bestPath
                            relevant = pathDeets widthsX' xTraverse slopeslol

                        putStr "Start: "
                        print start'
                        putStr "Goal: "
                        print end'
                        print tester
                        mapM_ print relevant
--                        mapM_ print xTraverse
--                        mapM_ print bestPath

--Forms paths from the sampled points based on obstacle locations and allowable proximity.
paveRoads :: [(Float, Float)] -> [(Float, Float)] ->
             [[(Float, Float)]] ->
             [((Float, Float), (Float, Float), Float)]
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
uCS :: [((Float, Float), (Float, Float), Float)] ->
       [((Float, Float), (Float, Float), Float)] ->
       [(Float, Float)] ->
       (Float, Float) ->
       [[(Float, Float)]] ->
--       IO()
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

dist2Obstacle :: [[(Float, Float)]] -> (Float, Float)-> [Float]
dist2Obstacle fZone node
                | length fZone < 2 = [minDist]
                | otherwise        = minDist : dist2Obstacle fZone' node
                      where z = map (dist2ASV node) $ head fZone
                            fZone' = tail fZone
                            minDist = minimum z

getPathwidth :: [[(Float, Float)]] -> (Float, Float) -> (Float, Float) -> Float
getPathwidth fZone pointA pointB
                    | length xs == 0 = 0.05
                    | otherwise =     minimum xs
                            where xs = map (minimum . dist2Obstacle fZone) lineEq
                                  xlist = [fst pointA, (fst pointA + 0.05) .. fst pointB]
                                  ylist = [snd pointA, (snd pointA + 0.05) .. snd pointB]
                                  grad = (snd pointB - snd pointA)/(fst pointB - fst pointA)
                                  beta = (snd pointA) - grad*(fst pointA)
                                  lineEq = [ ((y - beta)/grad, grad*x + beta) | x <- xlist, y <- ylist]

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
                        where prox1 = abs(fst start - fst (points !! 0)) +
                                      abs(snd start - snd (points !! 0))
                              prox2 = abs(fst start - fst (points !! 1)) +
                                      abs(snd start - snd (points !! 1))

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
        | (0.001 > y') && (x' < 0.001) = pointB
        | x' > y' && x < 1     = (fst pointB + 0.001, snd pointB)
        | x' > y' && x > 1     = (fst pointB - 0.001, snd pointB)
        | x' < y' && y < 1     = (fst pointB, snd pointB + 0.001)
        | otherwise            = (fst pointB, snd pointB - 0.001)
            where x  = fst pointB - fst pointA
                  y  = snd pointB - snd pointA
                  x' = abs x
                  y' = abs y

calcAngle :: Float -> Float -> Float
calcAngle nASV ratioRad
        | nASV < 2 = 90/z
        | otherwise = z + calcAngle nASV' ratioRad
            where nASV' = nASV - 1
                  z = ratioRad**nASV'

followPath :: (Float, Float) -> (Float, Float) -> [(Float, Float)] -> [[(Float, Float)]]
followPath start end configIn
                | (dx < 0.009) && (dy < 0.009) = [z']
                | otherwise =  z' : followPath z end z'
                    where z = bestMove end start
                          z' = map (bestMove z) configIn
                          dx = abs(fst start - fst end)
                          dy = abs(snd start - snd end)


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

configA :: (Float, Float) -> Float -> Float -> (Float, Float, Float) -> [(Float, Float)]
configA centre nASV total pathinfo
            | nASV < 2           = [(a, b)]
            | nASV > (total/2)   = (a, b) : configA centre nASV' total pathinfo
            | otherwise          = (c, d) : configA centre nASV' total pathinfo
               where theta = atan ( snd centre / fst centre)
                     pWidth = pull1 pathinfo
                     bRad = (49/pWidth)*(total - 1)**2 / 1000
                     delta = bRad / pWidth
                     dRad = (bRad - pWidth)/num
                     minAngle = calcAngle num delta
                     mult = total - nASV
                     mult' = num - nASV
                     a = fst centre - ((pWidth + mult*dRad) * (sin (minAngle**mult))) / (sin theta)
                     b = snd centre - ((pWidth + mult*dRad) * (cos (minAngle**mult))) / (cos theta)
                     c = fst centre + ((pWidth + mult'*dRad) * (sin (minAngle**mult'))) / (sin theta)
                     d = snd centre - ((pWidth + mult'*dRad) * (cos (minAngle**mult))) / (cos theta)
                     num = total / 2
                     nASV' = nASV - 1

configB :: (Float, Float) -> Float -> Float -> (Float, Float, Float) -> [(Float, Float)]
configB centre nASV total pathinfo
            | nASV < 2           = [(a, b)]
            | nASV > (total/2)   = (a, b) : configB centre nASV' total pathinfo
            | otherwise          = (c, d) : configB centre nASV' total pathinfo
               where theta = atan ( snd centre / fst centre)
                     pWidth = pull1 pathinfo
                     bRad = (49/pWidth)*(total - 1)**2 / 1000
                     delta = bRad / pWidth
                     dRad = (bRad - pWidth)/num
                     minAngle = calcAngle num delta
                     mult = total - nASV
                     mult' = num - nASV
                     a = fst centre + ((pWidth + mult*dRad) * (sin (minAngle**mult))) * (sin theta)
                     b = snd centre - ((pWidth + mult*dRad) * (cos (minAngle**mult))) * (cos theta)
                     c = fst centre + ((pWidth + mult'*dRad) * (sin (minAngle**mult'))) * (sin theta)
                     d = snd centre - ((pWidth + mult'*dRad) * (cos (minAngle**mult))) * (cos theta)
                     num = total / 2
                     nASV' = nASV - 1

configC :: (Float, Float) -> Float -> Float -> (Float, Float, Float) -> [(Float, Float)]
configC centre nASV total pathinfo
            | nASV < 2           = [(a, b)]
            | nASV > (total/2)   = (a, b) : configC centre nASV' total pathinfo
            | otherwise          = (c, d) : configC centre nASV' total pathinfo
               where theta = atan ( snd centre / fst centre)
                     pWidth = pull1 pathinfo
                     bRad = (49/pWidth)*(total - 1)**2 / 1000
                     delta = bRad / pWidth
                     dRad = (bRad - pWidth)/num
                     minAngle = calcAngle num delta
                     mult = total - nASV
                     mult' = num - nASV
                     a = fst centre - ((pWidth + mult*dRad) * (sin (minAngle**mult))) * (sin theta)
                     b = snd centre - ((pWidth + mult*dRad) * (cos (minAngle**mult))) * (cos theta)
                     c = fst centre + ((pWidth + mult'*dRad) * (sin (minAngle**mult'))) * (sin theta)
                     d = snd centre - ((pWidth + mult'*dRad) * (cos (minAngle**mult))) * (cos theta)
                     num = total / 2
                     nASV' = nASV - 1

configD :: (Float, Float) -> Float -> Float -> (Float, Float, Float) -> [(Float, Float)]
configD centre nASV total pathinfo
            | nASV < 2           = [(a, b)]
            | nASV > num         = (a, b) : configD centre nASV' total pathinfo
            | otherwise          = (c, d) : configD centre nASV' total pathinfo
               where theta = 90 - atan ( abs (pull3 pathinfo))
                     alpha = dist2ASV (0.0, 0.0) centre
                     pWidth = pull1 pathinfo
                     bRad = (0.000049/pWidth)*((total - 1)**2)
                     delta = (\x -> if x > 1 then x else 1/x) (bRad / pWidth)
                     delta' = bRad / pWidth
                     dRad = abs(bRad - pWidth)/(num - 1)
                     minAngle = calcAngle total delta
                     refAngleCos = (\x -> if x > 0 then 1 else 0) mult * (90 - angleCreep minAngle delta' mult)
                     refAngleSin = (\x -> if x > 0 then 1 else 3.14) mult * (90 - angleCreep minAngle delta' mult)
                     refAngleCos' = (\x -> if x > 0 then 1 else 0) mult' * (90 - angleCreep minAngle delta' mult')
                     refAngleSin' = (\x -> if x > 0 then 1 else 3.14) mult' * (90 - angleCreep minAngle delta' mult')
                     mult = total - nASV
                     mult' = nASV
                     a = (fst centre + pWidth +
                                       (mult*dRad) *
                                       (cos refAngleCos)) *
                                       (cos theta)
                     b = (snd centre + pWidth +
                                        (mult*dRad) *
                                        (sin refAngleSin)) *
                                        (sin theta)
                     c = (fst centre + pWidth +
                                       (mult'*dRad) *
                                       (cos refAngleCos')) *
                                       (sin theta)
                     d = (snd centre - pWidth +
                                        (mult'*dRad) *
                                        (sin refAngleSin')) *
                                        (cos theta)
                     a' = fst centre + bRad * (cos theta)
                     b' = snd centre - bRad * (sin theta)
                     num = total / 2
                     nASV' = nASV - 1

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
