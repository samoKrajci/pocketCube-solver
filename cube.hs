import qualified Data.Map as Map
import qualified Data.Sequence as Sequence

-- queue
qPush :: a -> Sequence.Seq a -> Sequence.Seq a
qPush a s = s Sequence.:|> a

qFirst :: Sequence.Seq a -> Maybe a
qFirst s = Sequence.lookup 0 s 

qPop :: Sequence.Seq a -> Sequence.Seq a
qPop s = Sequence.drop 1 s

qEmpty :: Sequence.Seq a -> Bool
qEmpty s = Sequence.null s

-- order: ul ur dr dl
data Side = Side Char Char Char Char
    deriving (Eq, Show, Ord)

-- order: front right up back left down
data Cube = Cube Side Side Side Side Side Side
    deriving (Eq, Show, Ord)

-- cube map in format:
-- UU
-- UU
-- FFRRBBLL
-- FFRRBBLL
-- DD
-- DD
data CubeMap = CubeMap String String String String String String
    deriving (Eq, Show, Ord)

rotF :: Cube -> Cube
rotF (Cube 
    front@(Side ful fur fdr fdl)
    right@(Side rul rur rdr rdl)
    up@(Side uul uur udr udl) 
    back 
    left@(Side lul lur ldr ldl)
    down@(Side dul dur ddr ddl)
    ) = Cube
        (Side fdl ful fur fdr)
        (Side udl rur rdr udr)
        (Side uul uur lur ldr)
        back
        (Side lul dul dur ldl)
        (Side rdl rul ddr ddl)

rotFr :: Cube -> Cube
rotFr c = rotF $ rotF $ rotF c

rotR :: Cube -> Cube
rotR (Cube 
    front@(Side ful fur fdr fdl)
    right@(Side rul rur rdr rdl)
    up@(Side uul uur udr udl) 
    back@(Side bul bur bdr bdl)
    left
    down@(Side dul dur ddr ddl)
    ) = Cube
        (Side ful dur ddr fdl)
        (Side rdl rul rur rdr)
        (Side uul fur fdr udl)
        (Side udr bur bdr uur)
        left
        (Side dul bdl bul ddl)

rotRr :: Cube -> Cube
rotRr c = rotR $ rotR $ rotR c

rotU :: Cube -> Cube
rotU (Cube 
    front@(Side ful fur fdr fdl)
    right@(Side rul rur rdr rdl)
    up@(Side uul uur udr udl) 
    back@(Side bul bur bdr bdl)
    left@(Side lul lur ldr ldl)
    down
    ) = Cube
        (Side rul rur fdr fdl)
        (Side bul bur rdr rdl)
        (Side udl uul uur udr)
        (Side lul lur bdr bdl)
        (Side ful fur ldr ldl)
        down

rotUr :: Cube -> Cube
rotUr c = rotU $ rotU $ rotU c

-- parseInput
cubeMapToCube :: CubeMap -> Cube
cubeMapToCube (CubeMap r0 r1 r2 r3 r4 r5) =
    Cube
    (Side (r2!!0) (r2!!1) (r3!!1) (r3!!0))
    (Side (r2!!2) (r2!!3) (r3!!3) (r3!!2))
    (Side (r0!!0) (r0!!1) (r1!!1) (r1!!0))
    (Side (r2!!4) (r2!!5) (r3!!5) (r3!!4))
    (Side (r2!!6) (r2!!7) (r3!!7) (r3!!6))
    (Side (r4!!0) (r4!!1) (r5!!1) (r5!!0))

solvedCube :: Cube
solvedCube = Cube (Side 'g' 'g' 'g' 'g') (Side 'r' 'r' 'r' 'r') (Side 'w' 'w' 'w' 'w') (Side 'b' 'b' 'b' 'b') (Side 'o' 'o' 'o' 'o') (Side 'y' 'y' 'y' 'y')
-- solvedCube = Cube (Side 'o' 'o' 'o' 'o') (Side 'b' 'b' 'b' 'b') (Side 'y' 'y' 'y' 'y') (Side 'r' 'r' 'r' 'r') (Side 'g' 'g' 'g' 'g') (Side 'w' 'w' 'w' 'w')

xCube :: Cube 
xCube = let s = Side 'x' 'x' 'x' 'x' in Cube s s s s s s 

isOneColor :: Side -> Bool
isOneColor (Side a b c d) = (a==b) && (a==c) && (a==d)

isSolved :: Cube -> Bool
isSolved (Cube a b c d e f) = all isOneColor [a, b, c, d, e, f]

allRotations :: [Cube -> Cube]
allRotations = [rotUr, rotU, rotFr, rotF, rotRr, rotR]

allRotationsString :: [String]
allRotationsString = ["U'", "U", "F'", "F", "R'", "R"]

allRotationsTuple :: [(Cube -> Cube, String)]
allRotationsTuple = zip allRotations allRotationsString

bfs :: Sequence.Seq (Cube, [String]) -> Map.Map Cube [String] -> Map.Map Cube [String]
bfs queue m 
    | qEmpty queue = m
    | otherwise = let Just (cube, str) = qFirst queue in 
        if isSolved cube
        then Map.insert xCube str m
        else
            if Map.member cube m
            then bfs (qPop queue) m
            else let nQueue = foldr (\(rot, rotStr) q ->  qPush ((rot cube), ([rotStr] ++ str)) q) (qPop queue) allRotationsTuple 
                in
                bfs nQueue (Map.insert cube str m)

solve :: Cube -> String
solve cube 
    | res == Nothing = error "invalid cube"
    | otherwise = let Just s = res in concatWithSpaces $ addDoubleTurns $ reverse s
    where res = Map.lookup xCube $ bfs (Sequence.singleton (cube, [])) Map.empty

addDoubleTurns :: [String] -> [String]
addDoubleTurns ar = init $ foldr (\x (y:ys) -> if x == y then ["2" ++ (take 1 y)] ++ ys else [x] ++ [y] ++ ys) [""] ar

concatWithSpaces :: [String] -> String
concatWithSpaces ar = foldr (\a b -> a ++ " " ++ b) "" ar

outputWrapper :: String -> String
outputWrapper "" = "Cube is already solved."
outputWrapper str = str

main = do
    putStrLn "Enter cube map:"  
    r0 <- getLine  
    r1 <- getLine  
    r2 <- getLine  
    r3 <- getLine  
    r4 <- getLine  
    r5 <- getLine  
    putStrLn "Calculating..."  
    let cube = cubeMapToCube (CubeMap r0 r1 r2 r3 r4 r5) in
        putStrLn $ outputWrapper $ solve cube