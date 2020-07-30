import qualified Data.Map as Map
import qualified Data.Sequence as Sequence

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

solvedCube :: Cube
solvedCube = Cube (Side 'g' 'g' 'g' 'g') (Side 'r' 'r' 'r' 'r') (Side 'w' 'w' 'w' 'w') (Side 'b' 'b' 'b' 'b') (Side 'o' 'o' 'o' 'o') (Side 'y' 'y' 'y' 'y')
-- solvedCube = Cube (Side 'o' 'o' 'o' 'o') (Side 'b' 'b' 'b' 'b') (Side 'y' 'y' 'y' 'y') (Side 'r' 'r' 'r' 'r') (Side 'g' 'g' 'g' 'g') (Side 'w' 'w' 'w' 'w')

xCube :: Cube 
xCube = let s = Side 'x' 'x' 'x' 'x' in Cube s s s s s s 

isOneColor :: Side -> Bool
isOneColor (Side a b c d)
    | (a==b) && (a==c) && (a==d) = True
    | otherwise = False

isSolved :: Cube -> Bool
isSolved (Cube a b c d e f) 
    | map isOneColor [a, b, c, d, e, f] == [True, True, True, True, True, True] = True
    | otherwise = False


allRotations :: [Cube -> Cube]
allRotations = [rotU, rotUr, rotF, rotFr, rotR, rotRr]

allRotationsString :: [String]
allRotationsString = ["U", "U'", "F", "F'", "R", "R'"]

allRotationsTuple :: [(Cube -> Cube, String)]
allRotationsTuple = zip allRotations allRotationsString

findShortest :: Int -> Cube -> Maybe String
findShortest (-1) _ = Nothing
findShortest n cube = if prev == Nothing then dfs n cube else prev
    where prev = findShortest (n-1) cube

dfs :: Int -> Cube -> Maybe String
dfs n cube 
    | cube == solvedCube = Just ""
    | n <= 0 = Nothing
    | otherwise = firstValid allCubes
    where
        allCubes = map joinSequence $ zip allRotationsString (map (getPerm (n-1) Map.empty) $ map ($ cube) allRotations)

getPerm :: Int -> Map.Map Cube String -> Cube -> Maybe String
getPerm n m cube 
    | l == Nothing = dfs n cube
    | otherwise = l
    where l = Map.lookup cube m

firstValid :: [Maybe String] -> Maybe String
firstValid [] = Nothing
firstValid (Nothing:rest) = firstValid rest
firstValid (Just s:_) = Just s

joinSequence :: (String, Maybe String) -> Maybe String
joinSequence (a, Just b) = Just (a ++ b)
joinSequence (_, Nothing) = Nothing

-------------
solve2 :: Cube -> Int
solve2 cube = Map.size (dfs2 7 Map.empty cube)

dfs2 :: Int -> Map.Map Cube String -> Cube -> Map.Map Cube String
dfs2 n m cube
    | cube == solvedCube = Map.insert cube "" m
    | n <= 0 = m
    | otherwise = if Map.lookup cube prevM /= Nothing 
        then prevM 
        else let str = scrambleString allRotationsTuple cube prevM in
            if str == "cut"
            then m
            else Map.insert cube str prevM
        where prevM = foldr (\rotation mm -> getPerm2 n mm (rotation cube)) m allRotations

getPerm2 :: Int -> Map.Map Cube String -> Cube -> Map.Map Cube String
getPerm2 n m cube 
    | Map.lookup cube m /= Nothing = m
    | otherwise = dfs2 (n-1) m cube

scrambleString :: [(Cube -> Cube, String)] -> Cube -> Map.Map Cube String -> String
scrambleString [] _ _ = "cut"
scrambleString ((rotation, s):rest) cube m = let res = Map.lookup (rotation cube) m in 
    if res == Nothing 
    then scrambleString rest cube m
    else let Just a = res in s ++ a 

------------
dfs3 :: Int -> Map.Map Cube String -> Cube -> String -> Map.Map Cube String
dfs3 0 m cube sl = Map.insert cube sl m
dfs3 n m cube sl = Map.insert cube sl prevM
        where prevM = foldr (\(rotation, rotStr) mm -> getPerm3 n mm (rotation cube) (rotStr ++ sl)) m allRotationsTuple

getPerm3 :: Int -> Map.Map Cube String -> Cube -> String -> Map.Map Cube String
getPerm3 n m cube sl
    | Map.member cube m = let Just str = Map.lookup cube m in 
        if (length str) > (length sl)
        then dfs3 (n-1) m cube sl
        else m
    | otherwise = dfs3 (n-1) m cube sl

bfs :: Sequence.Seq (Cube, [String]) -> Map.Map Cube [String] -> Map.Map Cube [String]
bfs queue m 
    | qEmpty queue = m
    -- | let Just (cube, str) = qFirst queue in (length str) > 30 = m
    | otherwise = let Just (cube, str) = qFirst queue in 
        if isSolved cube
        then Map.insert xCube str m
        else
            if Map.member cube m
            then bfs (qPop queue) m
            else let nQueue = foldr (\(rot, rotStr) q -> qPush ((rot cube), ([rotStr] ++ str)) q) (qPop queue) allRotationsTuple in
                bfs nQueue (Map.insert cube str m)

solve :: Cube -> [String]
solve cube 
    | res == Nothing = error "invalid cube"
    | otherwise = let Just s = res in reverse s
    where res = Map.lookup xCube $ bfs (Sequence.singleton (cube, [])) Map.empty


-- (Cube (Side 'w' 'y' 'g' 'g') (Side 'b' 'r' 'b' 'w') (Side 'g' 'y' 'r' 'r') (Side 'g' 'w' 'b' 'w') (Side 'o' 'b' 'o' 'o') (Side 'y' 'r' 'o' 'y'))
-- (Cube (Side 'y' 'g' 'w' 'y') (Side 'r' 'b' 'o' 'b') (Side 'y' 'w' 'w' 'b') (Side 'o' 'r' 'b' 'w') (Side 'g' 'r' 'g' 'o') (Side 'o' 'r' 'g' 'y'))
-- (Cube (Side 'b' 'r' 'b' 'g') (Side 'g' 'o' 'r' 'r') (Side 'g' 'b' 'y' 'r') (Side 'w' 'r' 'b' 'o') (Side 'w' 'i' 'w' 'o') (Side 'o' 'w' 'g' 'i'))
-- (Cube (Side 'y' 'r' 'y' 'g') (Side 'y' 'r' 'r' 'o') (Side 'r' 'y' 'b' 'o') (Side 'g' 'b' 'w' 'g') (Side 'w' 'b' 'w' 'o') (Side 'o' 'g' 'w' 'b'))

allScrambles :: Int -> [Cube]
allScrambles 0 = [solvedCube]
allScrambles n = foldr (\cube list -> insertWithoutDuplicates (map ($ cube) allRotations) list) [] prevScrambles
    where prevScrambles = allScrambles (n-1)

insertWithoutDuplicates :: Eq a => [a] -> [a] -> [a]
insertWithoutDuplicates [] list = list
insertWithoutDuplicates (a:rest) list
    | elem a list = l
    | otherwise = (a:l)
    where l = insertWithoutDuplicates rest list

main = print $ solve (Cube (Side 'y' 'r' 'y' 'g') (Side 'y' 'r' 'r' 'o') (Side 'r' 'y' 'b' 'o') (Side 'g' 'b' 'w' 'g') (Side 'w' 'b' 'w' 'o') (Side 'o' 'g' 'w' 'b'))
