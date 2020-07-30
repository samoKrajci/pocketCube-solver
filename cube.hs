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


bfs :: Sequence.Seq (Cube, [String]) -> Map.Map Cube [String] -> Map.Map Cube [String]
bfs queue m 
    | qEmpty queue = m
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
-- (Cube (Side 'b' 'r' 'g' 'w') (Side 'y' 'r' 'g' 'y') (Side 'g' 'y' 'b' 'w') (Side 'g' 'r' 'b' 'o') (Side 'w' 'r' 'b' 'o') (Side 'o' 'o' 'w' 'y'))

main :: IO()
main = print $ solve (Cube (Side 'b' 'r' 'g' 'w') (Side 'y' 'r' 'g' 'y') (Side 'g' 'y' 'b' 'w') (Side 'g' 'r' 'b' 'o') (Side 'w' 'r' 'b' 'o') (Side 'o' 'o' 'w' 'y'))
