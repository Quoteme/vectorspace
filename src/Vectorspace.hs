module Vectorspace where

import Data.Complex
import Data.Maybe

newtype Vector a = Vector [a] deriving (Eq)
newtype Matrix a = Matrix [[a]] deriving (Eq)

instance Show a => Show (Vector a) where
        show (Vector v)
            | length v == 0 = "()"
            | length v == 1 = "(" ++ show (v!!0) ++ ")"
            | length v == 2 = start v ++ end v
            | length v >= 3 = start v ++ center v ++ end v where
                start v = vectortop $ (show.head) v
                center v = concatMap (vectorcenter.show) ( (init.tail) v)
                end   v = vectorbottom $ (show.last) v
                vectortop x = "/" ++ x ++ "\\\n"
                vectorcenter x = "|" ++ x ++ "|\n"
                vectorbottom x = "\\" ++ x ++ "/"

instance Show a => Show (Matrix a) where
        show (Matrix m) = start m ++ "\n" ++ center m ++ "\n" ++ end m where
            start m  = (matrixtop.concat) [ cell v | v <- head m ]
            center m = (matrixcenter.concat) [ cell v | v <- (head.init.tail) m ]
            end   m  = (matrixbottom.concat) [ cell v | v <- last m ]
            matrixtop v    = "┌" ++ v ++ "┐"
            matrixcenter v = "│" ++ v ++ "│"
            matrixbottom v = "└" ++ v ++ "┘"
            cell v = [ if i< length (show v) then (show v)!!i else ' ' | i <- [0..cellWidth] ]
            cellWidth =  maximum $ map (length.show) $ concat m

instance Floating a => Num (Vector a) where
        -- componentwise addition
        (Vector v) + (Vector w)
                            | length v == length w = Vector $ zipWith (+) v w
                            | length v >  length w = Vector v + Vector (take (length v) (w++repeat 0))
                            | length v <  length w = Vector w + Vector v
        (Vector v) * (Vector w) = Vector $ zipWith (*) v w
        negate (Vector v) = Vector $ map (*(-1)) v
        abs (Vector v) = Vector [sqrt $ sum (map (**2) v)]
        signum (Vector v) = Vector $ map signum v
        fromInteger n = Vector [fromInteger n]

-- multiplies a vector by a scalar
vecmul :: Integer -> [Integer] -> [Integer]
vecmul a b = map (*a) b

-- multiplies a matrix by a scalar
matscalmult :: Num a => [[a]] -> a -> [[a]]
matscalmult m s = map (\y -> map (\x -> (m!!y!!x)*s) [0..(length (m!!y))-1]) [0..(length m)-1]

-- multiplies / composes two matrices
matmult :: Num a => [[a]] -> [[a]] -> [[a]]
matmult m n
    | length m > length n       = matmult m (scale n (length m))
    | length m /= length (m!!0) = matmult (sqrify m) n
    | length n /= length (n!!0) = matmult m (sqrify n)
    | otherwise                 = [[
                                        sum (zipWith (*) (getColMat m y) (getRowMat n x))
                                    | x<-[0..((length (m!!y))-1)] ]
                                | y<-[0..((length m)-1)]]

-- multiplies a matrix by a vector / applies a matrix to a vector
matvecmult :: Num a => [[a]] -> [a] -> [a]
matvecmult m v = map (\e-> head e) $ matmult m (map (\e-> [e]) v)

applyMatrix :: Num a => [[a]] -> [a] -> [a]
applyMatrix m v = map (\s -> sum (zipWith (*) v s)) m

-- adds two matrices together
matadd :: Num a => [[a]] -> [[a]] -> [[a]]
matadd m n
    | length m > length n       = matadd m (scale n (length m))
    | length m /= length (m!!0) = matadd (sqrify m) n
    | length n /= length (n!!0) = matadd m (sqrify n)
    | otherwise                 = [[
                                         (m!!y!!x) + (n!!y!!x)
                                    | x <- [0..((length(m!!0))-1)]]
                                | y <- [0..((length m)-1)]]

-- subtracts one matrix from another
matsub :: Num a => [[a]] -> [[a]] -> [[a]]
matsub m n = matadd (m) (matscalmult n (-1))

-- creates an identity matrix of dimension n
matid :: Num a => Int -> [[a]]
matid n = matgen n (\i -> (\j -> if(i==j)then(1)else(0)))

-- maps a matrices entries based on a function that takes the indices as input
matmap :: Num a => [[a]] -> ( Int -> Int -> a -> a ) -> [[a]]
matmap m f = map (\y -> map (\x -> f y x (m!!y!!x) ) [0..((length (m!!y))-1)]) [0..((length m)-1)]

-- complex conjugat of a matrix
matcon :: RealFloat a => [[Complex a]] -> [[Complex a]]
matcon m = matmap m (\y x a -> conjugate a)

-- generates a matrix of size t and then maps it with values
matgen :: (Num a, Num t, Enum t) => t -> (t -> t -> a) -> [[a]]
matgen s f = [[
             f x y
        | x<- [0..(s-1)]]
    | y<- [0..(s-1)]]

-- fills in entries with 0's, so that a matrix is square
sqrify :: Num a => [[a]] -> [[a]]
sqrify m = scale m (max (length m) (length (m!!0)))

-- scales a matrix m to a size s (also makes it a square matrix
scale :: Num a => [[a]] -> Int -> [[a]]
scale m s = [[ if (y<(length m) && x<(length (m!!0))) then(m!!y!!x) else(0) | x <- [0..(s-1)] ] | y <- [0..(s-1)] ]

-- pretty prints a matrix
printMat :: (Foldable t, Show a) => t a -> IO ()
printMat m = mapM_ print m

-- all non zero elements of a matrix are replaces by '.', otherwise they are replaced by ' '
prettifyMat m = [[ if(m!!y!!x == 0)then(' ')else('.') | x<-[0..(length (m!!y)-1)] ] | y<-[0..((length m)-1)]]

indexMatrix :: (Num a, Num b,Enum b)  => [[a]] -> [([(a,b)], b)]
indexMatrix m = (map (\i -> zip i [0..]) m) `zip` [0..]

-- gets the element of the y th row and x th collumn of a matrix
matxy :: Num a => [[a]] -> Int -> Int -> a
matxy m x y = m!!x!!y

-- transposes a matrix
transpose :: Num a => [[a]] -> [[a]]
transpose m = do
    let im = indexMatrix m
    map (\i -> map (\j -> matxy m (snd j) (snd i)) (fst i)) im

-- transposes a vector
vectrans :: Num a => [a] -> [[a]]
vectrans v = [ [x] | x<-v ]

-- adjugate matrix of m
adju :: Num a => [[a]] -> [[a]]
adju m = transpose (cof m)

-- cut out the matrix from x1-x2 and y1-y2 of a matrix m
submatrix :: Num a => [[a]] -> Int -> Int -> Int -> Int -> [[a]]
submatrix m x1 y1 x2 y2 = do
    let ycut = take (y2-y1) (drop y1 m)
    map (\i -> take (x2-x1) (drop x1 i)) ycut

-- gets the elements of a matrix in row r
getRowMat :: Num a => [[a]] -> Int -> [a]
getRowMat m r = [ m!!r!!x | x <- [0..((length (m!!0))-1)]]

-- gets the elements of a matrix in collumn c
getColMat :: Num a => [[a]] -> Int -> [a]
getColMat m c = [ m!!x!!c | x <- [0..((length m)-1)]]

-- return the matrix with collumn c removed
elimRowMat :: Num a => [[a]] -> Int -> [[a]]
elimRowMat m r = map (\i -> take r i ++ drop (r+1) i) m

-- returns the matrix with row r removed
elimColMat :: Num a => [[a]] -> Int -> [[a]]
elimColMat m c = take c m ++ drop (c+1) m

-- eliminates row y and collumn x
elimRowColMat :: (Num a) => [[a]] -> Int -> Int -> [[a]]
elimRowColMat m x y = elimRowMat (elimColMat m y) x

-- takes a matrix and replaces the n th collumn with a vector
replCol :: Num a => [[a]] -> Int -> [a] -> [[a]]
replCol m i v = map (\y -> map (\x -> if(x==i && y<length v)then(v!!y)else(m!!y!!x)) [0..((length (m!!0))-1)]) [0..((length m)-1)]

-- takes a matrix and replaces the n th row with a vector
replRow :: Num a => [[a]] -> Int -> [a] -> [[a]]
replRow m i v = map (\y -> map (\x -> if(y==i && x<length v)then(v!!x)else(m!!y!!x)) [0..((length (m!!0))-1)]) [0..((length m)-1)]

-- calculates the determinant of a matrix
det :: Num a => [[a]] -> a
det m = case (length m, length (m!!0)) of
    (1,1) -> m!!0!!0
    (p,q) -> do
        let grid = [ (m!!0!!x)*(-1)^x | x<-[0..p-1] ]
        let underdet = [ (grid!!x) * det(elimRowColMat m x 0) | x<-[0..p-1]]
        sum underdet

-- returns the minor of a matrix from row x and col y
minor :: Num a => [[a]] -> Int -> Int -> a
minor m x y = det (elimRowColMat m x y)

-- cofactor matrix / i.e. replace each entry with their minor and apply the checkerboard pattern of (1) and (-1)
cof :: Num a => [[a]] -> [[a]]
cof m = [
        [
            ((-1)^(x+y)) * (minor m x y)
        | x<-[0..(length (m!!y))-1]]
    | y<-[0..(length m)-1]]

-- returns if a matrix is invertible or nor
invertible :: (Eq a, Num a) => [[a]]-> Bool
invertible m = (det m) /= 0

-- returns the inverse of a matrix, if it exists
inv :: (Eq a, Fractional a) => [[a]] -> Maybe [[a]]
inv m
    | not (invertible m) = Nothing
    | otherwise = Just (matscalmult (adju m) (1/det(m)))

-- removes duplicates inside a list
remDupl :: (Eq a) => [a] -> [a]
remDupl a
    | a == [] = []
    | not (elem (a!!0) (drop 1 a)) = (a!!0):(remDupl(drop 1 a))
    | otherwise = remDupl (drop 1 a)

-- true if list a and list b are linearily dependent
linDep :: (Fractional a, Eq a, Enum a) => [a] -> [a] -> Bool
linDep a b
    | length a < length b   = linDep b a
    | length a == 0         = True
    | a!!0==b!!0 && a!!0==0  = linDep (tail a) (tail b)
    | a!!0/=0 && b!!0/=0    = all (\e -> (fst e)/b!!0 == (snd e)/a!!0) $ zip a (b++[0,0..])
    | otherwise             = False

-- adds a collumn of 0 at the beginning
addCol :: Num a => [[a]] -> [[a]]
addCol m = map (\e -> 0:e) m

-- reduced row-echelon form (TODO)
rref :: (Fractional a, Eq a) => [[a]] -> [[a]]
rref m = rref_step 0 m

rref_step :: (Fractional a, Eq a) => Int -> [[a]] -> [[a]]
rref_step s m
    | coef s == 0                               = rref_step (s+1) m
    | length (newPivot (normalize s) s) == 0    = normalize s
    | s+1 < length m                            = rref_step (s+1) (elim s)
    | otherwise                                 = normalize s
    where
        coef s          = length ( filter (\i-> i/=0) (getRowMat m s) )
        ones s          = length ( filter (\i-> i==1) (getRowMat m s) )
        emptyUntil s r  = 0 == length ( filter (\i-> i/=0) (take s (getColMat m r)))
        normalize s     = map (\y-> if(m!!y!!s /= 0 && emptyUntil s y)then(map (/m!!y!!s) (m!!y))else(m!!y) ) [0..((length m)-1)]
        newPivot n s    = (take 1 (filter (\y-> (emptyUntil s y) && (n!!y!!s==1)) [0..((length n)-1)]))
        elim s          = do
                            let n = normalize s
                            let np = (newPivot n s)!!0
                            map (\y-> if(n!!y!!s /=0 && y/=np)then( map (\x-> (n!!y!!x)- ((n!!y!!s)*(n!!np!!x))) [0..((length (n!!0))-1)] )else( n!!y )) [0..((length n)-1)]

dimKer :: (Fractional a, Eq a) => [[a]] -> Int
dimKer m = length $ filter (\e-> 0 == (length ( filter (\i-> i/=0) e)) ) (rref m)

--rank :: (Fractional a, Eq a) => [[a]] -> Int
rank m = length m - dimKer m

ker :: (Fractional a, Eq a) => [[a]] -> Maybe [[a]]
ker m
    | det m /= 0 = Nothing
    | otherwise = do
        let r = rref m
        let f = filter (\i-> any (\j-> j/=0) i) r
        Just f

solvMat :: Fractional a => [[a]] -> [a] -> [a]
solvMat m v = map (\x-> det (replCol m x v) / det m) [0..((length (m!!0))-1)]

--jord :: Num a => [[a]] -> [[a]]
--jord m =

--charpoly :: Num a => [[a]] -> a -> a
--charpoly m x = det (matadd m (matscalmult (matid (length m)) x ))
--
--eigenVal :: (RealFrac a, Enum a, Integral b) => [[a]] -> [b]
--eigenVal m = filter (\i-> i/=0) $ remDupl $ map (round) $ filter (\x -> round (charpoly m x)>(-10) && round (charpoly m x)<10) [-20,-19.9 .. 20]
--
--geoMult :: (Fractional a, Eq a) => [[a]] -> a -> Int
--geoMult m e = dimKer $ matadd (matscalmult (matid (length (sqrify m))) e) (matscalmult m (-1))

tr :: Num a => [[a]] -> a
tr m = sum $ map (\i -> (sqrify m)!!i!!i) [0..((length (m!!0))-1)]

polyadd :: Num a => [a] -> [a] -> [a]
polyadd p q
    | length p < length q = polyadd q p
    | otherwise = [ p!!x + if(x<length q)then(q!!x)else(0) | x<- [0..((length p)-1)]]

polymonomult :: Num a => [a] -> (a,Int) -> [a]
polymonomult p m = [0| x<-[1..(snd m)]] ++ map (* (fst m)) p

--polymult :: Num a => [a] -> [a] -> [a]
polymult p q = finish $ map (\i -> polymonomult q (p!!i,i) ) [0..((length p)-1)]
    where
        finish x
            | length x == 1 = x
            | otherwise = finish $ polyadd (x!!0) (x!!1) : tail (tail x)

--jnf :: Num a => [[a,Int,Int]] => [[a]]

--algMult ::
--algMult m es =
--    where s = length (sqrify m)
--
--partitions :: Int -> [Int]
--partitions x =

dotprod :: Num a => [a] -> [a] -> a
dotprod v w = sum $ matvecmult (vectrans v) w

orthoVec :: (Num a, Eq a) => [a] -> [a] -> Bool
orthoVec v w = (dotprod v w) == 0

adjo :: RealFloat a => [[Complex a]] -> [[Complex a]]
adjo m = transpose $ matcon m

ortho :: (Num a, Eq a) => [[a]] -> Bool
ortho m = matid (length (sqrify m)) == matmult m (transpose m)

hermit :: RealFloat a => [[Complex a]] -> Bool
hermit m = adjo m == m

unitar :: RealFloat a => [[Complex a]] -> Bool
unitar m = adjo m == fromMaybe [[0]] (inv m)

kroneck :: Num a => [[a]] -> [[a]] -> [[a]]
kroneck m n = [
            [ (m!!( yPosM y )!!( xPosM x )) * (n!!( yPosN y )!!( xPosN x ))
        | x<-[0..((length (m!!0))*(length (n!!0))-1)]]
    | y<-[0..((length m)*(length n)-1)]]
    where
        yPosM y = y `mod` (length m)
        xPosM x = x `mod` (length (m!!0))
        yPosN y = if (y==0)then(0)else( y `div` (length m) )
        xPosN x = if (x==0)then(0)else( x `div` (length (m!!0)) )
