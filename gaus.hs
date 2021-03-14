type Row    = [Double]
type Matrix = [Row]

swap :: Matrix -> Int -> Int -> Matrix
swap matrix n1 n2 = map row [0..n]
                    where n=length matrix - 1
                          row k | k==n1 = matrix !! n2
                                | k==n2 = matrix !! n1
                                | otherwise = matrix !! k

comb :: Row -> Row -> Double -> Row
comb r1 r2 f = zipWith (\ x y -> x+f*y) r1 r2

trans :: Matrix -> Int -> Int -> Double -> Matrix
trans matrix n1 n2 f = map row [0..n]
                       where n=length matrix - 1
                             row k | k==n1 = comb (matrix !! n1) (matrix !! n2) f
                                   | otherwise = matrix !! k

getNz :: Row -> Int
getNz xs = if length tmp == 0 then (-1) else snd $ head tmp
           where tmp=dropWhile (\ (x,k) -> (abs x) <= 1.0e-10) $ zip xs [0..]

search :: Matrix -> Int -> Matrix
search matrix k | (abs ((matrix !! k) !! k)) > 1.0e-10 = matrix
                | nz < 0 = matrix  -- матрица вырождена    
                | otherwise = swap matrix k p 
                           where n   = length matrix
                                 lst = map (\ r -> r !! k) $ drop k matrix
                                 nz  = getNz lst
                                 p   = k + nz

mkzero :: Matrix -> Int -> Int -> Matrix
mkzero matrix k p | p>n-1 = matrix
                  | otherwise = mkzero (trans matrix p k (-f)) k (p+1)
                    where n = length matrix
                          f = ((matrix !! p) !! k)/((matrix !! k) !! k)

triangle :: Matrix -> Int -> Matrix
triangle matrix k | k>=n = matrix
                  | (abs v) <= 1.0e-10 = [[0.0]] -- матрица вырождена
                  | otherwise = triangle (mkzero tmp k k1) k1 
                    where n   = length matrix
                          tmp = search matrix k
                          v   = (tmp !! k) !! k -- диагональный элемент
                          k1  = k+1

gauss :: Matrix -> Matrix
gauss matrix = triangle matrix 0 

proddiag :: Matrix -> Double
proddiag matrix = product $ map (\ (r,k) -> r !!k) $ zip matrix [0,1..]

det :: Matrix -> Double
det matrix = proddiag $ triangle matrix 0

main = print $ det  [[1,2,3],[4,5,6],[7,8,-9]]