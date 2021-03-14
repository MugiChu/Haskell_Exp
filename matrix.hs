type Row    = [Double]
type Matrix = [Row]

deln :: Matrix -> Int -> Matrix
deln matrix k = map (\ r -> (take (k) r)++(drop (k+1) r)) matrix

minor :: Matrix -> Int -> Double
minor matrix k = det $ deln (drop 1 matrix) k

sgn :: Int -> Double
sgn n = if n `rem` 2 == 0 then 1.0 else (-1.0)

det :: Matrix -> Double
det [[a,b],[c,d]] = a*d-b*c
det matrix = sum $ map (\c -> ((matrix !! 0)!!c)*(sgn c)*(minor matrix c))  [0..n]
             where n = length matrix - 1

main = print $ det [[1,2,3],[4,5,6],[7,8,(-9)]]