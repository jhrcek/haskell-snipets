import Graphics.Gloss

sceneWidth, sceneHeight :: Float
sceneWidth = 800
sceneHeight = 600

pascalTriangle :: [[Float]]
pascalTriangle = iterate next [1]
  where next xs = zipWith (+) (0 : xs) (xs ++ [0])

drawPascalLevel :: [Float] -> Picture
drawPascalLevel lvl = Pictures $ zipWith shift [0..] (map barPicture lvl)
  where
    shift barIndex pic = translate (barIndex * barWidth - (sceneWidth / 2)) (- sceneHeight / 2) pic
    barPicture n = polygon [(0,0), (barWidth, 0), (barWidth, barHeight n), (0, barHeight n)] :: Picture
    barHeight n = let h = (n / maximum lvl) * sceneHeight in if isNaN h then 0 else h :: Float
    barWidth = sceneWidth / fromIntegral (length lvl) :: Float

animation :: Float -> Picture
animation time = drawPascalLevel (pascalTriangle !! (round (10*time)))

main = animate win white animation
      --display win white (drawPascalLevel (pascalTriangle !! 10))
  where win = InWindow "Pascal levels" (round sceneWidth, round sceneHeight) (10, 10)
