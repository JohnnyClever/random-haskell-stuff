shoelace :: [(Double, Double)] -> Double
shoelace =
  let calcSums ((xi, yi), (nxi, nyi)) (l, r) = (l + xi * nyi, r + nxi * yi)
  in (/ 2) .
     abs . uncurry (-) . foldr calcSums (0, 0) . (<*>) zip (tail . cycle)