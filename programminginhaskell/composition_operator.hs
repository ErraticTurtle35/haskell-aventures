sumsqreven ns = sum (map (^2) filter even ns)
sumsqreven ns = sum . map (^2) . filter even