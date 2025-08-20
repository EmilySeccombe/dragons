#Find exponents:

as_percentage_change <- function(coefficient){
  exponent <- exp(coefficient)
  result <- (100 * (exponent -1))
  return(result)
}

labels <- c("C.v pH", "C.s pH", "C.s N", "L.f TW", "L.f pH", "L.f O.D", "L.f N", "C.b TW", "C.b pH", "C.b N", "C.b 25", "P.p pH", "P.p O.D", "G.v BOD", "G.v pH", "G.v SS")
values <- c(-0.1310987, 0.3516652, 0.1935793, 0.2299029, 0.111091, -0.209301, 0.105463, 0.195055, -0.2448382, -0.2565875, -3.798768, 0.5011964, 0.1474047, 0.02718557, 0.388231, 0.02507896)
results <- data.frame(labels, values)

results$perc_change <- as_percentage_change(results$values)

# 
#Calopteryx virgo with chemical: pH
# exp(-0.1310987)
# 
# #Calopteryx splendens with chemical: pH
# exp(0.3516652)
# 
# #Calopteryx splendens with chemical: N Oxidised
# exp(0.1935793)
# 
# # Libellula fulva with chemical: Temp Water
# exp(0.2299029)
# 
# # Libellula fulva with chemical: pH
# exp(0.111091)
# 
# # Libellula fulva with chemical: O Diss %sat 
# exp(-0.209301)
# 
# # Libellula fulva with chemical: N Oxidised
# exp(0.105463)
# 
# # Cordulegaster boltonii with chemical: Temp Water
# exp(0.195055)
# 
# # Cordulegaster boltonii with chemical: pH 
# exp(-0.2448382)
# 
# # Cordulegaster boltonii with chemical: N Oxidised 
# exp(-0.2565875)
# 
# # Cordulegaster boltonii with chemical: Cond @ 25C
# exp(-3.798768)
# 
# # Platycnemis pennipes with chemical: pH 
# exp(0.5011964)
# 
# # Platycnemis pennipes with chemical: O Diss %sat 
# exp(0.1474047)
# 
# # Gomphus vulgatissimus with chemical: BOD ATU
# exp(0.02718557)
# 
# # Gomphus vulgatissimus with chemical: pH 
# exp(0.388231)
# 
# # Gomphus vulgatissimus with chemical: Sld Sus@105C
# exp(0.02507896)
# 
