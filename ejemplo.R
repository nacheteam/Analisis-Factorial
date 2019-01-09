# Initialize covariance matrix
cov_matrix <- Harman74.cor$cov
library(psych)
library(GPArotation)
library(cfa)
# Usado para los datos en CFA y la sintaxis lavaan
library(lavaan)

##                                 EFA                                       ##

###############################################################################
##                         Number of factors                                 ##
###############################################################################

VSS(sim.item(nvar=24),n=8,fm="minres" ,title="VSS of 24 simple structure variables")

###############################################################################
##                               With rotation                               ##
###############################################################################
# Principal Axis
pa_rotated <- fa(cov_matrix, 4, fm="pa", rotate="varimax")
# Unweighted least squares is minres
uls_rotated <- fa(cov_matrix, 4, rotate = "varimax")
# Weighted least squares
wls_rotated <- fa(cov_matrix, 4, fm = "wls")

###############################################################################
##                       Maximumm likelihood                                ##
###############################################################################

mle_rotated <- factanal(covmat = cov_matrix, factors = 4)

###############################################################################
##                              No rotation                                  ##
###############################################################################
# Weighted least squares
wls_nonrotated <- fa(cov_matrix,4 , rotate = "none", fm="wls")
# Principal Axis
pa_nonrotated <- fa(cov_matrix, 4, rotate = "none", fm="pa")
# Minres
minres_nonrotated <- factanal(factors=4,covmat=cov_matrix,rotation="none")
# Maximum likelihood
mle_nonrotated <- fa(cov_matrix, 4, rotate = "none", fm="mle")
# Unweighted least squares
uls_nonrotated <- fa(cov_matrix, 4, rotate = "none", fm="uls")

cat("\n\n\n          ----------------\n          | CON ROTACION |\n          ----------------\n\n\n")
cat("Resultados de principal axis con rotacion:\n\n")
print(pa_rotated)
cat("\n\nResultados de unweighted least squares con rotacion:\n\n")
print(uls_rotated)
cat("\n\nResultados de weighted least squares con rotacion:\n\n")
print(wls_rotated)
cat("\n\nResultados de maximum likelihood con rotacion:\n\n")
print(mle_rotated)

cat("\n\n\n          ----------------\n          | SIN ROTACION |\n          ----------------\n\n\n")
cat("Resultados de weighted least squares sin rotacion:\n\n")
print(wls_nonrotated)
cat("Resultados de principal axis sin rotacion:\n\n")
print(pa_nonrotated)
cat("Resultados de minres sin rotacion:\n\n")
print(minres_nonrotated)
cat("Resultados de maximum likelihood sin rotacion:\n\n")
print(mle_nonrotated)
cat("Resultados de unweighted least squares sin rotacion:\n\n")
print(uls_nonrotated)

##                                 CFA                                       ##
# Hacemos nuestra hipótesis del modelo
HS.model <- ' visual  =~ x1 + x2 + x3      
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

# Comprobamos la hipótesis
fit <- cfa(HS.model, data=HolzingerSwineford1939)

# Comprobamos los datos sobre el ajuste
summary(fit, fit.measures=TRUE)