# Initialize covariance matrix
cov_matrix <- Harman74.cor$cov
library(psych)
library(GPArotation)
library(cfa)

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
pa_nonrotates <- fa(cov_matrix, 4, rotate = "none", fm="pa")
# Minres
minres_nonrotated <- factanal(factors=4,covmat=cov_matrix,rotation="none")
# Maximum likelihood
mle_nonrotated <- fa(cov_matrix, 4, rotate = "none", fm="mle")
# Unweighted least squares
uls_nonrotated <- fa(cov_matrix, 4, rotate = "none", fm="uls")

cat("\n\n\n          ----------------\n          | CON ROTACIÓN |\n          ----------------\n\n\n")
cat("Resultados de principal axis con rotación:\n\n")
print(pa_rotated)
cat("\n\nResultados de unweighted least squares con rotación:\n\n")
print(uls_rotated)
cat("\n\nResultados de weighted least squares con rotación:\n\n")
print(wls_rotated)
cat("\n\nResultados de maximum likelihood con rotación:\n\n")
print(mle_rotated)

cat("\n\n\n          ----------------\n          | SIN ROTACIÓN |\n          ----------------\n\n\n")
cat("Resultados de weighted least squares sin rotación:\n\n")
print(wls_nonrotated)
cat("Resultados de principal axis sin rotación:\n\n")
print(pa_nonrotated)
cat("Resultados de minres sin rotación:\n\n")
print(minres_nonrotated)
cat("Resultados de maximum likelihood sin rotación:\n\n")
print(mle_nonrotated)
cat("Resultados de unweighted least squares sin rotación:\n\n")
print(uls_nonrotated)