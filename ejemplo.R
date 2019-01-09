cov_matrix <- Harman74.cor$cov
library(psych)
library(GPArotation)
library(cfa)
library(lavaan)
##                                 EFA                                       ##

##                        Numero de factores                                 ##
VSS(sim.item(nvar=24),n=8,fm="minres" ,title="VSS of 24 simple structure variables")

##                               Con rotacion                                ##
# Principal Axis
pa_rotated <- fa(cov_matrix, 4, fm="pa", rotate="varimax")
# Unweighted least squares: minres
uls_rotated <- fa(cov_matrix, 4, rotate = "varimax")
# Weighted least squares
wls_rotated <- fa(cov_matrix, 4, fm = "wls")

##                       Máxima verosimilitud                                ##
mle_rotated <- factanal(covmat = cov_matrix, factors = 4)

##                             Sin rotacion                                  ##
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

#----------------          | CON ROTACION |          ----------------
#Resultados de principal axis con rotacion:
summary(pa_rotated)
#Resultados de unweighted least squares con rotacion:
summary(uls_rotated)
#Resultados de weighted least squares con rotacion:
summary(wls_rotated)
#Resultados de maximum likelihood con rotacion:
summary(mle_rotated)

#----------------          | SIN ROTACION |          ----------------
#Resultados de weighted least squares sin rotacion:
summary(wls_nonrotated)
#Resultados de principal axis sin rotacion:
summary(pa_nonrotated)
#Resultados de minres sin rotacion:
summary(minres_nonrotated)
#Resultados de maximum likelihood sin rotacion:
summary(mle_nonrotated)
#Resultados de unweighted least squares sin rotacion:
summary(uls_nonrotated)

##                                 CFA                                       ##
# Hacemos nuestra hipotesis del modelo
HS.model <- ' visual  =~ x1 + x2 + x3
              textual =~ x4 + x5 + x6
              speed   =~ x7 + x8 + x9 '

# Comprobamos la hipotesis
fit <- cfa(HS.model, data=HolzingerSwineford1939)

# Resultados de CFA
summary(fit, fit.measures=TRUE)
summary(fit)
