# PRELIMINARES ------------------------------------------------------------
graphics.off() # Borrar gráficos
rm(list=ls())  # Borrar memoria
# General------------------------------------------------------------------------------ 
# Cargar librerias y modulo wiener
library(runjags)
library(rjags)
library(tidyverse)
library(loo)
setwd('C:/Users/aleja/OneDrive/Escritorio/evaluacion/')
load.module("wiener")
source("HDIofMCMC.r") # Load the HDI function

# CARGAR RESULTADOS DE LOS 5 MODELOS

load("Downloads/resultsMod1.Rdata")
load("resultsMod2.Rdata")
load("resultsMod3.Rdata")
load("resultsMod4.Rdata")
load("resultsMod5.Rdata")

# ANALISIS DE RESULTADOS --------------------------------------------------

# CONCATENACION DE CHAINS
chains1a = rbind(results1a$mcmc[[1]], results1a$mcmc[[2]], results1a$mcmc[[3]])
chains1b = rbind(results1b$mcmc[[1]], results1b$mcmc[[2]], results1b$mcmc[[3]])
chains1c = rbind(results1c$mcmc[[1]], results1c$mcmc[[2]], results1c$mcmc[[3]])
chains1d = rbind(results1d$mcmc[[1]], results1d$mcmc[[2]], results1d$mcmc[[3]])
chains1e = rbind(results1e$mcmc[[1]], results1e$mcmc[[2]], results1e$mcmc[[3]])

chains2a = rbind(results2a$mcmc[[1]], results2a$mcmc[[2]], results2a$mcmc[[3]])
chains2b = rbind(results2b$mcmc[[1]], results2b$mcmc[[2]], results2b$mcmc[[3]])
chains2c = rbind(results2c$mcmc[[1]], results2c$mcmc[[2]], results2c$mcmc[[3]])
chains2d = rbind(results2d$mcmc[[1]], results2d$mcmc[[2]], results2d$mcmc[[3]])
chains2e = rbind(results2e$mcmc[[1]], results2e$mcmc[[2]], results2e$mcmc[[3]])

chains3a = rbind(results3a$mcmc[[1]], results3a$mcmc[[2]], results3a$mcmc[[3]])
chains3e = rbind(results3b$mcmc[[1]], results3b$mcmc[[2]], results3b$mcmc[[3]])
chains3a = rbind(results3c$mcmc[[1]], results3c$mcmc[[2]], results3c$mcmc[[3]])
chains3a = rbind(results3d$mcmc[[1]], results3d$mcmc[[2]], results3d$mcmc[[3]])
chains3a = rbind(results3e$mcmc[[1]], results3e$mcmc[[2]], results3e$mcmc[[3]])

chains4a = rbind(results4a$mcmc[[1]], results4a$mcmc[[2]], results4a$mcmc[[3]])
chains4e = rbind(results4b$mcmc[[1]], results4b$mcmc[[2]], results4b$mcmc[[3]])
chains4a = rbind(results4c$mcmc[[1]], results4c$mcmc[[2]], results4c$mcmc[[3]])
chains4a = rbind(results4d$mcmc[[1]], results4d$mcmc[[2]], results4d$mcmc[[3]])
chains4a = rbind(results4e$mcmc[[1]], results4e$mcmc[[2]], results4e$mcmc[[3]])

chains5a = rbind(results5a$mcmc[[1]], results5a$mcmc[[2]], results5a$mcmc[[3]])
chains5b = rbind(results5b$mcmc[[1]], results5b$mcmc[[2]], results5b$mcmc[[3]])
chains5c = rbind(results5c$mcmc[[1]], results5c$mcmc[[2]], results5c$mcmc[[3]])
chains5d = rbind(results5d$mcmc[[1]], results5d$mcmc[[2]], results5d$mcmc[[3]])
chains5e = rbind(results5e$mcmc[[1]], results5e$mcmc[[2]], results5e$mcmc[[3]])


# Analisis de los parametros de los modelos -------------------------------
# Analisis MODELO 1 -------------------------------------------------------
#modelo 1 A

alph.1a=chains1a[,"alph"]
ta.1a  =chains1a[,"ta"]
bet.1a =chains1a[,"bet"]
delt.1a=chains1a[,"delt"]

# PLOT de alpha.1a
plot(density(alph.1a), lwd=3)
hdi = HDIofMCMC(alph.1a , credMass=0.95)
abline(v=median(alph.1a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de bet.1a
plot(density(bet.1a), lwd=3)
hdi = HDIofMCMC(bet.1a , credMass=0.95)
abline(v=median(bet.1a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de ta.1a
plot(density(ta.1a), lwd=3)
hdi = HDIofMCMC(ta.1a , credMass=0.95)
abline(v=median(ta.1a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de delt.1a
plot(density(delt.1a), lwd=3)
hdi = HDIofMCMC(delt.1a , credMass=0.95)
abline(v=median(delt.1a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

###

#modelo 1 B

alph.1b=chains1b[,"alph"]
ta.1b  =chains1b[,"ta"]
bet.1b =chains1b[,"bet"]
delt.1b=chains1b[,"delt"]

# PLOT de alpha.1b
plot(density(alph.1b), lwd=3)
hdi = HDIofMCMC(alph.1b , credMass=0.95)
abline(v=median(alph.1b), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de bet.1b
plot(density(bet.1b), lwd=3)
hdi = HDIofMCMC(bet.1b , credMass=0.95)
abline(v=median(bet.1b), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de ta.1b
plot(density(ta.1b), lwd=3)
hdi = HDIofMCMC(ta.1b , credMass=0.95)
abline(v=median(ta.1b), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de delt.1b
plot(density(delt.1b), lwd=3)
hdi = HDIofMCMC(delt.1b , credMass=0.95)
abline(v=median(delt.1b), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

###

#modelo 1 C

alph.1c=chains1c[,"alph"]
ta.1c  =chains1c[,"ta"]
bet.1c =chains1c[,"bet"]
delt.1c=chains1c[,"delt"]

# PLOT de alpha.1c
plot(density(alph.1c), lwd=3)
hdi = HDIofMCMC(alph.1c , credMass=0.95)
abline(v=median(alph.1c), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de bet.1c
plot(density(bet.1c), lwd=3)
hdi = HDIofMCMC(bet.1c , credMass=0.95)
abline(v=median(bet.1c), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de ta.1c
plot(density(ta.1c), lwd=3)
hdi = HDIofMCMC(ta.1c , credMass=0.95)
abline(v=median(ta.1c), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de delt.1c
plot(density(delt.1c), lwd=3)
hdi = HDIofMCMC(delt.1c , credMass=0.95)
abline(v=median(delt.1c), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)
###

#modelo 1 D
alph.1d=chains1d[,"alph"]
ta.1d  =chains1d[,"ta"]
bet.1d =chains1d[,"bet"]
delt.1d=chains1d[,"delt"]

# PLOT de alpha.1d
plot(density(alph.1d), lwd=3)
hdi = HDIofMCMC(alph.1d , credMass=0.95)
abline(v=median(alph.1d), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de bet.1d
plot(density(bet.1d), lwd=3)
hdi = HDIofMCMC(bet.1d , credMass=0.95)
abline(v=median(bet.1d), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de ta.1d
plot(density(ta.1d), lwd=3)
hdi = HDIofMCMC(ta.1d , credMass=0.95)
abline(v=median(ta.1d), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de delt.1d
plot(density(delt.1d), lwd=3)
hdi = HDIofMCMC(delt.1d , credMass=0.95)
abline(v=median(delt.1d), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

###

#modelo 1 E

alph.1e=chains1e[,"alph"]
ta.1e  =chains1e[,"ta"]
bet.1e =chains1e[,"bet"]
delt.1e=chains1e[,"delt"]

# PLOT de alpha.1e
plot(density(alph.1e), lwd=3)
hdi = HDIofMCMC(alph.1e , credMass=0.95)
abline(v=median(alph.1e), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de bet.1e
plot(density(bet.1e), lwd=3)
hdi = HDIofMCMC(bet.1e , credMass=0.95)
abline(v=median(bet.1e), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de ta.1e
plot(density(ta.1e), lwd=3)
hdi = HDIofMCMC(ta.1e , credMass=0.95)
abline(v=median(ta.1e), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de delt.1e
plot(density(delt.1e), lwd=3)
hdi = HDIofMCMC(delt.1e , credMass=0.95)
abline(v=median(delt.1e), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# Analisis MODELO 2 -------------------------------------------------------

#modelo 2 A

alph.2a=chains2a[,"alph"]
ta.2a  =chains2a[,"ta"]
bet.2a =chains2a[,"bet"]
b0.2a = chains2a[,"b0"]
b1.2a = chains2a[,"b1"]
b2.2a = chains2a[,"b2"]

# PLOT de alpha.2a
plot(density(alph.2a), lwd=3)
hdi = HDIofMCMC(alph.2a , credMass=0.95)
abline(v=median(alph.2a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de bet.2a
plot(density(bet.2a), lwd=3)
hdi = HDIofMCMC(bet.2a , credMass=0.95)
abline(v=median(bet.2a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de ta.2a
plot(density(ta.2a), lwd=3)
hdi = HDIofMCMC(ta.2a , credMass=0.95)
abline(v=median(ta.2a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b0.2a
plot(density(b0.2a), lwd=3)
hdi = HDIofMCMC(b0.2a , credMass=0.95)
abline(v=median(b0.2a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b1.2a
plot(density(b1.2a), lwd=3)
hdi = HDIofMCMC(b1.2a , credMass=0.95)
abline(v=median(b1.2a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b2.2a
plot(density(b2.2a), lwd=3)
hdi = HDIofMCMC(b2.2a , credMass=0.95)
abline(v=median(b2.2a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)
###

#modelo 2 B

alph.2b=chains2b[,"alph"]
ta.2b  =chains2b[,"ta"]
bet.2b =chains2b[,"bet"]
b0.2b = chains2b[,"b0"]
b1.2b = chains2b[,"b1"]
b2.2b = chains2b[,"b2"]

# PLOT de alpha.2b
plot(density(alph.2b), lwd=3)
hdi = HDIofMCMC(alph.2b , credMass=0.95)
abline(v=median(alph.2b), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de bet.2b
plot(density(bet.2b), lwd=3)
hdi = HDIofMCMC(bet.2b , credMass=0.95)
abline(v=median(bet.2b), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de ta.2b
plot(density(ta.2b), lwd=3)
hdi = HDIofMCMC(ta.2b , credMass=0.95)
abline(v=median(ta.2b), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b0.2b
plot(density(b0.2b), lwd=3)
hdi = HDIofMCMC(b0.2b , credMass=0.95)
abline(v=median(b0.2b), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b1.2b
plot(density(b1.2b), lwd=3)
hdi = HDIofMCMC(b1.2b , credMass=0.95)
abline(v=median(b1.2b), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b2.2b
plot(density(b2.2b), lwd=3)
hdi = HDIofMCMC(b2.2b , credMass=0.95)
abline(v=median(b2.2b), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

###

#modelo 2 C

alph.2c=chains2c[,"alph"]
ta.2c  =chains2c[,"ta"]
bet.2c =chains2c[,"bet"]
b0.2c = chains2c[,"b0"]
b1.2c = chains2c[,"b1"]
b2.2c = chains2c[,"b2"]

# PLOT de alpha.2c
plot(density(alph.2c), lwd=3)
hdi = HDIofMCMC(alph.2c , credMass=0.95)
abline(v=median(alph.2c), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de bet.2c
plot(density(bet.2c), lwd=3)
hdi = HDIofMCMC(bet.2c , credMass=0.95)
abline(v=median(bet.2c), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de ta.2c
plot(density(ta.2c), lwd=3)
hdi = HDIofMCMC(ta.2c , credMass=0.95)
abline(v=median(ta.2c), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b0.2c
plot(density(b0.2c), lwd=3)
hdi = HDIofMCMC(b0.2c , credMass=0.95)
abline(v=median(b0.2c), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b1.2c
plot(density(b1.2c), lwd=3)
hdi = HDIofMCMC(b1.2c , credMass=0.95)
abline(v=median(b1.2c), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b2.2c
plot(density(b2.2c), lwd=3)
hdi = HDIofMCMC(b2.2c , credMass=0.95)
abline(v=median(b2.2c), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

###

#modelo 2 D

alph.2d=chains2d[,"alph"]
ta.2d  =chains2d[,"ta"]
bet.2d =chains2d[,"bet"]
b0.2d = chains2d[,"b0"]
b1.2d = chains2d[,"b1"]
b2.2d = chains2d[,"b2"]

# PLOT de alpha.2d
plot(density(alph.2d), lwd=3)
hdi = HDIofMCMC(alph.2d , credMass=0.95)
abline(v=median(alph.2d), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de bet.2d
plot(density(bet.2d), lwd=3)
hdi = HDIofMCMC(bet.2d , credMass=0.95)
abline(v=median(bet.2d), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de ta.2d
plot(density(ta.2d), lwd=3)
hdi = HDIofMCMC(ta.2d , credMass=0.95)
abline(v=median(ta.2d), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b0.2d
plot(density(b0.2d), lwd=3)
hdi = HDIofMCMC(b0.2d , credMass=0.95)
abline(v=median(b0.2d), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b1.2d
plot(density(b1.2d), lwd=3)
hdi = HDIofMCMC(b1.2d , credMass=0.95)
abline(v=median(b1.2d), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b2.2d
plot(density(b2.2d), lwd=3)
hdi = HDIofMCMC(b2.2d , credMass=0.95)
abline(v=median(b2.2d), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

###

#modelo 2 E

alph.2e=chains2e[,"alph"]
ta.2e  =chains2e[,"ta"]
bet.2e =chains2e[,"bet"]
b0.2e = chains2e[,"b0"]
b1.2e = chains2e[,"b1"]
b2.2e = chains2e[,"b2"]

# PLOT de alpha.2e
plot(density(alph.2e), lwd=3)
hdi = HDIofMCMC(alph.2e , credMass=0.95)
abline(v=median(alph.2e), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de bet.2e
plot(density(bet.2e), lwd=3)
hdi = HDIofMCMC(bet.2e , credMass=0.95)
abline(v=median(bet.2e), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de ta.2e
plot(density(ta.2e), lwd=3)
hdi = HDIofMCMC(ta.2e , credMass=0.95)
abline(v=median(ta.2e), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b0.2e
plot(density(b0.2e), lwd=3)
hdi = HDIofMCMC(b0.2e , credMass=0.95)
abline(v=median(b0.2e), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b1.2e
plot(density(b1.2e), lwd=3)
hdi = HDIofMCMC(b1.2e , credMass=0.95)
abline(v=median(b1.2e), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b2.2e
plot(density(b2.2e), lwd=3)
hdi = HDIofMCMC(b2.2e , credMass=0.95)
abline(v=median(b2.2e), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)


# Analisis MODELO 3 -------------------------------------------------------

#modelo 3 A

alph.3a=chains3a["alph"]
ta.3a  =chains3a["ta"]
bet.3a =chains3a["bet"]
delt.3a=chains3a["delt"]
b0.3a = chains3a[,"b0"]
b1.3a = chains3a[,"b1"]
b2.3a = chains3a[,"b2"]
b3.3a = chains3a[,"b3"]
b4.3a = chains3a[,"b4"]

# PLOT de alpha.3a
plot(density(alph.3a), lwd=3)
hdi = HDIofMCMC(alph.3a , credMass=0.95)
abline(v=median(alph.3a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de bet.3a
plot(density(bet.3a), lwd=3)
hdi = HDIofMCMC(bet.3a , credMass=0.95)
abline(v=median(bet.3a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de ta.3a
plot(density(ta.3a), lwd=3)
hdi = HDIofMCMC(ta.3a , credMass=0.95)
abline(v=median(ta.3a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b0.3a
plot(density(b0.3a), lwd=3)
hdi = HDIofMCMC(b0.3a , credMass=0.95)
abline(v=median(b0.3a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b1.3a
plot(density(b1.3a), lwd=3)
hdi = HDIofMCMC(b1.3a , credMass=0.95)
abline(v=median(b1.3a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b2.3a
plot(density(b2.3a), lwd=3)
hdi = HDIofMCMC(b2.3a , credMass=0.95)
abline(v=median(b2.3a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b3.3a
plot(density(b3.3a), lwd=3)
hdi = HDIofMCMC(b3.3a , credMass=0.95)
abline(v=median(b3.3a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)

# PLOT de b4.3a
plot(density(b4.3a), lwd=3)
hdi = HDIofMCMC(b4.3a , credMass=0.95)
abline(v=median(b4.3a), col="red", lwd=2)
# añadir los límites del HDI del MCMC de interés
abline(v=hdi[1], lty=2, col="red", lwd=2)
abline(v=hdi[2], lty=2, col="red", lwd=2)
# compare the HDI limits to 0
abline(v=0, col="grey", lwd=2)





# COMPARACION DE MODELOS - DIC --------------------------------------------

# MODELO 1
DIC.1a = mean(chains1a[,"deviance"]) + (sd(chains1a[,"deviance"])^2) / 2
DIC.1b = mean(chains1b[,"deviance"]) + (sd(chains1b[,"deviance"])^2) / 2
DIC.1c = mean(chains1c[,"deviance"]) + (sd(chains1c[,"deviance"])^2) / 2
DIC.1d = mean(chains1d[,"deviance"]) + (sd(chains1d[,"deviance"])^2) / 2
DIC.1e = mean(chains1e[,"deviance"]) + (sd(chains1e[,"deviance"])^2) / 2

# MODELO 2
DIC.2a = mean(chains2a[,"deviance"]) + (sd(chains2a[,"deviance"])^2) / 2
DIC.2b = mean(chains2b[,"deviance"]) + (sd(chains2b[,"deviance"])^2) / 2
DIC.2c = mean(chains2c[,"deviance"]) + (sd(chains2c[,"deviance"])^2) / 2
DIC.2d = mean(chains2d[,"deviance"]) + (sd(chains2d[,"deviance"])^2) / 2
DIC.2e = mean(chains2e[,"deviance"]) + (sd(chains2e[,"deviance"])^2) / 2

# MODELO 3
DIC.3a = mean(chains3a[,"deviance"]) + (sd(chains3a[,"deviance"])^2) / 2
DIC.3b = mean(chains3b[,"deviance"]) + (sd(chains3b[,"deviance"])^2) / 2
DIC.3c = mean(chains3c[,"deviance"]) + (sd(chains3c[,"deviance"])^2) / 2
DIC.3d = mean(chains3d[,"deviance"]) + (sd(chains3d[,"deviance"])^2) / 2
DIC.3e = mean(chains3e[,"deviance"]) + (sd(chains3e[,"deviance"])^2) / 2

# MODELO 4
DIC.4a = mean(chains4a[,"deviance"]) + (sd(chains4a[,"deviance"])^2) / 2
DIC.4b = mean(chains4b[,"deviance"]) + (sd(chains4b[,"deviance"])^2) / 2
DIC.4c = mean(chains4c[,"deviance"]) + (sd(chains4c[,"deviance"])^2) / 2
DIC.4d = mean(chains4d[,"deviance"]) + (sd(chains4d[,"deviance"])^2) / 2
DIC.4e = mean(chains4e[,"deviance"]) + (sd(chains4e[,"deviance"])^2) / 2

# MODELO 5
DIC.5a = mean(chains5a[,"deviance"]) + (sd(chains5a[,"deviance"])^2) / 2
DIC.5b = mean(chains5b[,"deviance"]) + (sd(chains5b[,"deviance"])^2) / 2
DIC.5c = mean(chains5c[,"deviance"]) + (sd(chains5c[,"deviance"])^2) / 2
DIC.5d = mean(chains5d[,"deviance"]) + (sd(chains5d[,"deviance"])^2) / 2
DIC.5e = mean(chains5e[,"deviance"]) + (sd(chains5e[,"deviance"])^2) / 2



# COMPARACION DE MODELOS - LOO --------------------------------------------
# LOO Modelo 1 ------------------------------------------------------------


#MODELO 1A
extend_1a = extend.jags(results1a, 
                        drop.monitor=results1a$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo1a = rbind(extend_1a$mcmc[[1]], extend_1a$mcmc[[2]], extend_1a$mcmc[[3]])

loo_1a <- loo(bind_loo1a)

###

#MODELO 1B
extend_1b = extend.jags(results1b, 
                        drop.monitor=results1b$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo1b = rbind(extend_1b$mcmc[[1]], extend_1b$mcmc[[2]], extend_1b$mcmc[[3]])

loo_1b<- loo(bind_loo1b)

###

#MODELO 1C
extend_1c = extend.jags(results1c, 
                        drop.monitor=results1c$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo1c = rbind(extend_1c$mcmc[[1]], extend_1c$mcmc[[2]], extend_1c$mcmc[[3]])

loo_1c <- loo(bind_loo1c)

###

#MODELO 1D
extend_1d = extend.jags(results1d, 
                        drop.monitor=results1$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo1d = rbind(extend_1d$mcmc[[1]], extend_1d$mcmc[[2]], extend_1d$mcmc[[3]])

loo_1d <- loo(bind_loo1d)

###

#MODELO 1E
extend_1e = extend.jags(results1e, 
                        drop.monitor=results1e$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo1e = rbind(extend_1e$mcmc[[1]], extend_1e$mcmc[[2]], extend_1e$mcmc[[3]])

loo_1e <- loo(bind_loo1e)


# LOO Modelo 2 ------------------------------------------------------------

#MODELO 2A
extend_2a = extend.jags(results2a, 
                        drop.monitor=results2a$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo2a = rbind(extend_2a$mcmc[[1]], extend_2a$mcmc[[2]], extend_2a$mcmc[[3]])

loo_2a <- loo(bind_loo2a)

###

#MODELO 2B
extend_2b = extend.jags(results2b, 
                        drop.monitor=results2b$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo1b = rbind(extend_2b$mcmc[[1]], extend_2b$mcmc[[2]], extend_2b$mcmc[[3]])

loo_2b<- loo(bind_loo2b)

###

#MODELO 2C
extend_2c = extend.jags(results2c, 
                        drop.monitor=results2c$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo1d = rbind(extend_2c$mcmc[[1]], extend_2c$mcmc[[2]], extend_2c$mcmc[[3]])

loo_2c <- loo(bind_loo2c)

###

#MODELO 2D
extend_2d = extend.jags(results2d, 
                        drop.monitor=results2d$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo2d = rbind(extend_2d$mcmc[[1]], extend_2d$mcmc[[2]], extend_2d$mcmc[[3]])

loo_2d <- loo(bind_loo2d)

###

#MODELO 2E
extend_2e = extend.jags(results2e, 
                        drop.monitor=results2e$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo2e = rbind(extend_2e$mcmc[[1]], extend_2e$mcmc[[2]], extend_2e$mcmc[[3]])

loo_2e <- loo(bind_loo2e)



# LOO modelo 3 ------------------------------------------------------------

#MODELO 3A
extend_3a = extend.jags(results3a, 
                        drop.monitor=results3a$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo3a = rbind(extend_3a$mcmc[[1]], extend_3a$mcmc[[2]], extend_3a$mcmc[[3]])

loo_3a <- loo(bind_loo3a)

###

#MODELO 3B
extend_3b = extend.jags(results3b, 
                        drop.monitor=results3b$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo3b = rbind(extend_3b$mcmc[[1]], extend_3b$mcmc[[2]], extend_3b$mcmc[[3]])

loo_3b<- loo(bind_loo3b)

###

#MODELO 3C
extend_3c = extend.jags(results3c, 
                        drop.monitor=results3c$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo3d = rbind(extend_3c$mcmc[[1]], extend_3c$mcmc[[2]], extend_3c$mcmc[[3]])

loo_3c <- loo(bind_loo3c)

###

#MODELO 3D
extend_3d = extend.jags(results3d, 
                        drop.monitor=results3d$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo3d = rbind(extend_3d$mcmc[[1]], extend_3d$mcmc[[2]], extend_3d$mcmc[[3]])

loo_3d <- loo(bind_loo3d)

###

#MODELO 3E
extend_3e = extend.jags(results3e, 
                        drop.monitor=results3e$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo3e = rbind(extend_3e$mcmc[[1]], extend_3e$mcmc[[2]], extend_3e$mcmc[[3]])

loo_3e <- loo(bind_loo3e)



# LOO Modelo 4 ------------------------------------------------------------

#MODELO 4A
extend_4a = extend.jags(results4a, 
                        drop.monitor=results4a$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo4a = rbind(extend_4a$mcmc[[1]], extend_4a$mcmc[[2]], extend_4a$mcmc[[3]])

loo_4a <- loo(bind_loo4a)

###

#MODELO 4B
extend_4b = extend.jags(results4b, 
                        drop.monitor=results4b$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo4b = rbind(extend_4b$mcmc[[1]], extend_4b$mcmc[[2]], extend_4b$mcmc[[3]])

loo_4b<- loo(bind_loo4b)

###

#MODELO 4C
extend_4c = extend.jags(results4c, 
                        drop.monitor=results4c$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo4c = rbind(extend_4c$mcmc[[1]], extend_4c$mcmc[[2]], extend_4c$mcmc[[3]])

loo_4c <- loo(bind_loo4c)

###

#MODELO 4D
extend_4d = extend.jags(results4d, 
                        drop.monitor=results4d$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo4d = rbind(extend_4d$mcmc[[1]], extend_4d$mcmc[[2]], extend_4d$mcmc[[3]])

loo_4d <- loo(bind_loo4d)

###

#MODELO 4E
extend_4e = extend.jags(results4e, 
                        drop.monitor=results4e$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo4e = rbind(extend_4e$mcmc[[1]], extend_4e$mcmc[[2]], extend_4e$mcmc[[3]])

loo_4e <- loo(bind_loo4e)


# LOO Modelo 5 ------------------------------------------------------------

#MODELO 5A
extend_5a = extend.jags(results5a, 
                        drop.monitor=results5a$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo5a = rbind(extend_5a$mcmc[[1]], extend_5a$mcmc[[2]], extend_5a$mcmc[[3]])

loo_5a <- loo(bind_loo5a)

###

#MODELO 5B
extend_5b = extend.jags(results5b, 
                        drop.monitor=results5b$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo5b = rbind(extend_5b$mcmc[[1]], extend_5b$mcmc[[2]], extend_5b$mcmc[[3]])

loo_5b<- loo(bind_loo5b)

###

#MODELO 5C
extend_5c = extend.jags(results5c, 
                        drop.monitor=results5c$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo5c = rbind(extend_5c$mcmc[[1]], extend_5c$mcmc[[2]], extend_5c$mcmc[[3]])

loo_5c <- loo(bind_loo5c)

###

#MODELO 5D
extend_5d = extend.jags(results5d, 
                        drop.monitor=results5d$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo5d = rbind(extend_5d$mcmc[[1]], extend_5d$mcmc[[2]], extend_5d$mcmc[[3]])

loo_5d <- loo(bind_loo5d)

###

#MODELO 4E
extend_5e = extend.jags(results5e, 
                        drop.monitor=results5e$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo5e = rbind(extend_5e$mcmc[[1]], extend_5e$mcmc[[2]], extend_5e$mcmc[[3]])

loo_5e <- loo(bind_loo5e)

