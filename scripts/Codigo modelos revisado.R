# PRELIMINARES ------------------------------------------------------------
graphics.off() # Borrar gráficos
rm(list=ls())  # Borrar memoria
# General------------------------------------------------------------------------------ 
# Cargar librerias y modulo wiener
library(runjags)
library(rjags)
library(tidyverse)
library(loo)
setwd('C:/Users/usuario/Desktop/Tarea baye')
load.module("wiener")
source("HDIofMCMC.r") # Load the HDI function

# Cargar base de datos
load('DM_data.Rdata')

#Recodificacion tiempos de reaccion con signo (izquierda negativo/derecha positivo)
RT <- (DATA$ttr-DATA$ttp)/10000 
RT <- as.numeric(RT)
x<- seq(1,3000,length.out=3000)
y<- DATA$RT
y[DATA$r==1]<- -RT[DATA$r==1] #izquierda negativo
y[DATA$r==2]<-  RT[DATA$r==2] #derecha positivo

DATA<-mutate(DATA,y=y)
DATA<-mutate(DATA,x=x)

# Distribucion del conjunto total de datos
c <- ggplot(DATA,aes(y))
c + geom_histogram(binwidth = 0.1)

# MODELO 1 alpha, beta, delta y tau ---------------------------------------

#### FILTRAR DATOS MODELO 1: CASO A: SIN AMBIGUEDAD (máscara sin opacidad)
DATAF<-filter(DATA,a==0)
#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Preparar data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, Ntotal=Ntotal))

# Iniciar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar JAGS variables latentes a monitoriar
monitor = c("alph","ta","bet","delt","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results1a <- run.jags(model="models/ddm_evaluacion_modelo1.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results1a

#### FILTRAR DATOS MODELO 1: CASO B: AMBIGUEDAD (máscara con opacidad 0.3)
DATAF<-filter(DATA,a==0.3)
#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Preparar data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, Ntotal=Ntotal))

# Iniciar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar JAGS variables latentes a monitoriar
monitor = c("alph","ta","bet","delt","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results1b <- run.jags(model="models/ddm_evaluacion_modelo1.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results1b

#### FILTRAR DATOS MODELO 1: CASO C: AMBIGUEDAD (máscara con opacidad 0.4)
DATAF<-filter(DATA,a==0.4)
#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Preparar data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, Ntotal=Ntotal))

# Iniciar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar JAGS variables latentes a monitoriar
monitor = c("alph","ta","bet","delt","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results1c <- run.jags(model="models/ddm_evaluacion_modelo1.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results1c

#### FILTRAR DATOS MODELO 1: CASO D: AMBIGUEDAD (máscara con opacidad 0.5)
DATAF<-filter(DATA,a==0.5)
#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Preparar data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, Ntotal=Ntotal))

# Iniciar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar JAGS variables latentes a monitoriar
monitor = c("alph","ta","bet","delt","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results1d <- run.jags(model="models/ddm_evaluacion_modelo1.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results1d

###

#### FILTRAR DATOS MODELO 1: CASO E: CON INCERTIDUMBRE, ALPHA=0.6
DATAF<-filter(DATA,a==0.6)
#Distribucion de los datos que se usar?n en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# prepare the data for JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, Ntotal=Ntotal))

# Initialize chains
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,delt=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Tell JAGS which latent variables to monitor
monitor = c("alph","ta","bet","delt","deviance")

# Run the function that fits the models using JAGS
results1e <- run.jags(model="models/ddm_evaluacion_modelo1.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results1e

# GUARDAR RESULTADOS
save(results1a,results1b,results1c,results1d,results1e,file="resultsMod1.Rdata")



# MODELO 2 alpha beta tau y delta (lineal O y P) --------------------------

###

# FILTRAR DATOS MODELO 2: CASO A: SIN AMBIGUEDAD (máscara sin opacidad)
DATAF<-filter(DATA,a==0)
#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# preparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b2","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results2a <- run.jags(model="models/ddm_evaluacion_modelo2.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results2a

###

# FILTRAR DATOS MODELO 2: CASO B: AMBIGUEDAD (máscara con opacidad 0.3)
DATAF<-filter(DATA,a==0.3)
#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b2","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results2b <- run.jags(model="models/ddm_evaluacion_modelo2.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results2b

###

# FILTRAR DATOS MODELO 2: CASO C: AMBIGUEDAD (máscara con opacidad 0.4)
DATAF<-filter(DATA,a==0.4)
#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b2","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results2c <- run.jags(model="models/ddm_evaluacion_modelo2.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results2c

###

# FILTRAR DATOS MODELO 2: CASO D: AMBIGUEDAD (máscara con opacidad 0.5)
DATAF<-filter(DATA,a==0.5)
#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b2","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results2d <- run.jags(model="models/ddm_evaluacion_modelo2.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results2d

###

# FILTRAR DATOS MODELO 2: CASO E: AMBIGUEDAD (máscara con opacidad 0.6)
DATAF<-filter(DATA,a==0.6)
#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b2","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results2e <- run.jags(model="models/ddm_evaluacion_modelo2.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results2e

# GUARDAR RESULTADOS
save(results2a,results2b,results2c,results2d,results2e,file="resultsMod2.Rdata")


# MODELO 3: Alpha, beta, tau y delta (P y O cuadrática) -------------------

###

# FILTRAR DATOS MODELO 3: CASO A: SIN AMBIGUEDAD (máscara sin opacidad)
DATAF<-filter(DATA,a==0)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b2","b3","b4","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results3a <- run.jags(model="models/ddm_evaluacion_modelo3.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results3a

###

# FILTRAR DATOS MODELO 3: CASO B: AMBIGUEDAD (máscara con opacidad 0.3)
DATAF<-filter(DATA,a==0.3)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b2","b3","b4","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results3b <- run.jags(model="models/ddm_evaluacion_modelo3.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results3b

###

# FILTRAR DATOS MODELO 3:  CASO C: AMBIGUEDAD (máscara con opacidad 0.4)
DATAF<-filter(DATA,a==0.4)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b2","b3","b4","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results3c <- run.jags(model="models/ddm_evaluacion_modelo3.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results3c

###

# FILTRAR DATOS MODELO 3: CASO D: AMBIGUEDAD (máscara con opacidad 0.5)
DATAF<-filter(DATA,a==0.5)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b2","b3","b4","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results3d <- run.jags(model="models/ddm_evaluacion_modelo3.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results3d

# GUARDAR RESULTADOS
save(results3a,results3b,results3c,results3d,results3e,file="resultsMod3.Rdata")


###

# FILTRAR DATOS MODELO 3: CASO E: AMBIGUEDAD (máscara con opacidad 0.6)
DATAF<-filter(DATA,a==0.6)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b2=0,b3=0,b4=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b2","b3","b4","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results3e <- run.jags(model="models/ddm_evaluacion_modelo3.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results3e


# MODELO 4 alpha, beta, tau y delta (P cuadrado) --------------------------

# FILTRAR DATOS MODELO 4: CASO A: SIN AMBIGUEDAD (máscara sin opacidad)
DATAF<-filter(DATA,a==0)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b3","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results4a <- run.jags(model="models/ddm_evaluacion_modelo4",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results4a

###
# FILTRAR DATOS MODELO 4: CASO B: AMBIGUEDAD (máscara con opacidad 0.3)
DATAF<-filter(DATA,a==0.3)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b3","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results4b <- run.jags(model="models/ddm_evaluacion_modelo4.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results4b

###
# FILTRAR DATOS MODELO 4: CASO C: AMBIGUEDAD (máscara con opacidad 0.4)
DATAF<-filter(DATA,a==0.4)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b3","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results4c <- run.jags(model="models/ddm_evaluacion_modelo4.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results4c

###

# FILTRAR DATOS MODELO 4: CASO D: AMBIGUEDAD (máscara con opacidad 0.5)
DATAF<-filter(DATA,a==0.5)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b3","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results4d <- run.jags(model="models/ddm_evaluacion_modelo4.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results4d

###

# FILTRAR DATOS MODELO 4: CASO E: AMBIGUEDAD (máscara con opacidad 0.6)
DATAF<-filter(DATA,a==0.6)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, p=p, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b1=0,b3=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b1","b3","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results4e <- run.jags(model="models/ddm_evaluacion_modelo4.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results4e

# GUARDAR RESULTADOS
save(results4a,results4b,results4c,results4d,results4e,file="resultsMod4.Rdata")



# MODELO 5: alpha, beta, tau y delta (O cuadrado) -------------------------

# FILTRAR DATOS MODELO 5: CASO A: SIN AMBIGUEDAD (máscara sin opacidad)
DATAF<-filter(DATA,a==0)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b2","b4","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results5a <- run.jags(model="models/ddm_evaluacion_modelo5.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results5a

###

# FILTRAR DATOS MODELO 5: CASO B: AMBIGUEDAD (máscara con opacidad 0.3)
DATAF<-filter(DATA,a==0.3)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b2","b4","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results5b <- run.jags(model="models/ddm_evaluacion_modelo5.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results5b

###

# FILTRAR DATOS MODELO 5: CASO C: AMBIGUEDAD (máscara con opacidad 0.4)
DATAF<-filter(DATA,a==0.4)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b2","b4","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results5c <- run.jags(model="models/ddm_evaluacion_modelo5.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results5c
###


# FILTRAR DATOS MODELO 5: CASO D: AMBIGUEDAD (máscara con opacidad 0.5)
DATAF<-filter(DATA,a==0.5)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b2","b4","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results5d <- run.jags(model="models/ddm_evaluacion_modelo5.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results5d
###

# FILTRAR DATOS MODELO 5: CASO E: AMBIGUEDAD (máscara con opacidad 0.6)
DATAF<-filter(DATA,a==0.6)

#Distribucion de los datos que se usaron en el modelo
c <- ggplot(DATAF,aes(y))
c + geom_histogram(binwidth = 0.1)

# Perparar la data para JAGS
y<-DATAF$y
p<-DATAF$p
o<-DATAF$o
Ntotal <- length(y)
dat <- dump.format(list(y=y, o=o, Ntotal=Ntotal))

# Comenzar cadenas
inits1 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Super-Duper", .RNG.seed=99999 ))
inits2 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Wichmann-Hill", .RNG.seed=1234 ))
inits3 <- dump.format(list(alph=1,ta=0.001,bet=0.5,b0=0,b2=0,b4=0, .RNG.name="base::Mersenne-Twister", .RNG.seed=6666 ))

# Informar a JAGS las variables latentes por monitorear
monitor = c("alph","ta","bet","b0","b2","b4","deviance")

# Ejecutar la función que ajusta los modelos usando JAGS
results5e <- run.jags(model="models/ddm_evaluacion_modelo5.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results5e

# GUARDAR RESULTADOS
save(results5a,results5b,results5c,results5d,results5e,file="resultsMod5.Rdata")









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
chains3b = rbind(results3b$mcmc[[1]], results3b$mcmc[[2]], results3b$mcmc[[3]])
chains3c = rbind(results3c$mcmc[[1]], results3c$mcmc[[2]], results3c$mcmc[[3]])
chains3d = rbind(results3d$mcmc[[1]], results3d$mcmc[[2]], results3d$mcmc[[3]])
chains3e = rbind(results3e$mcmc[[1]], results3e$mcmc[[2]], results3e$mcmc[[3]])

chains4a = rbind(results4a$mcmc[[1]], results4a$mcmc[[2]], results4a$mcmc[[3]])
chains4b = rbind(results4b$mcmc[[1]], results4b$mcmc[[2]], results4b$mcmc[[3]])
chains4c = rbind(results4c$mcmc[[1]], results4c$mcmc[[2]], results4c$mcmc[[3]])
chains4d = rbind(results4d$mcmc[[1]], results4d$mcmc[[2]], results4d$mcmc[[3]])
chains4e = rbind(results4e$mcmc[[1]], results4e$mcmc[[2]], results4e$mcmc[[3]])

chains5a = rbind(results5a$mcmc[[1]], results5a$mcmc[[2]], results5a$mcmc[[3]])
chains5b = rbind(results5b$mcmc[[1]], results5b$mcmc[[2]], results5b$mcmc[[3]])
chains5c = rbind(results5c$mcmc[[1]], results5c$mcmc[[2]], results5c$mcmc[[3]])
chains5d = rbind(results5d$mcmc[[1]], results5d$mcmc[[2]], results5d$mcmc[[3]])
chains5e = rbind(results5e$mcmc[[1]], results5e$mcmc[[2]], results5e$mcmc[[3]])



#Dic y LOO-----------------------

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
                        drop.monitor=results1d$monitor, add.monitor=c("y_dlog"),
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

com1<-loo_compare(loo_2e, loo_1e)
print(com1, simplify = FALSE, digits = 3)

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

bind_loo2b = rbind(extend_2b$mcmc[[1]], extend_2b$mcmc[[2]], extend_2b$mcmc[[3]])

loo_2b<- loo(bind_loo2b)

###

#MODELO 2C

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


###

#MODELO 3C
extend_3c = extend.jags(results3c, 
                        drop.monitor=results3c$monitor, add.monitor=c("y_dlog"),
                        check.stochastic = FALSE, sample = 1000, adapt=0,
                        burnin=0, summarise=FALSE, method=c("parallel"))

bind_loo3c = rbind(extend_3c$mcmc[[1]], extend_3c$mcmc[[2]], extend_3c$mcmc[[3]])

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


