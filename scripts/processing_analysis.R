# PRELIMINARES ------------------------------------------------------------
graphics.off() # Borrar gráficos
rm(list=ls())  # Borrar memoria
# General------------------------------------------------------------------------------ 
# Cargar librerias y modulo wiener

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# usage

packages <- c("runjags","rjags","tidyverse","loo")
ipak(packages)

#setwd('C:/Users/aleja/OneDrive/Escritorio/evaluacion/')

load.module("wiener")
source("HDIofMCMC.r") # Load the HDI function

# Cargar base de datos
load('Data/DM_data.Rdata')

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
results1a <- run.jags(model="Data/models/ddm_evaluacion_modelo1.txt",
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
results1b <- run.jags(model="Data/models/ddm_evaluacion_modelo1.txt",
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

results1c <- run.jags(model="Data/models/ddm_evaluacion_modelo1.txt",
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

results1d <- run.jags(model="Data/models/ddm_evaluacion_modelo1.txt",
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

results1e <- run.jags(model="Data/models/ddm_evaluacion_modelo1.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results1e

# GUARDAR RESULTADOS
save(results1a,results1b,results1c,results1d,results1e,file="results/tables/resultsMod1.Rdata")



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

results2a <- run.jags(model="Data/models/ddm_evaluacion_modelo2.txt",
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

results2b <- run.jags(model="Data/models/ddm_evaluacion_modelo2.txt",
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
results2d <- run.jags(model="Data/models/ddm_evaluacion_modelo2.txt",
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

results2e <- run.jags(model="Data/models/ddm_evaluacion_modelo2.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results2e

# GUARDAR RESULTADOS

save(results2a,results2b,results2c,results2d,results2e,file="results/tables/resultsMod2.Rdata")


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

results3a <- run.jags(model="Data/models/ddm_evaluacion_modelo3.txt",
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

results3b <- run.jags(model="Data/models/ddm_evaluacion_modelo3.txt",
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
results3c <- run.jags(model="Data/models/ddm_evaluacion_modelo3.txt",
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

results3d <- run.jags(model="Data/models/ddm_evaluacion_modelo3.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results3d

# GUARDAR RESULTADOS

save(results3a,results3b,results3c,results3d,results3e,file="results/tables/resultsMod3.Rdata")


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
results3e <- run.jags(model="Data/models/ddm_evaluacion_modelo3.txt",
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
results4a <- run.jags(model="Data/models/ddm_evaluacion_modelo4",
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
results4b <- run.jags(model="Data/models/ddm_evaluacion_modelo4.txt",
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
results4c <- run.jags(model="Data/models/ddm_evaluacion_modelo4.txt",
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
results4d <- run.jags(model="Data/models/ddm_evaluacion_modelo4.txt",
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
results4e <- run.jags(model="Data/models/ddm_evaluacion_modelo4.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results4e

# GUARDAR RESULTADOS
save(results4a,results4b,results4c,results4d,results4e,file="results/tables/resultsMod4.Rdata")



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
results5a <- run.jags(model="Data/models/ddm_evaluacion_modelo5.txt",
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
results5b <- run.jags(model="Data/models/ddm_evaluacion_modelo5.txt",
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
results5c <- run.jags(model="Data/models/ddm_evaluacion_modelo5.txt",
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
results5d <- run.jags(model="Data/models/ddm_evaluacion_modelo5.txt",
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
results5e <- run.jags(model="Data/models/ddm_evaluacion_modelo5.txt",
                      monitor=monitor, data=dat, n.chains=3, 
                      inits=c(inits1, inits2, inits3), plots = TRUE,
                      burnin=4000, sample=1000, thin=4,
                      modules=c("wiener"), method=c("parallel"))

results5e

# GUARDAR RESULTADOS
save(results5a,results5b,results5c,results5d,results5e,file="results/tables/resultsMod5.Rdata")



