#
#	Seleccion de variables
#

rm(list=ls())

#install.packages("rjags")
require("rjags")

#install.packages("jags")
require("jags")


#	En Windows
path <- "C:/JCMO.Trabajo/Seminars,Visits&Talks/17-04.Cinvestav/17-04.Cinvestav_Taller/Code/"
path.plot <- "C:/JCMO.Trabajo/Seminars,Visits&Talks/17-04.Cinvestav/17-04.Cinvestav_Taller/Images/"


n <- 500
p <- 20
X <- matrix(rnorm(n*p),ncol=p)
beta <- 2^(0:(1-p))
print(beta)
alpha <- 3
tau <- 2
eps <- rnorm(n,0,1/sqrt(tau))
y <- alpha+as.vector(X%*%beta + eps)



mod <- lm(y~X)
print(summary(mod))


data <- list(y=y,X=X,n=n,p=p)


#   Modelo 1
init_1 <- list(tau=1,alpha=0,beta=rep(0,p))

modelst_1 <- 
"model{
 for(i in 1:n){
  mean[i] <- alpha+inprod(X[i,],beta)
  y[i] ~ dnorm(mean[i],tau)
  }
 for(j in 1:p){
  beta[j]~dnorm(0,0.001)
  }
 alpha~dnorm(0,0.0001)
 tau~dgamma(1,0.001)
 }
"

model_1 <- jags.model(
  textConnection(modelst_1),
  data=data,
  inits=init_1)

update(model_1,n.iter=100)

output <- coda.samples(
  model=model_1,
  variable.names=c("alpha","beta","tau"),
  n.iter=10000,
  thin=1)


print(summary(output))

plot(output)

# Selección estocástica de covariables

data <- list(y=y,X=X,n=n,p=p)

init <- list(tau=1,alpha=0,betaT=rep(0,p),ind=rep(0,p))

modelstring="
model{
for(i in 1:n){
mean[i] <- alpha+inprod(X[i,],beta)
y[i] ~ dnorm(mean[i],tau)
}
for(j in 1:p){
ind[j] ~ dbern(0.2)
betaT[j] ~ dnorm(0,0.001)
beta[j] <- ind[j]*betaT[j]
}
alpha~dnorm(0,0.0001)
tau~dgamma(1,0.001)
}
"

model <- jags.model(
  textConnection(modelstring),
  data=data,
  inits=init)

update(model,n.iter=1000)

output <- coda.samples(model=model,
                       variable.names=c("alpha","beta","ind","tau"),
                       n.iter=10000,
                       thin=1)

print(summary(output))

plot(output)

##

data <- list(y=y,X=X,n=n,p=p)
init <- list(tau=1,taub=1,alpha=0,betaT=rep(0,p),ind=rep(0,p))

modelstring="
model{
for(i in 1:n){
mean[i] <- alpha+inprod(X[i,],beta)
y[i] ~ dnorm(mean[i],tau)
}
for(j in 1:p){
ind[j]~dbern(0.2)
betaT[j]~dnorm(0,taub)
beta[j]<-ind[j]*betaT[j]
}
alpha ~ dnorm(0,0.0001)
tau ~ dgamma(1,0.001)
taub ~ dgamma(1,0.001)
}
"

model <- jags.model(
  textConnection(modelstring),
  data=data,
  inits=init)

update(model,n.iter=1000)


output <- coda.samples(
  model=model,
  variable.names=c("alpha","beta","ind","tau","taub"),
  n.iter=10000,
  thin=1)

print(summary(output))

#plot(output)

##

data <- list(y=y,X=X,n=n,p=p)
init <- list(tau=1,taub=1,pind=0.5,alpha=0,betaT=rep(0,p),ind=rep(0,p))

modelstring_four="
model{
for(i in 1:n){
mean[i] <- alpha+inprod(X[i,],beta)
y[i] ~ dnorm(mean[i],tau)
}
for(j in 1:p){
ind[j] ~ dbern(pind)
betaT[j] ~ dnorm(0,taub)
beta[j] <- ind[j]*betaT[j]
}
alpha ~ dnorm(0,0.0001)
tau ~ dgamma(1,0.001)
taub ~ dgamma(1,0.001)
pind ~ dbeta(2,8)
}
"

model <- jags.model(
  textConnection(modelstring_four),
  data=data,
  inits=init)

update(model,n.iter=1000)

output <- coda.samples(
  model=model,
  variable.names=c("alpha","beta","ind","tau","taub","pind"),
  n.iter=10000,
  thin=1)

print(summary(output))

plot(output)
