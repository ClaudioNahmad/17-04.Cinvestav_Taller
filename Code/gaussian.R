#
# Normal distribution
#

rm(list = ls())

# install.packages("TeachingDemos")

require("TeachingDemos")

#	En Windows
path <- "C:/JCMO.Trabajo/Seminars,Visits&Talks/17-04.Cinvestav/17-04.Cinvestav_Taller/Code/"
path.plot <- "C:/JCMO.Trabajo/Seminars,Visits&Talks/17-04.Cinvestav/17-04.Cinvestav_Taller/Images/"


# Bivariada

# x: 2x1 vector, mu: 2x1 mean vector, Sigma: 2x2 covariance matrix
bivariate.normal <- function(x, mu, Sigma){
  exp(-.5*t(x-mu)%*%solve(Sigma)%*%(x-mu))/sqrt(2*pi*det(Sigma))
}
mu <- c(0,0)
Sigma <- matrix(c(1,.5,.5,1), nrow=2)
x <- y <- seq(-3, 3, len=25)

# Evalucion 
z <- outer(x, y, 
           FUN=function(x, y, ...){
             apply(cbind(x,y), 1, bivariate.normal, ...)
           },
           mu=c(0,0), Sigma=Sigma)

# Contour
filled.contour(x,y,z, main="", xlab="X_1", ylab="X_2", color.palette=topo.colors)
jpeg(paste(path.plot,"gaussian_contour.jpg", sep = ""), width=700, height=500)
filled.contour(x,y,z, main="", xlab="X_1", ylab="X_2", color.palette=topo.colors)
dev.off()


# Surface
jpeg(paste(path.plot,"gaussian_surface.jpg", sep = ""), width=700, height=500)
persp(x, y, z, shade=.75, col="red", main="Densidad Bivariada") 
dev.off()

persp(x, y, z, shade=.75, col="red", main="Densidad Bivariada") 
rotate.persp(x, y, z)
