#
#	Modelo Bernoulli-beta
#

rm(list = ls())

# install.packages("LearnBayes")

require("LearnBayes")

#	En Windows
path <- "C:/JCMO.Trabajo/Seminars,Visits&Talks/17-04.Cinvestav/17-04.Cinvestav_Taller/Code/"
path.plot <- "C:/JCMO.Trabajo/Seminars,Visits&Talks/17-04.Cinvestav/17-04.Cinvestav_Taller/Images/"

# Licitación de distribuciones iniciales sobre la proporción
quantile1 <- list(p=0.5, x=0.85) 	# licitacion para la mediana inicial en 0.85
quantile2 <- list(p=0.99999,x=0.95) # licitacion para el cuantil 0.95 de la inicial
quantile3 <- list(p=0.00001,x=0.05) # licitacion para el cuantil 0.05 de la inicial

#------------------------------
#	Funcion
prior.Beta <- function(quantile1,quantile2,quantile3){
  # encuentra los cuantiles especificados por los argumentos 
  # "quantile1", "quantile2" y "quantile3"
  quantile1_p <- quantile1[[1]]
  quantile1_q <- quantile1[[2]]
  quantile2_p <- quantile2[[1]]
  quantile2_q <- quantile2[[2]]
  quantile3_p <- quantile3[[1]]
  quantile3_q <- quantile3[[2]]
  # encuentra la distriución inicial de beta usando
  #	a) "quantile1" y "quantile2"
  priorA <- beta.select(quantile1,quantile2)
  priorA_a <- priorA[1]
  priorA_b <- priorA[2]
  #	b) "quantile1" y "quantile3"
  priorB <- beta.select(quantile1,quantile3)
  priorB_a <- priorB[1]
  priorB_b <- priorB[2]
  # encuentra la mejor distribución beta posible
  diff_a <- abs(priorA_a - priorB_a)
  diff_b <- abs(priorB_b - priorB_b)
  step_a <- diff_a / 100
  step_b <- diff_b / 100
  if(priorA_a < priorB_a){
    start_a <- priorA_a
    end_a <- priorB_a
  }
  else{
    start_a <- priorB_a
    end_a <- priorA_a
  }
  if(priorA_b < priorB_b){
    start_b <- priorA_b
    end_b <- priorB_b
  }
  else{
    start_b <- priorB_b
    end_b <- priorA_b
  }
  steps_a <- seq(from=start_a, to=end_a, length.out=1000)
  steps_b <- seq(from=start_b, to=end_b, length.out=1000)
  max_error <- 10000000000000000000
  best_a <- 0
  best_b <- 0
  for(a in steps_a){
    for(b in steps_b){
      # priorC <-> beta(a,b)
      # encuentra los cuantiles 
      #	"quantile1_q", "quantile2_q", "quantile3_q" de priorC
      priorC_q1 <- qbeta(c(quantile1_p), a, b)
      priorC_q2 <- qbeta(c(quantile2_p), a, b)
      priorC_q3 <- qbeta(c(quantile3_p), a, b)
      priorC_error <- abs(priorC_q1-quantile1_q) +
        abs(priorC_q2-quantile2_q) +
        abs(priorC_q3-quantile3_q)
      if(priorC_error < max_error){
        max_error <- priorC_error
        best_a <- a
        best_b <- b
      }
    }
  }
  print(paste("La licitacion inicial es a=",best_a,"b=",best_b))
}
#
#------------------------------

#	Licitación de la distribución inicial Beta
prior.Beta(quantile1,quantile2,quantile3)

#	Gráfica de la distribución inicial licitada
curve(dbeta(x, 3.8, 0.91))
jpeg(paste(path.plot,"bernoulli_beta_prior.jpg", sep = ""), width=700, height=500)
curve(dbeta(x, 3.8, 0.91))
dev.off()

#------------------------------
#	Verosimilitud
#------------------------------

#------------------------------
#	Función
Likelihood.Binomial <- function(successes, total){
  curve(dbinom(successes,total,x)) 
}
#
#------------------------------

successes <- 60
total <- 90
Likelihood.Binomial(successes, total)

jpeg(paste(path.plot,"bernoulli_beta_likelihood.jpg", sep = ""), width=700, height=500)
Likelihood.Binomial(successes, total)
dev.off()

#------------------------------
#	Aprendizaje
#------------------------------

#------------------------------
#	Función
Aprendizaje.Binomial <- function(successes, total, a, b){
  # Actualización de la información
  likelihood_a <- successes + 1
  likelihood_b <- total - successes + 1
  posterior_a <- a + successes
  posterior_b = b + total - successes
  theta <- seq(0.005, 0.995, length = 500)
  prior <- dbeta(theta, a, b)
  likelihood <- dbeta(theta, likelihood_a, likelihood_b)
  posterior <- dbeta(theta, posterior_a, posterior_b)
  m <- max(c(prior, likelihood, posterior))
  plot(theta, posterior, type = "l", ylab = "Densidad", lty = 2, lwd = 3,
       xlim = c(0,1), ylim = c(0, m), col = "red")
  lines(theta, likelihood, lty = 1, lwd = 3, col = "blue")
  lines(theta, prior, lty = 3, lwd = 3, col = "green")
  legend(x=0.8, y=m, c("Prior", "Verosimilitud", "Posterior"), 
         lty = c(3, 1, 2), lwd = c(3, 3, 3), 
         col = c("green", "blue", "red"))
  # Resumen:
  calcBetaMode <- function(aa, bb){
    BetaMode <- (aa - 1)/(aa + bb - 2)
    return(BetaMode)
  }
  calcBetaMean <- function(aa, bb){
    BetaMean <- (aa)/(aa + bb)
    return(BetaMean)
  } 
  calcBetaSd <- function(aa, bb){
    BetaSd <- sqrt((aa * bb)/(((aa + bb)^2) * (aa + bb + 1)))
  }
  prior_mode <- calcBetaMode(a, b)
  likelihood_mode <- calcBetaMode(likelihood_a, likelihood_b)
  posterior_mode <- calcBetaMode(posterior_a, posterior_b)
  prior_mean <- calcBetaMean(a, b)
  likelihood_mean <- calcBetaMean(likelihood_a, likelihood_b)
  posterior_mean <- calcBetaMean(posterior_a, posterior_b)
  prior_sd <- calcBetaSd(a, b)
  likelihood_sd <- calcBetaSd(likelihood_a, likelihood_b)
  posterior_sd <- calcBetaSd(posterior_a, posterior_b)
  print("Moda:")
  print(paste("Prior=",prior_mode,
              ", Verosimilitud=",likelihood_mode,
              ", Posterior=",posterior_mode))
  print("Media:")
  print(paste("Prior=",prior_mean,
              ", Verosimilitud=",likelihood_mean,
              ", Posterior=",posterior_mean))
  print("Desviacion estandar:")
  print(paste("Prior=",prior_sd,
              ", Verosimilitud=",likelihood_sd,
              ", Posterior=",posterior_sd))
}
#
#------------------------------

#	Aprendizaje
successes <- 60
total <- 90
a <- 3.8
b <- 0.91

Aprendizaje.Binomial(successes, total, a, b)

jpeg(paste(path.plot,"bernoulli_beta_posterior.jpg", sep = ""), width=700, height=500)
Aprendizaje.Binomial(successes, total, a, b)
dev.off()

#
# -- FIN --