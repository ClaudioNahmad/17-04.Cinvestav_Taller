#
#	Selección de variables con LASSO
#

options(width=200, digits=6, digits.secs=6)
rm(list=ls())

#install.packages("plyr")
#install.packages("ggplot2")
#install.packages("grid")
#install.packages("reshape")   
#install.packages("scales")   
#install.packages("distr")
#install.packages("optimx")
#install.packages("foreach")
#install.packages("doMC")
#install.packages("tikzDevice")

require(plyr)
require(ggplot2)
require(grid)
require(reshape)   
require(scales)   
require(distr)
require(optimx)

library(foreach)
library(doMC)

registerDoMC(4)

library(tikzDevice)

print(options('tikzLatexPackages'))
options(tikzLatexPackages =
        c("\\usepackage{tikz}\n",
          "\\usepackage[active,tightpage,psfixbb]{preview}\n",
          "\\PreviewEnvironment{pgfpicture}\n",
          "\\setlength\\PreviewBorder{0pt}\n",
          "\\usepackage{amsmath}\n",
          "\\usepackage{xfrac}\n"
          )
        )
setTikzDefaults(overwrite = FALSE)
print(options('tikzLatexPackages'))

scl.str.DAT_DIR <- "C:/JCMO.Trabajo/@Mis.Cursos/2016-B_Fundamentos Estadistica/"
scl.str.FIG_DIR <- "C:/JCMO.Trabajo/@Mis.Cursos/2016-B_Fundamentos Estadistica/lasso/"
set.seed(12356)

# Plot LASSO estimates

scl.flt.LAM    <- 1.0
vec.flt.R      <- seq(-2, 2, by = 0.02)

mat.df.PLOT <- data.frame(r     = vec.flt.R,
                          muHat = sign(vec.flt.R) * (abs(vec.flt.R) - scl.flt.LAM) * ((abs(vec.flt.R) - scl.flt.LAM) > 0)
                          )


theme_set(theme_bw())

scl.str.RAW_FILE <- 'plot--lasso-coefficient-estimates--24sep2016'
scl.str.TEX_FILE <- paste(scl.str.RAW_FILE,'.tex',sep='')
scl.str.PDF_FILE <- paste(scl.str.RAW_FILE,'.pdf',sep='')
scl.str.AUX_FILE <- paste(scl.str.RAW_FILE,'.aux',sep='')
scl.str.LOG_FILE <- paste(scl.str.RAW_FILE,'.log',sep='')

tikz(file = scl.str.TEX_FILE, height = 2, width = 7, standAlone=TRUE)

obj.gg2.PLOT <- ggplot()
obj.gg2.PLOT <- obj.gg2.PLOT + scale_colour_brewer(palette="Set1")
obj.gg2.PLOT <- obj.gg2.PLOT + geom_path(data = mat.df.PLOT,
                                         aes(x = r,
                                             y = muHat
                                             ),
                                         size   = 3.00,
                                         alpha  = 0.75
                                         )
obj.gg2.PLOT <- obj.gg2.PLOT + coord_cartesian(xlim = c(-2, 2), ylim = c(-1, 1))
obj.gg2.PLOT <- obj.gg2.PLOT + scale_x_continuous(breaks = c(-2, -1, 0, 1, 2))
obj.gg2.PLOT <- obj.gg2.PLOT + scale_y_continuous(breaks = c(-1, 0, 1))
obj.gg2.PLOT <- obj.gg2.PLOT + xlab("$r$")
obj.gg2.PLOT <- obj.gg2.PLOT + ylab("$\\hat{\\mu}(r)$")
obj.gg2.PLOT <- obj.gg2.PLOT + annotate("text", x = 1, y = -0.75, size = 6, label = "LASSO estimate of $\\mu^\\star$")
obj.gg2.PLOT <- obj.gg2.PLOT + annotate("text", x = -0.75, y = 0.15, size = 3.5, label = "$\\lambda = 1.0{\\scriptstyle \\%}$")
obj.gg2.PLOT <- obj.gg2.PLOT + theme(plot.margin      = unit(c(0.15,0.15,0.15,0.15), "lines"),
                                     axis.title       = element_text(size = 10),
                                     axis.text        = element_text(size = 10),
                                     plot.title       = element_blank(),
                                     panel.grid.minor = element_blank()
                                     )

print(obj.gg2.PLOT)
dev.off()

system(paste('lualatex', file.path(scl.str.TEX_FILE)), ignore.stdout = TRUE)
system(paste('rm ', scl.str.TEX_FILE, sep = ''))
system(paste('mv ', scl.str.PDF_FILE, ' ', scl.str.FIG_DIR, sep = ''))
system(paste('rm ', scl.str.AUX_FILE, sep = ''))
system(paste('rm ', scl.str.LOG_FILE, sep = ''))











# Estimate beta hat


vec.flt.SCALE  <- c(1, 2, 4, 8, 16)
vec.flt.R      <- seq(-2, 2, by = 0.02)
scl.int.NUM_R  <- length(vec.flt.R)


mat.df.PLOT <- foreach(s = 1:5, .combine = "rbind") %do% {
    ## for (s in 1:5) {
    
    scl.flt.SCALE    <- vec.flt.SCALE[s]
    scl.flt.LOCATION <- 1/3.5
    scl.flt.SIG_MU_H <- scl.flt.LOCATION * sqrt(scl.flt.SCALE)
    scl.flt.SIG_MU_L <- scl.flt.LOCATION / sqrt(scl.flt.SCALE)
    scl.flt.SIG_EP   <- scl.flt.LOCATION
    
    obj.fun.LOG_LIKELIHOOD <- function(MU, R) {
        
        scl.flt.FIT <- (1/sqrt(2 * pi * scl.flt.SIG_EP^2)) * exp(- (R - MU)^2/(2 * scl.flt.SIG_EP^2))
        
        scl.flt.PRIOR_H <- (1/sqrt(2 * pi * scl.flt.SIG_MU_H^2)) * exp(- (MU - 0)^2/(2 * scl.flt.SIG_MU_H^2))
        scl.flt.PRIOR_L <- (1/sqrt(2 * pi * scl.flt.SIG_MU_L^2)) * exp(- (MU - 0)^2/(2 * scl.flt.SIG_MU_L^2))
        
        scl.flt.LOG_LIKELIHOOD <- log(scl.flt.FIT) + log(scl.flt.PRIOR_H + scl.flt.PRIOR_L)
        
        return(-scl.flt.LOG_LIKELIHOOD)
        
    }
    
    mat.df.PANEL <- foreach(r=1:scl.int.NUM_R, .combine = "rbind") %dopar% {
        
        scl.flt.R       <- vec.flt.R[r]
        obj.opt.RESULTS <- optimx(par    = c(0),
                                  fn     = obj.fun.LOG_LIKELIHOOD,
                                  method = "BFGS",
                                  R      = c(scl.flt.R)
                                  )
        scl.flt.MU_HAT  <- obj.opt.RESULTS$p1
        mat.df.TEMP     <- data.frame(r = scl.flt.R, muHat = scl.flt.MU_HAT)
        return(mat.df.TEMP)
        
    }
    mat.df.PANEL$scale <- scl.flt.SCALE
    
    return(mat.df.PANEL)
    
}

mat.df.PLOT$scale <- factor(mat.df.PLOT$scale,
                            levels = c(1,2,4,8,16),
                            labels = paste("$\\sfrac{\\overline{\\sigma}_{\\mu}}{\\underline{\\sigma}_{\\mu}} = ", c(1,2,4,8,16), "$", sep = "")
                            )








# Plot results


theme_set(theme_bw())

scl.str.RAW_FILE <- 'plot--bayesian-lasso-intuition--24sep2016'
scl.str.TEX_FILE <- paste(scl.str.RAW_FILE,'.tex',sep='')
scl.str.PDF_FILE <- paste(scl.str.RAW_FILE,'.pdf',sep='')
scl.str.AUX_FILE <- paste(scl.str.RAW_FILE,'.aux',sep='')
scl.str.LOG_FILE <- paste(scl.str.RAW_FILE,'.log',sep='')

tikz(file = scl.str.TEX_FILE, height = 2, width = 7, standAlone=TRUE)

obj.gg2.PLOT <- ggplot()
obj.gg2.PLOT <- obj.gg2.PLOT + scale_colour_brewer(palette="Set1")
obj.gg2.PLOT <- obj.gg2.PLOT + geom_path(data = mat.df.PLOT,
                                         aes(x        = r,
                                             y        = muHat,
                                             group    = scale
                                             ),
                                         size   = 3.00,
                                         alpha  = 0.75
                                         )
obj.gg2.PLOT <- obj.gg2.PLOT + coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2))
obj.gg2.PLOT <- obj.gg2.PLOT + scale_x_continuous(breaks = c(-2, 0, 2))
obj.gg2.PLOT <- obj.gg2.PLOT + scale_y_continuous(breaks = c(-2, -1, 0, 1, 2))
obj.gg2.PLOT <- obj.gg2.PLOT + xlab("$r$")
obj.gg2.PLOT <- obj.gg2.PLOT + ylab("$\\hat{\\mu}(r)$")
obj.gg2.PLOT <- obj.gg2.PLOT + facet_wrap(~ scale, ncol = 5)
obj.gg2.PLOT <- obj.gg2.PLOT + theme(plot.margin      = unit(c(0.15,0.15,0.15,0.15), "lines"),
                                     axis.title       = element_text(size = 10),
                                     axis.text        = element_text(size = 10),
                                     plot.title       = element_blank(),
                                     panel.grid.minor = element_blank(),
                                     legend.position  = c(0.75, 0.25)
                                     )

print(obj.gg2.PLOT)
dev.off()

system(paste('lualatex', file.path(scl.str.TEX_FILE)), ignore.stdout = TRUE)
system(paste('rm ', scl.str.TEX_FILE, sep = ''))
system(paste('mv ', scl.str.PDF_FILE, ' ', scl.str.FIG_DIR, sep = ''))
system(paste('rm ', scl.str.AUX_FILE, sep = ''))
system(paste('rm ', scl.str.LOG_FILE, sep = ''))