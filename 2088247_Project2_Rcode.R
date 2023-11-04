library(geoR)
library(fields)
#Setting file directory
setwd("C:/Users/wendy/Desktop/Masters/03 STAT7006A/Project 2")

#Plotting the Study Area Map
plot(sic.100, borders = sic.borders)
plot(sic.367, borders = sic.borders)

#Looking at the Variogram
vario.b<- variog(sic.100,option =c ("bin", "cloud", "smooth"),
                 bin.cloud=TRUE)
plot(vario.b)
vario.c <- variog(sic.100, op="cloud")
bplot.xy(vario.c$u,vario.c$v, breaks=vario.b$u,col="grey80",
         lwd=2,cex=0.1,outline=FALSE)
vario4<-variog4(sic.100)
plot(vario4,same=FALSE)

#Variogram fitting
vario.em <- variog(sic.100, coords = sic.100$coords, 
                   data = sic.100$data, option = "bin")
plot(vario.em)
vario.sphe<-(variofit(vario.em,cov.model= "spherical", 
                      ini.cov.pars=c(range = 15000, sill = 200)))
vario.expo<-(variofit(vario.em,cov.model= "exp", ini.cov.pars=c(15000, 200)))
vario.linear <- (variofit(vario.em, cov.model = "linear", 
                          ini.cov.pars = c(15000, 200)))

#Plot the variograms
par(mfrow=c(2,2), mar=c(3,3,1,1), mgp =c (2,1,0))
plot(vario.em,main="Spherical")
lines.variomodel(cov.model="spherical",cov.pars=c(15000,100), 
                 nug = 0, max.dist=350)
plot(vario.em,main="Exponential")
lines.variomodel(cov.model="exp",cov.pars=c(15000,100),
                 nug=0,max.dist=350)
plot.geodata(sic.100, borders = sic.borders)

#Preforming Kriging
Sic_pred.grid <- expand.grid(seq(0,350, l=51),seq (0,220, l=51))
colour.palette <- colorRampPalette(c("blue", "lightblue",
                                     "orange", "red"),space="rgb")
Sic_kriging <- krige.conv(sic.100,
                          coords = sic.100$coords, data = sic.100$data,
                          locations = Sic_pred.grid,
                          krige=krige.control(cov.model= "spherical",
                                              cov.pars=c(15000, 25)))
image(Sic_kriging, loc = Sic_pred.grid,col = colour.palette(20), 
      xlab="Coord X", ylab="Coord Y",      
      borders = sic.borders , main="Estimation")
image(Sic_kriging, Sic_kriging$krige.var, loc = Sic_pred.grid, 
      col=colour.palette(20),
      xlab="Coord X",ylab="Coord Y",borders=sic.borders,
      main="Kriging variance")
str(Sic_kriging)


#Estimated versus Observed Values
Sic_krigingsample<- krige.conv(sic.100, loc = sic.100$coords,
                 krige=krige.control(cov.model="spherical",cov.pars=c(160,47)))
sic_krigingctsample<- krige.conv(sic.100, loc = sic.367$coords,
                 krige=krige.control(cov.model="spherical",cov.pars=c(160,47)))
plot(sic.100$data,Sic_krigingsample$predict,xlab="Observed",ylab="Estimated", 
     main="Control sample")
abline(a=0,b=1,col="red")
plot(sic.367$data, sic_krigingctsample$predict, xlab="Observed",ylab="Estimated",
     main="Control")
abline(a=0,b=1,col="red")

#Transform Geodata to have closer to normal distribution and then plot new graph
plot.geodata(sic.100,bor=sic.borders,lambda=0.5)
vario.ext<- variog(sic.100,option="bin",lambda=0.5)
plot(vario.ext, main="Matern Transformed")
lines.variomodel(cov.model = "matern",cov.pars =c (105, 36), nug = 6.9,
                 max.dist = 300,kappa = 1, lty = 1)
transformedkriging <- krige.conv(sic.100, locations = Sic_pred.grid, 
                                 krige = krige.control(cov.model = "matern", 
                                                       cov.pars = c(105, 36), 
                                                       kappa = 1, nugget = 6.9, 
                                                       lambda = 0.5))
image(transformedkriging, loc = Sic_pred.grid, col= colour.palette(20), 
      xlab="Coord X", ylab="Coord Y", borders=sic.borders, 
      main="Estimation")
image(transformedkriging, transformedkriging$krige.var, loc = Sic_pred.grid, 
      col = colour.palette(20), xlab="Coord X", ylab="Coord Y", 
      borders=sic.borders, main="Kriging Variance")

#Transformed data estimated verses observed values
transkrigsample<- krige.conv(sic.100, loc = sic.100$coords,
                  krige=krige.control(cov.model="matern",
                                      cov.pars=c(160,47),
                                      kappa=1,nugget=6.9,lambda=0.5))
transkrigctsample<- krige.conv(sic.100, loc = sic.367$coords,
                  krige=krige.control(cov.model="matern",
                                      cov.pars=c(160,47),
                                      kappa=1,nugget=6.9,lambda=0.5))
plot(sic.100$data, transkrigsample$predict,
     xlab="Observed",ylab="Estimated", main="Sample")
abline(a=0,b=1,col="red")
plot(sic.367$data,transkrigctsample$predict,
     xlab="Observed",ylab="Estimated", main="Control sample")
abline(a=0,b=1,col="red")

#Calculating the root mean square error of the datasets 
sqrt(mean((sic.367$data - sic_krigingctsample$predict)^2))
sqrt(mean((sic.367$data - transkrigctsample$predict)^2))   
summary(sic.367)
summary(sic_krigingctsample)
summary(transkrigctsample)

knitr::stitch_rhtml("Sic data stuff.R")
