## Pseudo-code for running the process deviations on all the stocks
##August 21st, 2017
##Jessica Gill

#Step 1:
##Read in data from stock
setwd("C:/Users/jagil/Documents/School/699/Herring Data/Source Files/")

herring_biomass   <- read.csv("Biomass_all.csv")
herring_biomass <- herring_biomass[,-1]
names(herring_biomass) <- c(1:49)
herring_security <- herring_biomass[31:65,1]
herring_catch     <- read.csv("Catch_all.csv")
herring_catch   <- herring_catch[,-1]
names(herring_catch) <- c(1:49)

#Step 2:
##Run logistic growth model using: 
#B(t+1) = B(t-1) + r*B(t-1)*(1-B(t-1)/B0)-C(t-1)
logisticGrowth <- function(B0, r, Binit, times=35)
{
  ##Need to create vector to place all the predicted biomass (B) into:
  B <- vector("numeric", length=length(times))
  
  ##Initialize that the first predict B value is Binit
  B[1] <- Binit
  
  ##Run a for loop to compute each subsequent B value from previous
  #    in logistic growth formula B(t+1) = B(t-1) + r* B(t-1) * (1-B(t-1)/B0)
  for( t in 2:times )
  {
    B[t] <- B[t-1] + r*B[t-1]*( 1.-B[t-1]/B0)
  }
  
  ##Return the B vectors:
  return(B)
  
}


#Step 3:
##Determine B0, r, Binitial (Binit)
#####OK not entirely sure if I did this already?

#Step 4:
##Determine B0, r, Binitial for 1-4 year lag time on it; set observation (sigma_obs) and 
##process error (sigma_proc) standard deviations to at set value (0.4 and 0.1)
##is this even necessary????
sigma_obs  <- 1
sigma_proc <- 1

#Step 5: 
##Create a proc_dev for each year with the lags.  
##Then take the process likelihoods (proc_LL) (just sum product; but how in R?)
##Is this just a random number at the beginning???? 

proc_dev <- rnorm(n = 66, mean = 0, sd = .05)
proc_dev <- proc_dev*sigma_proc
proc_LL  <- sum(proc_dev * proc_dev)

#Step 6: 
##Determine sum of square deviations from (observed biomass - predicted biomass)^2

SoSNeglnLL <- function(param=par, observed=observed)
{
  ##Back transform parameters
  tmp <- c(exp(param[[1]]), exp(param[[2]]), exp(param[[3]])/(1+exp(param[[3]])))
  #print(paste("parameters are",tmp))
  ##Get observed values
  #observed <- herring_biomass[[1]]
  observed<-observed
  ##Get the values from the logistic growth equation
  Bmodel <- logisticGrowth( r=tmp[3], Binit=tmp[2], B0=tmp[1] )
  
  ##Compute Residuals
  #narmobs <- !is.na(observed)
  #resids  <- observed[narmobs==TRUE] - Bmodel[narmobs==TRUE]
  resids<-observed-Bmodel
  ##Get sample size
  N <- length(resids)
  ##Sum of squares calculation
  SoS <- sum(resids * resids, na.rm=TRUE)
  #print(SoS)
  #Step 7: 
  ##Determine negative log-likelihood
  #neglnL = 0.5*sum(sum of square devations)/sigma_obs^2 + proc_LL
  neglnL=(SoS)
  #Log_LL <- 0.5*log(SoS)/(sigma_obs^2) + 0
  #proc_LL
  #neglnL<-(-Log_LL)
  ##Return neglnL
  return( neglnL )
}





#Step 8:
##Optimize the neglnL for the minimum.  Alter B0, Binit, r, and proc_dev.  
##Do this for all the years with lags
# starting values to start the optim guessing at
B0=8952
Binit=7037
r=.368
init_pars<-c(log(B0), log(Binit), log(r/(1-r))) #logit transform between 0 and 1
BmodelPERFECT<-logisticGrowth(r=.4,Binit=7000,B0=9000)
BmodelPERFECT=BmodelPERFECT+rnorm(66,mean=0,sd=500) ##manually adding in error into the data
optimizer <- optim(par = init_pars, 
                   fn = SoSNeglnLL,
                   method = "BFGS",
                   observed = herring_security,
                   hessian = TRUE )

##BUT Running it with the herring data bringing in
##Trouble with the likelihood function so currently minimizing the SoS.

exp(optimizer$par[[1]])
exp(optimizer$par[[2]])
exp(optimizer$par[[3]])/(1+exp(optimizer$par[[3]]))
# optimizer <- optim(par = list(B0=log(B0), Binit=log(Binit), r=log(r)), 
#                    fn = SoSNeglnLL,
#                    method = "Nelder-Mead",
#                    observed = herring_biomass[[1]],
#                    hessian = TRUE )










#Step 9:
##Get a table of the proc_devs for each of the lags.  
##Get a table of all the B0, r, and Binit for each of the lags
cat( "The objective function is: ",    optimizer$value      , "\n")  
cat( "The estimated growth rate is: ", exp(optimizer$par[3]), "\n")
cat( "The Binit is: ",                 exp(optimizer$par[2]), "\n")
cat( "The B0 is: ",                    exp(optimizer$par[1]), "\n")

##Predicted B values
predicted   <- logisticGrowth( r = exp(optimizer$par[3]),
                               Binit = exp(optimizer$par[2]),
                               B0 = exp(optimizer$par[1])
)



#Step 10: 
##Plot the modelled biomass on top of the observed biomass for best fitting model. 
maxObserved <- max( observed, na.rm=TRUE )
maxModel    <- max( predicted )
yMax        <- max( c(maxObserved,maxModel) )

plot(x = 1951+c(1:66), y = BmodelPERFECT,
     xlab = "Year", ylab = "Abundance (mt)",
     ylim = c(0,10000), 
     pch = 17, las = 1)
lines(x = 1951+c(1:66), y = predicted, 
      col = "red", type = "l")

#

#Step 11:
##Plot the process deviations for best fitting model.

#Step 12:
##Plot the production and depletion of the stock for the best fitting model.  
#Production is: Bt-B(t-1)/B(t-1)
#Depletion is: Bt/B0

#Step 13:
##Do this for all 49 stocks

