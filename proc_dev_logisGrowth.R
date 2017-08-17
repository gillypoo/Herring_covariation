##Setting up a process deviations loop and function for herring data to get recruitment.
##Working off of fitLogisticGrowth-LogL.r
##Using the Biomass_all.csv
setwd("C:/Users/jagil/Documents/School/699/Herring Data/Source Files/")


#  logisModel  - logistic growth model 
#            r - intrinsic growth rate (to be estimated)
#        Binit - initial value for abundance (from data) 
#           B0 - carrying capacity (aka K)
#  proc_errors - process errors (recruitment proxy)
#        times - times at which to compute model predictions
logisModel <- function( r=0.05, Binit=100, B0=1000, 
                        proc_errors=vector("numeric", length = times),
                        times=1:66 )  ##SHOULD  THE  PROC_ERRORS GO HERE OR BE IT'S OWN OBJECT BELOW?
{ 
  # a. create vector B to hold predicted values
  B <- vector("numeric",length=length(times))
  
  # b. initialize first B with observed value (stored globally)
  B[1] <- Binit
  
  # c. for loop to compute each subsequent B value from previous
  #    in logistic growth formula B(t+1) = B(t-1) + r* B(t-1) * (1-B(t-1)/B0)
  for( t in 2:length(times) )
  {
    B[t] <- B[t-1] + r*B[t-1]*( 1.-B[t-1]/B0 )
  }
  
  # return B vector
  return( B )
} 


# 2. Create a goodness-of-fit function based on two arguments: par, ...
#        goodFit - function to get goodness of fit given par and whatever else... 
#            par - parameter to be estimated 
#       observed - vector of observed values
#           ...  - arguments passed through to other functions
goodFit <- function( par, observed )
{
  
  # back-transform the parameters
  tmp    <- exp( par )
  # a. get predicted values for time-series from expModel() <---- IS THIS SOMETHING I NEED????
  Nmodel <- logisModel( r=tmp[1], Binit=tmp[2], B0=tmp[3] )
  
  # b. compute log-residual function, residuals
  idx    <- !is.na(observed)
  resids <- log(Nmodel[idx]) - log(observed[idx])
  
  # get sample size
  N      <- length( resids )
  
  # c. sum-of-squares, SS
  SS     <- sum( resids*resids, na.rm=TRUE )
  
  # calculate conditional MLE of sigma
  sigma  <- sqrt( SS/N )
  
  # final negative log-likelihood
  logL   <- (N/2.)*log(SS)
  
  # return negative log-likelihood
  return( logL )
}

#----------------------------------------------------
# this will load the herring time series
herring        <- read.csv("Biomass_all.csv") 
herring        <- herring[,-1]
names(herring) <- c(1:49)


# extract 1 time-series of observed values ##I DON"T THINK I NEED THIS PART
stock1    <- "1"
rInterval <- c(0.005,0.2)
observed  <- herring[[stock1]]
sigma     <- 0.4

# Now use optim to find lowest SS
# Using log-scale to constrain and scale 
# parameters r, Binit
optR <- optim( par=log(c(0.07,4000,35000)), 
               fn=goodFit,
               method="Nelder-Mead", 
               observed=observed,
               control=list(maxit=200,fnscale=1),
               hessian=TRUE 
              )


cat( "The objective function is: ",    optR$value, "\n")  ##WHAT DOES THE OBJECTIVE FUNCTION MEAN???
cat( "The estimated growth rate is: ", exp(optR$par[1]), "\n")
cat( "The Binit is: ",                    exp(optR$par[2]), "\n")
cat( "The B0 is: ",     exp(optR$par[3])      )



# make plot of the fit, starting with observed values
maxObserved <- max( observed, na.rm=TRUE )
# predicted values
predicted   <- logisModel( r=exp(optR$par[1]),
                           Binit=exp(optR$par[2]),
                           B0 = exp(optR$par[3]) 
)
maxModel    <- max( predicted )
yMax        <- max( c(maxObserved,maxModel) )

plot( x=1951+c(1:66), y=observed, type="p",
      ylim=c(0,1.2*yMax),
      ylab=paste("Abundance (mt)",sep=""),
      xlab="Year",
      las=1 )

lines( x=1951+c(1:66), y=predicted, col="blue" )



############
#After first trial run, it worked... sort of... 
#Here is the output:
  # "The objective function is:  15.89995 
  #The estimated growth rate is:  0.1094032 
  #The Binit is:  18973.78 
  #The B0 is:  4479.555
  #Error in xy.coords(x, y, xlabel, ylabel, log) : 
  #  'x' and 'y' lengths differ
  #In addition: Warning messages:
  # 1: In log(Nmodel[idx]) : NaNs produced
  # 2: In log(Nmodel[idx]) : NaNs produced"
#
##Had some errors with graphics, and log model.  NaNs produced=prolly because there's missing
##numbers in there? 
##Also, need to loop over each of the stocks!  That loop either doesn't function, or most likely
##doesn't exist at this iteration. 




##I think what the problem is is that there is a loop at the beginning (in logisModel), but there
##isn't one in the second part where I actually optimize the model.  I thinkkkkk???