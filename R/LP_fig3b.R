#################################
## GERTLER-KARADI - Figure 3A & B
#################################

# This code replicates the IRFs in Figure 3(A) & 3(B) from
# "Macroeconomic Shocks and Their Propagation" by Valerie Ramey 
# in the 2016 Handbook of Macroeconomics. 
#
# Required data: Monetarydata.xlsx
#
#   Source: http://econweb.ucsd.edu/~vramey/research.html#data
#   (click on 'Monetary Shocks' to download the zip file)
#
# Make sure to install all packages before running the code
# and to set the working directory 


rm(list=ls())

library(readxl)
library(dynlm) # helps with time series regressions
library(vars)
library(sandwich) # needed for Newey-West s.e.


# Write your directory path here:

dir <- "U:/tf/Caldara/darioprojects/Courses/Georgetown2018/Codes/localprojections"
setwd(dir)

# Load the data from excel
vardata <- read_excel('data/Monetarydat.xlsx', sheet = 'Monthly')
colnames(vardata) <- tolower(colnames(vardata)) # makes variable name lower case

# Create a monthly date column
vardata$dates <- seq(from=as.Date("1959-01-01"), to=as.Date("2015-12-01"), by="month")


# Filter the data by sample date and save the relevant varibles to a new data frame
# For variable definitions, see the 'Readme' section of Monetarydat.xlsx
# NOTE: dates are not needed for the VAR function

gertler <- subset(vardata, 
                dates >= as.Date('1990-01-01') & dates <= as.Date('2012-06-01'), 
                select = c("dates", "lip", "lcpi", "gs1", "ebp", "ff4_tc"))


# Define the data as a time series to use lags and leads in the regression
gertlerts <- ts(gertler, start = c(1990,1), end = c(2012,06), frequency = 12) 

################################################################
################################################################
# Here we create matrices of NAs to be filled-in with regression
# coefficients and standard errors. This will make it easier to
# produce figures later on
periods <- 48
varnames <- colnames(gertlerts)[2:5] # create vector of variable names
cnames <- character(3*length(varnames))

responses <- data.frame(matrix(NA, nrow = periods, ncol = 3*length(varnames)))

for (i in 1:length(varnames)) {
  b <- paste0("b", varnames[i])
  cnames[i] <- b
}
for (i in 1:length(varnames)) {
  up <- paste0("up90b", varnames[i])
  cnames[i+length(varnames)] <- up
}
for (i in 1:length(varnames)) {
  low <- paste0("low90b", varnames[i])
  cnames[i+2*length(varnames)] <- low
}

colnames(responses) <- cnames

################################################################
################################################################

# The 'dynlm' function allows for a simpler implementation of time series analysis,
# especially via the L() operator for lagged variables. E.g. L(lip, 1:2) includes both
# 1 and 2 period lags for industrial production in the model

# We need to perform the regression using several different independent variables.
# To do this efficiently, we use the lapply() function, as we cannot simply loop over
# the variable names: R will try to regress a string on the other variables.
#
# If you are unfamiliar with the 'apply' family of functions in R it is worth reading up on. 
# https://www.datacamp.com/community/tutorials/r-tutorial-apply-family
# 
# If the function looks confusing, just know that we are running this code for each 
# variable:
#
# dynlm(L(lip, -i) ~  L(lip, 1:2) + L(gs1, 1:2) +
#                L(lcpi, 1:2) + L(ebp, 1:2) + L(ff4_tc, 0:2),
#              data = gertlerts)



for (i in 0:periods) {
  
  mods_responses <- lapply(varnames, function(x) { 
    form <- as.formula(paste0("L(", x, ",",-i, ") ~  L(lip, 1:2) + L(gs1, 1:2) +
                              L(lcpi, 1:2) + L(ebp, 1:2) + L(ff4_tc, 0:2)"))
    dynlm(form, data = gertlerts)
  })
  
  
  # To get Newey-West s.e. simply use the coeftest() function, specifiying NeweyWest for vcov.
  newey <- lapply(mods_responses, function(x) {coeftest(x, vcov. = NeweyWest(x, lag = (i+1), prewhite = FALSE))})
  
  
  # Extract the coefficients and s.e. of the  for each variable from the list
  for (j in 1:length(varnames)) {
    beta <- newey[[j]][10,1] # coefficient
    se <- newey[[j]][10,2] # S.E.
    up90 <- beta + 1.68*se
    low90 <- beta - 1.68*se
    responses[i+1,paste0("b", varnames[j])] <- beta
    responses[i+1,paste0("up90b", varnames[j])] <- up90
    responses[i+1,paste0("low90b", varnames[j])] <- low90
  }
}

##############################################################
##############################################################

# This code produces shaded areas representing the 90% CIs.
# See the commented code below to use dashed lines for the 
# confidence bands instead.

# NOTE: Using the polygon() function for shading between
# curves isn't necessarily intuitive. Two sets of x coordinates 
# are needed to draw the polygon.
# For a good explaination of how this function works, see this post:
# http://www.alisonsinclair.ca/2011/03/shading-between-curves-in-r/

###############################################################
###############################################################

pdf('figures/ramey_figure_3b_R.pdf', width = 10, height = 5)
par(mfrow=c(2,2), mar=c(2,2,2,2))

plot(responses$bgs1, type = "l", main = "One-year rate", 
     xlab = '', ylab = '',
     ylim = c(min(responses$low90bgs1), max(responses$up90bgs1))*1.1)
polygon(x = c(seq_along(responses$bgs1), rev(seq_along(responses$bgs1))), 
        y = c(responses$up90bgs1, rev(responses$low90bgs1)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(responses$bgs1, lty=1, col = "black", lwd = 2)


plot(responses$blip, type = "l", main = "Industrial production", 
     xlab = '', ylab = '',
     ylim = c(min(responses$low90blip), max(responses$up90blip))*1.1)
polygon(x = c(seq_along(responses$blip), rev(seq_along(responses$blip))), 
        y = c(responses$up90blip, rev(responses$low90blip)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(responses$blip, lty=1, col = "black", lwd = 2)


plot(responses$bebp, type = "l", main = "Excess bond premium", 
     xlab = '', ylab = '',
     ylim = c(min(responses$low90bebp), max(responses$up90bebp))*1.1)
polygon(x = c(seq_along(responses$bebp), rev(seq_along(responses$bebp))), 
        y = c(responses$up90bebp, rev(responses$low90bebp)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(responses$bebp, lty=1, col = "black", lwd = 2)



plot(responses$blcpi, type = "l", main = "CPI", 
     xlab = '', ylab = '',
     ylim = c(min(responses$low90blcpi), max(responses$up90blcpi))*1.1)
polygon(x = c(seq_along(responses$blcpi), rev(seq_along(responses$blcpi))), 
        y = c(responses$up90blcpi, rev(responses$low90blcpi)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(responses$blcpi, lty=1, col = "black", lwd = 2)



dev.off()
