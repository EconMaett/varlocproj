####################
## CEE - Figure 1
####################

# This code replicates the IRFs in Figure 1 from
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

library(dynlm) # helps with time series regressions
library(vars)
library(sandwich) # needed for Newey-West s.e.



# Write your directory path here:

dir <- "U:/tf/Caldara/darioprojects/Courses/Georgetown2018/Codes/VAR_STATA_R"
setwd(dir)

# Load the data from excel
vardata <- read.delim('data/dataGPR.txt', header = FALSE)
colnames(vardata) <- c("gpr", "epu", "sp500", "ip")

# Creat monthly date column
#vardata$dates <- seq(from=as.Date("1985-01-01"), to=as.Date("2015-12-01"), by="month")

# Estimate the VARs
var1 <- VAR(vardata, p =3) # Set the number of lags using 'p'


# View summary statistics for the estimated equations, as well as
# the covariance and the correlation matrices of residuals 
summary(var1)


# Use the irf() function to produce impulse responses for the estimaed VAR
# 'n.ahead' sets the length of the IRF in periods (default is 10)
# 'ci' sets the confidence bands; we use 68% here (default is 95%)
# 'runs' sets the number of runs for bootstrapping (default is 100)
irf1 <- irf(var1, impulse = "gpr", 
            response = c("gpr", "epu", "sp500", "ip"), 
            n.ahead = 48, ci = 0.68, runs = 500)

# IRF PLOTS 

# There are a few ways to plot the IRFs in R
# Since all of the results are simply stored in, e.g. 'irf1data', one 
# could conceivably produce the figures using the package ggplot2.
# Here we stay with the base graphics package.


# Create a dataframe of the impulse repsonses and upper/lower confidence bands 
# Syntax: irf1$irf$FFR (stored results $ response (irf/upper/lower) $ impulse)
irf1data <- data.frame(irf1$irf$gpr, irf1$Lower$gpr, irf1$Upper$gpr)

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

pdf('figures/cholesky_gpr.pdf', width = 10, height = 7)
par(mfrow=c(2,2), mar=c(2,2,2,2))

# Create the plot using the appropriate impulse response
# Set the limits according to the upper and lower confidence bands
# NOTE: the limits are scaled by 1.1 to allow for more space on the plot 
plot(irf1data$gpr, type = "l", main = "GPR", 
     xlab = '', ylab = '',
     ylim = c(min(irf1data$gpr.1), max(irf1data$gpr.2))*1.1)
polygon(x = c(seq_along(irf1data$gpr), rev(seq_along(irf1data$gpr))), 
        y = c(irf1data$gpr.2, rev(irf1data$gpr.1)),
        col = 'lightgray',
        lty = 0)
# Horizonal line at origin
abline(h=0)


# Black line for the median impulse response in main sample
lines(irf1data$gpr, lty=1, col = "black", lwd = 2)



plot(irf1data$ip, type = "l", main = "Industrial production", 
     xlab = '', ylab = '',
     ylim = c(min(irf1data$ip.1), max(irf1data$ip.2))*1.1)
polygon(x = c(seq_along(irf1data$ip), rev(seq_along(irf1data$ip))), 
        y = c(irf1data$ip.2, rev(irf1data$ip.1)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(irf1data$ip, lty=1, col = "black", lwd = 2)


plot(irf1data$sp500, type = "l", main = "S&P 500", 
     xlab = '', ylab = '',
     ylim = c(min(irf1data$sp500.1), max(irf1data$sp500.2))*1.1)
polygon(x = c(seq_along(irf1data$sp500), rev(seq_along(irf1data$sp500))), 
        y = c(irf1data$sp500.2, rev(irf1data$sp500.1)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(irf1data$sp500, lty=1, col = "black", lwd = 2)


plot(irf1data$epu, type = "l", main = "EPU", 
     xlab = '', ylab = '',
     ylim = c(min(irf1data$epu.1), max(irf1data$epu.2))*1.1)
polygon(x = c(seq_along(irf1data$epu), rev(seq_along(irf1data$epu))), 
        y = c(irf1data$epu.2, rev(irf1data$epu.1)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(irf1data$epu, lty=1, col = "black", lwd = 2)

dev.off()

