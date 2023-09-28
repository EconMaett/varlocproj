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

library(readxl)
library(dynlm) # helps with time series regressions
library(vars)
library(sandwich) # needed for Newey-West s.e.



# Write your directory path here:

dir <- "U:/tf/Caldara/darioprojects/Courses/Georgetown2018/Codes/VAR_STATA_R"
setwd(dir)

# Load the data from excel
vardata <- read_excel('data/Monetarydat.xlsx', sheet = 'Monthly')
colnames(vardata) <- tolower(colnames(vardata)) # makes variable name lower case

# Create a monthly date column
vardata$dates <- seq(from=as.Date("1959-01-01"), to=as.Date("2015-12-01"), by="month")

# Transform log variables for easier interpretation
vardata[c("lip", "lcpi", "lpcom", "lnbr", "ltr", "lm1")] <- vardata[c("lip", "lcpi", "lpcom", "lnbr", "ltr", "lm1")]*100



# Filter the data by sample date and save the relevant varibles to a new data frame
# For variable definitions, see the 'Readme' section of Monetarydat.xlsx
# NOTE: dates are not needed for the VAR function

cee6595 <- subset(vardata,
                  select = c("ffr", "lip", "unemp", "lcpi", "lpcom", "lnbr", "ltr", "lm1"),
                  dates >= as.Date('1965-01-01') & dates <= as.Date('1995-05-01'))

cee8307 <- subset(vardata,
                  select = c("ffr", "lip", "unemp", "lcpi", "lpcom", "lnbr", "ltr", "lm1"),
                  dates >= as.Date('1983-01-01') & dates <= as.Date('2007-12-01'))

# No monetary variables
cee8307nm <- subset(vardata,
                  select = c("ffr", "lip", "unemp", "lcpi", "lpcom"),
                  dates >= as.Date('1983-01-01') & dates <= as.Date('2007-12-01'))

# Estimate the VARs
var1 <- VAR(cee6595, p =12) # Set the number of lags using 'p'
var2 <- VAR(cee8307, p =12)
var3 <- VAR(cee8307nm, p =12)


# View summary statistics for the estimated equations, as well as
# the covariance and the correlation matrices of residuals 
summary(var1)

# Use the irf() function to produce impulse responses for the estimaed VAR
# 'n.ahead' sets the length of the IRF in periods (default is 10)
# 'ci' sets the confidence bands; we use 90% here (default is 95%)
# 'runs' sets the number of runs for bootstrapping (default is 100)
irf1 <- irf(var1, impulse = "ffr", 
            response = c("ffr", "lip", "unemp", "lcpi", "lpcom", "lnbr", "ltr", "lm1"), 
            n.ahead = 48, ci = 0.9, runs = 500)

irf2 <- irf(var2, impulse = "ffr", 
            response = c("ffr", "lip", "unemp", "lcpi", "lpcom", "lnbr", "ltr", "lm1"), 
            n.ahead = 48, ci = 0.9, runs = 500)

irf3 <- irf(var3, impulse = "ffr", 
            response = c("ffr", "lip", "unemp", "lcpi", "lpcom"), 
            n.ahead = 48, ci = 0.9, runs = 500)


# IRF PLOTS 

# There are a few ways to plot the IRFs in R
# Since all of the results are simply stored in, e.g. 'irf1data', one 
# could conceivably produce the figures using the package ggplot2.
# Here we stay with the base graphics package.


# Create a dataframe of the impulse repsonses and upper/lower confidence bands 
# Syntax: irf1$irf$FFR (stored results $ response (irf/upper/lower) $ impulse)
irf1data <- data.frame(irf1$irf$ffr, irf1$Lower$ffr, irf1$Upper$ffr)
irf2data <- data.frame(irf2$irf$ffr, irf2$Lower$ffr, irf2$Upper$ffr)
irf3data <- data.frame(irf3$irf$ffr, irf3$Lower$ffr, irf3$Upper$ffr)

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

pdf('figures/ramey_figure_1_R.pdf', width = 10, height = 7)
par(mfrow=c(4,2), mar=c(2,2,2,2))

# Create the plot using the appropriate impulse response
# Set the limits according to the upper and lower confidence bands
# NOTE: the limits are scaled by 1.1 to allow for more space on the plot 
plot(irf1data$ffr, type = "l", main = "Federal funds rate", 
     xlab = '', ylab = '',
     ylim = c(min(irf1data$ffr.1), max(irf1data$ffr.2))*1.1)
polygon(x = c(seq_along(irf1data$ffr), rev(seq_along(irf1data$ffr))), 
        y = c(irf1data$ffr.2, rev(irf1data$ffr.1)),
        col = 'lightgray',
        lty = 0)

# Horizonal line at origin
abline(h=0)

# Black line for the median impulse response in main sample
lines(irf1data$ffr, lty=1, col = "black", lwd = 2)
lines(irf2data$ffr, lty=2, col = "blue", lwd = 2)
lines(irf3data$ffr, lty=5, col = "red", lwd = 2)
text(15.5, 0.5, "1965-95")
#arrows(10, 0.5, 5, 0.4)

plot(irf1data$lip, type = "l", main = "Industrial production", 
     xlab = '', ylab = '',
     ylim = c(min(irf1data$lip.1), max(irf1data$lip.2))*1.1)
polygon(x = c(seq_along(irf1data$lip), rev(seq_along(irf1data$lip))), 
        y = c(irf1data$lip.2, rev(irf1data$lip.1)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(irf1data$lip, lty=1, col = "black", lwd = 2)
lines(irf2data$lip, lty=2, col = "blue", lwd = 2)
lines(irf3data$lip, lty=5, col = "red", lwd = 2)
text(22, 0.17, "1983-2007", col = "blue")


plot(irf1data$unemp, type = "l", main = "Unemployment", 
     xlab = '', ylab = '',
     ylim = c(min(irf1data$unemp.1), max(irf1data$unemp.2))*1.1)
polygon(x = c(seq_along(irf1data$unemp), rev(seq_along(irf1data$unemp))), 
        y = c(irf1data$unemp.2, rev(irf1data$unemp.1)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(irf1data$unemp, lty=1, col = "black", lwd = 2)
lines(irf2data$unemp, lty=2, col = "blue", lwd = 2)
lines(irf3data$unemp, lty=5, col = "red", lwd = 2)

plot(irf1data$lcpi, type = "l", main = "CPI", 
     xlab = '', ylab = '',
     ylim = c(min(irf1data$lcpi.1), max(irf3data$ffr))*1.1)
polygon(x = c(seq_along(irf1data$lcpi), rev(seq_along(irf1data$lcpi))), 
        y = c(irf1data$lcpi.2, rev(irf1data$lcpi.1)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(irf1data$lcpi, lty=1, col = "black", lwd = 2)
lines(irf2data$lcpi, lty=2, col = "blue", lwd = 2)
lines(irf3data$lcpi, lty=5, col = "red", lwd = 2)
text(22, 0.22, "1983-2007, omits money & reserves", col = "red")

plot(irf1data$lip, type = "l", main = "Commodity prices", 
     xlab = '', ylab = '',
     ylim = c(min(irf1data$lpcom.1), max(irf3data$lpcom))*1.1)
polygon(x = c(seq_along(irf1data$lpcom), rev(seq_along(irf1data$lpcom))), 
        y = c(irf1data$lpcom.2, rev(irf1data$lpcom.1)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(irf1data$lpcom, lty=1, col = "black", lwd = 2)
lines(irf2data$lpcom, lty=2, col = "blue", lwd = 2)
lines(irf3data$lpcom, lty=5, col = "red", lwd = 2)

plot(irf1data$lnbr, type = "l", main = "Nonborrowed reserves", 
     xlab = '', ylab = '',
     ylim = c(min(irf1data$lnbr.1), max(irf1data$lnbr.2))*1.1)
polygon(x = c(seq_along(irf1data$lnbr), rev(seq_along(irf1data$lnbr))), 
        y = c(irf1data$lnbr.2, rev(irf1data$lnbr.1)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(irf1data$lnbr, lty=1, col = "black", lwd = 2)
lines(irf2data$lnbr, lty=2, col = "blue", lwd = 2)

plot(irf1data$ltr, type = "l", main = "Total reserves", 
     xlab = '', ylab = '',
     ylim = c(min(irf2data$ltr), max(irf2data$ltr))*1.1)
polygon(x = c(seq_along(irf1data$ltr), rev(seq_along(irf1data$ltr))), 
        y = c(irf1data$ltr.2, rev(irf1data$ltr.1)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(irf1data$ltr, lty=1, col = "black", lwd = 2)
lines(irf2data$ltr, lty=2, col = "blue", lwd = 2)


plot(irf1data$lm1, type = "l", main = "M1", 
     xlab = '', ylab = '',
     ylim = c(min(irf2data$lm1.1), max(irf1data$lm1.2))*1.1)
polygon(x = c(seq_along(irf1data$lm1), rev(seq_along(irf1data$lm1))), 
        y = c(irf1data$lm1.2, rev(irf1data$lm1.1)),
        col = 'lightgray',
        lty = 0)
abline(h=0)
lines(irf1data$lm1, lty=1, col = "black", lwd = 2)
lines(irf2data$lm1, lty=2, col = "blue", lwd = 2)

dev.off()

