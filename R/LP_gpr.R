# *************************************************************************
# JORDA - LOCAL PROJECTIONS

# This code replicates the IRFs in Figure 2(B) from
# "Macroeconomic Shocks and Their Propagation" by Valerie Ramey
# in the 2016 Handbook of Macroeconomics.
#
# Required data: dataGPR.txt
#
#   Source: http://econweb.ucsd.edu/~vramey/research.html#data
#   (click on 'Monetary Shocks' to download the zip file)
#
# Make sure to install all packages before running the code
# and to set the working directory
options(repos = structure(c(CRAN = "http://cran.frb.gov/")))

rm(list = ls())

library(dynlm) # helps with time series regressions
library(vars)
library(sandwich) # needed for Newey-West s.e.



dir <- "U:/tf/Caldara/darioprojects/Courses/Georgetown2018/Codes/localprojections"
setwd(dir)

vardata <- read.delim("data/dataGPR.txt", header = FALSE)
colnames(vardata) <- c("gpr", "epu", "sp500", "ip")


# Define the data as a time series to use lags and leads in the regression
gpr <- ts(vardata, start = c(1985, 1), end = c(2015, 12), frequency = 12)

# *************************************************************************
# Here we create matrices of NAs to be filled-in with regression
# coefficients and standard errors. This will make it easier to
# produce figures later on
periods <- 12
varnames <- colnames(gpr) # create vector of variable names
cnames <- character(3 * length(varnames))

responses <- data.frame(matrix(NA, nrow = periods, ncol = 3 * length(varnames)))

for (i in 1:length(varnames)) {
  b <- paste0("b", varnames[i])
  cnames[i] <- b
}
for (i in 1:length(varnames)) {
  up <- paste0("up90b", varnames[i])
  cnames[i + length(varnames)] <- up
}
for (i in 1:length(varnames)) {
  low <- paste0("low90b", varnames[i])
  cnames[i + 2 * length(varnames)] <- low
}

colnames(responses) <- cnames

# *************************************************************************

# The 'dynlm' function allows for a simpler implementation of time series analysis,
# especially via the L() operator for lagged variables. E.g. L(ffr, 1:2) includes both
# 1 and 2 period lags for the fed funds rate in the model

# We need to perform the regression using several different independent variables.
# To do this efficiently, we use the lapply() function, as we cannot simply loop over
# the variable names: R will try to regress a string on the other variables.
#
# If you are unfamiliar with the 'apply' family of functions in R it is worth reading up on.
# https://www.datacamp.com/community/tutorials/r-tutorial-apply-family
#
# If the function looks confusing, just know that we are running this code for each
# variable (in the first model):
#
# -i indicates a 'negative' lag, in other words a lead t+1
#
# dynlm(L(lip, -i) ~  L(ffr, 1:2) + L(lip, 0:2) + L(unemp, 0:2) +
#                L(lcpi, 0:2) + L(lpcom, 0:2) + L(rrshockorig, 0:2),
#              data = jordats)


gpr_res <- (ar(gpr[, 1], aic = FALSE, order.max = 1, method = "ols")$resid) / 160


for (i in 0:periods) {
  mods_responses <- lapply(varnames, function(x) {
    form <- as.formula(paste0("L(", x, ",", -i, ") ~  L(gpr_res, 0:3) + L(epu, 1:3) +
                              L(sp500, 1:3) + L(ip, 1:3) + L(gpr, 1:3)"))
    dynlm(form, data = gpr)
  })

  #
  # To get Newey-West s.e. simply use the coeftest() function, specifiying NeweyWest for vcov.

  newey1 <- lapply(mods_responses, function(x) {
    coeftest(x, vcov. = NeweyWest(x, lag = (i + 1), prewhite = FALSE))
  })


  # Extract the coefficients and s.e. of the  for each variable from the list
  for (j in 1:length(varnames)) {
    beta <- newey1[[j]][2, 1] # coefficient
    se <- newey1[[j]][2, 2] # S.E.
    up90 <- beta + 1.68 * se
    low90 <- beta - 1.68 * se
    responses[i + 1, paste0("b", varnames[j])] <- beta
    responses[i + 1, paste0("up90b", varnames[j])] <- up90
    responses[i + 1, paste0("low90b", varnames[j])] <- low90
  }
}
# *************************************************************************

# This code produces shaded areas representing the 90% CIs.
# See the commented code below to use dashed lines for the
# confidence bands instead.

# NOTE: Using the polygon() function for shading between
# curves isn't necessarily intuitive. Two sets of x coordinates
# are needed to draw the polygon.
# For a good explaination of how this function works, see this post:
# http://www.alisonsinclair.ca/2011/03/shading-between-curves-in-r/

# *************************************************************************

pdf("figures/gpr_lp_R.pdf", width = 10, height = 5)
par(mfrow = c(2, 2), mar = c(2, 2, 2, 2))

plot(responses$bip,
  type = "l", main = "IP",
  xlab = "", ylab = "",
  ylim = c(min(responses$low90bip), max(responses$up90bip)) * 1.1
)
polygon(
  x = c(seq_along(responses$bip), rev(seq_along(responses$bip))),
  y = c(responses$up90bip, rev(responses$low90bip)),
  col = "lightgray",
  lty = 0
)
abline(h = 0)
lines(responses$bip, lty = 1, col = "black", lwd = 2)

plot(responses$bepu,
  type = "l", main = "EPU",
  xlab = "", ylab = "",
  ylim = c(min(responses$low90bepu), max(responses$up90bepu)) * 1.1
)
polygon(
  x = c(seq_along(responses$bepu), rev(seq_along(responses$bepu))),
  y = c(responses$up90bepu, rev(responses$low90bepu)),
  col = "lightgray",
  lty = 0
)
abline(h = 0)
lines(responses$bepu, lty = 1, col = "black", lwd = 2)


plot(responses$bsp500,
  type = "l", main = "S&P 500",
  xlab = "", ylab = "",
  ylim = c(min(responses$low90bsp500), max(responses$up90bsp500)) * 1.1
)
polygon(
  x = c(seq_along(responses$bsp500), rev(seq_along(responses$bsp500))),
  y = c(responses$up90bsp500, rev(responses$low90bsp500)),
  col = "lightgray",
  lty = 0
)
abline(h = 0)
lines(responses$bsp500, lty = 1, col = "black", lwd = 2)



plot(responses$bgpr,
  type = "l", main = "GPR",
  xlab = "", ylab = "",
  ylim = c(min(responses$low90bgpr), max(responses$bgpr)) * 1.1
)
polygon(
  x = c(seq_along(responses$bgpr), rev(seq_along(responses$bgpr))),
  y = c(responses$up90bgpr, rev(responses$low90bgpr)),
  col = "lightgray",
  lty = 0
)
abline(h = 0)
lines(responses$bgpr, lty = 1, col = "black", lwd = 2)



dev.off()
# END