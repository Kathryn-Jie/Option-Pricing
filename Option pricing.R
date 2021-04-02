# Put-call Parity

library(NMOF)

putCallParity(what, call, put, S, X, tau, r, q = 0, tauD = 0, D = 0)

# Example 1: Put-call Parity (continuous div)
# A non-dividend paying stock has a price of 40 today. A European call option allows buying the
# stock for 45 for the end of 9 months. The continuosly compounded risk-free rate is 5%. The
# premium of the call option is 2.84. Determine the premium of a European put option allowing
# selling the stock for 45 at the end of 9 months.

putCallParity("put", call=2.84, S=40, X=45, tau=9/12, r=0.05, q=0, tauD=0, D=0 )

# Example 2: Put-call Parity (continuous div)
# A dividend paying stock has a price of 40 today. A European put option allows selling the
# stock for 40 for the end of 3 months. The continuosly compounded risk-free rate is 4% and the countinous dividends proportional to its price at 2.086%. The premium of the call option is 3.91. Determine the premium of a European call option allowing buying the stock for 40 at the end of 3 months.

putCallParity("call", put=3.91, S=40, X=40, tau=3/12, r=0.04, q=0.02086, tauD=0, D=0)


# Cox, Ross, and Rubinstein Binomial Model

library(fOptions)

# CRRBinomialTreeOption(TypeFlag = c("ce", "pe", "ca", "pa"), S, X, Time, r, b, sigma, n, title = NULL, description = NULL)
# ce: European call  pe: European put  ca: American call  pa: American put

BinomialTreePlot(BinomialTreeValues, dx = -0.025, dy = 0.4, cex = 1, digits = 2, ...)

# American Call 

CRRBinomialTreeOption(TypeFlag = "ca", S = 150, X = 160, Time = 0.5, r = 0.06, b
                      = 0, sigma = 0.5, n = 2)

CRRTree = BinomialTreeOption(TypeFlag = "ca", S = 150, X = 160, Time = 0.5, r =
                               0.06, b = 0, sigma = 0.5, n = 2)
BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-10, 10),xlab = "n", ylab
                 = "Option Value")
title(main = "Option Tree")

# American Put

CRRBinomialTreeOption(TypeFlag = "pa", S=150, X=160, Time=0.5, r=0.06,b=0,sigma=0.5,n=5)
CRRTree = BinomialTreeOption(TypeFlag = "pa", S = 150, X = 160, Time = 0.5, r =
                               0.06, b = 0, sigma = 0.5, n = 5)
BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-10, 10),xlab = "n", ylab
                 = "Option Value")
title(main = "Option Tree")

# European Call

CRRBinomialTreeOption(TypeFlag = "ce", S = 150, X = 160, Time = 0.5, r = 0.06, b
                      = 0, sigma = 0.5, n = 2)

CRRTree = BinomialTreeOption(TypeFlag = "ce", S = 150, X = 160, Time = 0.5, r =
                               0.06, b = 0, sigma = 0.5, n = 2)
BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-10, 10),xlab = "n", ylab
                 = "Option Value")
title(main = "Option Tree")

# European Put

CRRBinomialTreeOption(TypeFlag = "pe", S = 150, X = 160, Time = 0.5, r = 0.06, b
                      = 0, sigma = 0.5, n = 5)

CRRTree = BinomialTreeOption(TypeFlag = "pe", S = 150, X = 160, Time = 0.5, r =
                               0.06, b = 0, sigma = 0.5, n = 5)
BinomialTreePlot(CRRTree, dy = 1, cex = 0.8, ylim = c(-10, 10),xlab = "n", ylab
                 = "Option Value")
title(main = "Option Tree")


# Black-Scholes Model

GBSOption(TypeFlag, S, X, Time, r, b, sigma, title = NULL, description = NULL)

# Black-Scholes Call

GBSOption(TypeFlag="c", S = 150, X = 160, Time = 0.5, r = 0.06, b = 0, sigma = 0.5)

# Black-Scholes Put

GBSOption(TypeFlag="p", S = 150, X = 160, Time = 0.5, r = 0.06, b = 0, sigma = 0.5)
