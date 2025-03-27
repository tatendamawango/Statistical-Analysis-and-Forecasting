library(fpp)


#
# EXAMPLE. Dynamic regression
#

length(insurance[,2])
advLag0 <- insurance[,2]
advLag1 <- c(NA, insurance[1:39, 2])
advLag2 <- c(NA, NA, insurance[1:38, 2])
advLag3 <- c(NA, NA, NA, insurance[1:37, 2])

cbind(advLag0, advLag1, advLag2, advLag3)

fitlagged <- tslm(insurance[,1]~advLag0 + advLag1)
summary(fitlagged)

fitlagged <- tslm(insurance[,1]~advLag0 + advLag1 + advLag2)
summary(fitlagged)

fitlagged <- tslm(insurance[,1]~advLag0 + advLag1 + advLag2 + advLag3)
summary(fitlagged)
