SqExpKernel <- function(x1, x2, hyperParam){
  K <- matrix(nrow=length(x1), ncol=length(x2))
  for (i in 1:length(x2)){
    K[, i] <- hyperParam[1]^2 * exp(-0.5 *( (x1-x2[i])/hyperParam[2]) ^2)
  }
  return(K)
}
PosteriorGP <- function(x, y, xStar, hyperParam, sigmaNoise){
  # Calculates f star bar
  fStar <- SqExpKernel(xStar, x, hyperParam) %*% solve(SqExpKernel(x, x, hyperParam)+
                                        sigmaNoise^2*diag(length(x))) %*% y
  # Calculates cov f star
  cov_fStar <- SqExpKernel(xStar, xStar, hyperParam) - SqExpKernel(xStar, x, hyperParam)%*%
    solve(SqExpKernel(x, x, hyperParam)+sigmaNoise^2*diag(length(x))) %*%
    SqExpKernel(x, xStar, hyperParam)
  
  # Store all values in a list
  val_list <- list(fStar=fStar, cov_fStar=cov_fStar, xStar=xStar)
  
  return(val_list)
}
assign1B <- PosteriorGP(x=0.4, y=0.719, xStar=seq(-1,1, 0.01), hyperParam=c(1, 0.3),
            sigmaNoise=0.1)
Upper1B <- assign1B$fStar + 1.96 * sqrt(diag(assign1B$cov_fStar))
Lower1B <- assign1B$fStar - 1.96 * sqrt(diag(assign1B$cov_fStar))

plot(y=assign1B$fStar, assign1B$xStar, ylim=c(-2.5,2.5), type="l", lwd=3, col="darkorange")
points(x=0.4, y=0.719, pch=21, bg="black")
lines(y=Upper1B, assign1B$xStar, col="seagreen", lwd=3)
lines(y=Lower1B, assign1B$xStar, col="seagreen", lwd=3)
assign1C <- PosteriorGP(x=c(0.4, -0.6), y=c(0.719, -0.044), xStar=seq(-1,1, 0.01), hyperParam=c(1, 0.3),
                        sigmaNoise=0.1)
Upper1C <- assign1C$fStar + 1.96 * sqrt(diag(assign1C$cov_fStar))
Lower1C <- assign1C$fStar - 1.96 * sqrt(diag(assign1C$cov_fStar))

plot(y=assign1C$fStar, assign1C$xStar, ylim=c(-2.5,2.5), type="l", lwd=3, col="darkorange")
points(x=c(0.4, -0.6), y=c(0.719, -0.044), pch=21, bg="black")
lines(y=Upper1C, assign1C$xStar, col="seagreen", lwd=3)
lines(y=Lower1C, assign1C$xStar, col="seagreen", lwd=3)
assign1D <- PosteriorGP(x=c(0.8, 0.4, -0.2, -0.6, -1), y=c(-0.664, 0.719, -0.94, -0.044, 0.768), xStar=seq(-1,1, 0.01),
                        hyperParam=c(1, 0.3),sigmaNoise=0.1)
Upper1D <- assign1D$fStar + 1.96 * sqrt(diag(assign1D$cov_fStar))
Lower1D <- assign1D$fStar - 1.96 * sqrt(diag(assign1D$cov_fStar))

plot(y=assign1D$fStar, assign1D$xStar, ylim=c(-2.5,2.5),type="l", lwd=3, col="darkorange")
points(x=c(0.8, 0.4, -0.2, -0.6, -1), y=c(-0.664, 0.719, -0.94, -0.044, 0.768), pch=21, bg="black")
lines(y=Upper1D, assign1D$xStar, col="seagreen", lwd=3)
lines(y=Lower1D, assign1D$xStar, col="seagreen", lwd=3)
assign1E <- PosteriorGP(x=c(0.8, 0.4, -0.2, -0.6, -1), y=c(-0.664, 0.719, -0.94, -0.044, 0.768), xStar=seq(-1,1, 0.01),
                        hyperParam=c(1, 1),sigmaNoise=0.1)
Upper1E <- assign1E$fStar + 1.96 * sqrt(diag(assign1E$cov_fStar))
Lower1E <- assign1E$fStar - 1.96 * sqrt(diag(assign1E$cov_fStar))

plot(y=assign1E$fStar, assign1E$xStar, ylim=c(-2.5,2.5), type="l", lwd=3, col="darkorange")
points(x=c(0.8, 0.4, -0.2, -0.6, -1), y=c(-0.664, 0.719, -0.94, -0.044, 0.768), pch=21, bg="black")
lines(y=Upper1E, assign1E$xStar, col="seagreen", lwd=3)
lines(y=Lower1E, assign1E$xStar, col="seagreen", lwd=3)
JapanTemp <- read.delim("C:/Users/Gustav/Documents/Machine-Learning/Lab 6/JapanTemp.dat", sep="", header = TRUE)
par(mfrow=c(1,3))
SigmaVal <- c(2, 5, 10)
for(i in SigmaVal){
  Assign2 <- PosteriorGP(x=JapanTemp$time, y=JapanTemp$temp, xStar=seq(0,1, 0.01),
                       hyperParam=c(1.5, 0.3),sigmaNoise=i)
Upper2 <- Assign2$fStar + 1.96 * sqrt(diag(Assign2$cov_fStar))
Lower2 <- Assign2$fStar - 1.96 * sqrt(diag(Assign2$cov_fStar))

plot(JapanTemp, ylim=c(10,30), pch=21, bg="darkorange")
lines(y=Assign2$fStar, Assign2$xStar, type="l", lwd=3, col="darkorange")
lines(y=Upper2, Assign2$xStar, col="seagreen", lwd=3)
lines(y=Lower2, Assign2$xStar, col="seagreen", lwd=3)
}
par(mfrow=c(1,1))
par(mfrow=c(1,3))
Sigma_fVal <- c(0.25, 0.75, 1.5)
for(i in Sigma_fVal){
Assign2 <- PosteriorGP(x=JapanTemp$time, y=JapanTemp$temp, xStar=seq(0,1, 0.01),
                       hyperParam=c(i, 0.3),sigmaNoise=2)
Upper2 <- Assign2$fStar + 1.96 * sqrt(diag(Assign2$cov_fStar))
Lower2 <- Assign2$fStar - 1.96 * sqrt(diag(Assign2$cov_fStar))

plot(JapanTemp, ylim=c(10,30), pch=21, bg="darkorange")
lines(y=Assign2$fStar, Assign2$xStar, type="l", lwd=3, col="darkorange")
lines(y=Upper2, Assign2$xStar, col="seagreen", lwd=3)
lines(y=Lower2, Assign2$xStar, col="seagreen", lwd=3)
}
par(mfrow=c(1,1))
par(mfrow=c(1,3))
iota <- c(0.05, 0.3, 0.75)
for(i in iota){
Assign2 <- PosteriorGP(x=JapanTemp$time, y=JapanTemp$temp, xStar=seq(0,1, 0.01),
                       hyperParam=c(1.5, i),sigmaNoise=2)
Upper2 <- Assign2$fStar + 1.96 * sqrt(diag(Assign2$cov_fStar))
Lower2 <- Assign2$fStar - 1.96 * sqrt(diag(Assign2$cov_fStar))

plot(JapanTemp, ylim=c(10,30), pch=21, bg="darkorange")
lines(y=Assign2$fStar, Assign2$xStar, type="l", lwd=3, col="darkorange")
lines(y=Upper2, Assign2$xStar, col="seagreen", lwd=3)
lines(y=Lower2, Assign2$xStar, col="seagreen", lwd=3)
}
par(mfrow=c(1,1))
## 
