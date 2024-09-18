#####################################################################
#####################################################################

set.seed(101)
acq.freq <- 200
time     <- 1
w        <- 2*pi/time
ts       <- seq(0,time,1/acq.freq)
trajectory <- 3*rnorm(101) + 3*sin(3*w*ts)
plot(trajectory, type="l")


X.k <- fft(trajectory)
plot.frequency.spectrum(X.k,xlimits=c(0,acq.freq/2))






####################################################################
####################################################################

library(GeneCycle)
trajectory1 <- trajectory + 25*ts # let's create a linear trend 
plot(trajectory1, type="l")


f.data <- GeneCycle::periodogram(trajectory1)
harmonics <- 1:20
plot(f.data$freq[harmonics]*length(trajectory1), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")




####################################################################
####################################################################

trend <- lm(trajectory1 ~ts)
detrended.trajectory <- trend$residuals
plot(detrended.trajectory, type="l")

f.data <- GeneCycle::periodogram(detrended.trajectory)
harmonics <- 1:20
plot(f.data$freq[harmonics]*length(detrended.trajectory), 
     f.data$spec[harmonics]/sum(f.data$spec), 
     xlab="Harmonics (Hz)", ylab="Amplitute Density", type="h")

