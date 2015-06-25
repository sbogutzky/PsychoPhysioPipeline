t=seq(0,1000,1)
f1=100
f2=50
fs=8000
x=sin(2*pi*f2/fs*t) + sin(2*pi*f1/fs*t)
plot(t*1/fs*1000,x)

spectrum        <-TSA::periodogram(x, plot = T)
index           <- which.max(spectrum[[2]])
spectrum[[1]][index] * fs

stop.band.filter <- butter(3, c(1/(fs/2) * 49, 1/(fs/2) * 51), "stop")

y1 <- filtfilt(stop.band.filter, x)
plot(t*1/fs*1000,y1)

spectrum        <-TSA::periodogram(y1, plot = T)
index           <- which.max(spectrum[[2]])
spectrum[[1]][index] * fs
