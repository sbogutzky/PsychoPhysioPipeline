par(mfcol = c(1, 2))

t = c(0, .17, .38, .48, .5, .52, .68, .85, .95, 1)
a = c(20, 7,  15, 1, -3, -7.5, -30, 0, 23, 20)
n = 60;
t1 = (0:n)/n;
a1 = interp1(t, a, t1, method = "spline")

#t1 = seq(0, 1, length.out = length(mean.a.x))
#a1 = mean.a.x

plot(t1, a1, type = "l", ylim = c(-40, 30))

j <- CalculateJerk(t1, a1)

plot(t1[-1], j, type = "l", ylim = c(-400, 600))


trapz(t1[-1] * 60, j^2) / 60 / 10^5

mean(j^2) / 10^5