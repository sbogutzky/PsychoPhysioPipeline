t = c(0, .05, .2, .3, .4, .6, .8, .85, 1)
f = c(-150, -190,  190, -30, -200, -380, 450, 400, -150) * 5
n = 100;
T = (0:n)/n;
F = interp1(t, f, T, method = "spline")
plot(T, F)
plot(T, F^2)
trapz(T, F^2)
sqrt(mean(F^2))