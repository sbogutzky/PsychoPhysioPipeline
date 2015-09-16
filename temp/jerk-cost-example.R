t = c(0, .05, .2, .3, .4, .6, .8, .85, 1)
f = c(-150, -190,  190, -30, -200, -380, 450, 400, -150) * 5
n = 100;
t1 = (0:n)/n;
f1 = interp1(t, f, t1, method = "spline")
plot(t1, f1)
plot(t1, f1^2)
trapz(t1, f1^2)
mean(f1^2)