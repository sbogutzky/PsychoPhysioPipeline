GetSigString <- function(p) {
  sig.string <- ""
  if (p < .1)
    sig.string <- "."
  if (p < .05)
    sig.string <- "*"
  if (p < .01)
    sig.string <- "**"
  if (p < .001)
    sig.string <- "***"
  return(sig.string)
}

lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

PlotModels <- function(x, y, x.lim, sub.line, interval) {
  
  # Plot quadratic model
  x2  <- x^2
  quadratic.model <- lm(y ~ x + x2)
  print(summary(quadratic.model))
  
  x.interval  <- seq(x.lim[1], x.lim[2], by = interval)
  y.predicted <- predict(quadratic.model, list(x = x.interval, x2 = x.interval^2))
  lines(x.interval, y.predicted, lty = "dashed")
  
  quadratic.adj.r.squared <- round(summary(quadratic.model)$adj.r.squared, 3)
  quadratic.p   <- lmp(quadratic.model)
  quadratic.sig <- GetSigString(quadratic.p)
  
  # Plot linear model
  linear.model <- lm(y ~ x)
  abline(linear.model)
  print(summary(linear.model))
  
  linear.adj.r.squared <- round(summary(linear.model)$adj.r.squared, 3)
  linear.p   <- lmp(linear.model)
  linear.sig <- GetSigString(linear.p)
  
  title(sub = bquote({R[linear]}^2 ~ "=" ~ .(linear.adj.r.squared) ~ .(linear.sig) ~ "(solid)     " ~ {R[quadratic]}^2 ~ "=" ~ .(quadratic.adj.r.squared) ~ .(quadratic.sig) ~ "(dashed)"), line = sub.line, cex.sub = .8)
}

levene <- function(y,group) {
  group.means <- tapply(y,group,mean)
  ynew <- abs(y-group.means[group])
  summary(aov(ynew ~ group))
}
