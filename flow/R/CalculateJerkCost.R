#' Calculates the jerk cost of an acceleration in x-, y- and z-direction.
#'
#' \code{CalculateJerkCost} returns a vector with the jerk cost of the acceleration.
#'
#' @param t. Vectors with intervals (s)
#' @param x. The other vector with acceleration. t and x must have the same length, greater than one, with no missing values.
#' @param y. The other vector with acceleration. t and y must have the same length, greater than one, with no missing values.
#' @param z. The other vector with acceleration. t and z must have the same length, greater than one, with no missing values.
#' @param normalized. Boolean. Normalized by cycle length.
#' @param plot. Boolean. Plot cycle acceleration, cycle jerk and the time differential of jerk-cost
#' @return A vector with the jerk cost [m^2/s^6] of the acceleration or a vector with normalized jerk cost [m^2/s^5] of the acceleration.

CalculateJerkCost <- function(t, x, y, z, normalized = F, plot = F) {
  
  jerk.x <- CalculateJerk(t, x)
  jerk.y <- CalculateJerk(t, y)
  jerk.z <- CalculateJerk(t, z)
  
  cycle.interval <- t[length(t)] - t[1]
  tn             <- t - t[1]
  p              <- tn / cycle.interval
  
  if (plot) {
    par(mfrow = c(3,1), mgp = c(2, 1, 0)) 
    
    y.lim <- c(min(x, y, z), max(x, y, z))
    plot(p, x, type = "l", xlab = "Time [ % ]", ylab = expression("Acceleration [" ~ m/s^2 ~ "]"), xaxs = "i", ylim=y.lim)
    lines(p, y, lty = "dashed")
    lines(p, z, lty = "dotted")
    y.lim <- c(min(jerk.x, jerk.y, jerk.z), max(jerk.x, jerk.y, jerk.z))
    plot(p[-1], jerk.x, type = "l", xlab = "Time [ % ]", ylab = expression("Jerk [" ~ m/s^3 ~ "]"), xaxs = "i", ylim=y.lim)
    lines(p[-1], jerk.y, lty = "dashed")
    lines(p[-1], jerk.z, lty = "dotted")
    plot(p[-1], jerk.x^2 + jerk.y^2 + jerk.z^2, type = "l", xlab = "Time [ % ]", ylab = expression(Jerk[x]^2 ~ (t)+Jerk[y]^2 ~ (t)+Jerk[z]^2 ~ (t)), xaxs = "i")
    title(sub = "Time differential of jerk-cost")
  }
  
  # Intergal of the time differential
  jerk.cost <- mean(jerk.x^2 + jerk.y^2 + jerk.z^2)
  # jerk.cost <- trapz(p[-1],jerk.x^2 + jerk.y^2 + jerk.z^2)
  
  
  
  if (normalized) {
    jerk.cost <- jerk.cost * cycle.interval
    # jerk.cost <- trapz(t[-1], jerk.x^2 + jerk.y^2 + jerk.z^2)
  }
  
  #jerk.cost <- sqrt(jerk.cost)
  
  return(jerk.cost)
}