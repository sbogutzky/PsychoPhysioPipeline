#   mid.swing.indexes.path <- paste(processed.data.directory.path, activity.directory, user.directory, date.directory, "mid-swing-indexes-", measurement,  ".csv", sep="")


#     # Check for mid swing indexes
#     mid.swing.indexes <- c()
#     if(file.exists(mid.swing.indexes.path)) {
#       mid.swing.indexes <- read.csv(mid.swing.indexes.path, skip = 2)[, 1]
#     }

#     # Smooth acceleration data
#     A <- M[, 1:4]
#     fn <- fs/2
#     n <- 2
#     
#     mean.A.vertical <- ComputeCycleMeanMatrix(A, 3, mid.swing.indexes)
#     par(mfcol = c(2, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
#     x.p <- 1:length(mean.A.vertical[, 1]) / length(mean.A.vertical[, 1]) * 100
#     plot(x.p,  mean.A.vertical[, 1], type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Acceleration vertical ("~m/s^2~")"))
#     for (j in 2:length(mean.A.vertical[1, ])) {
#       lines(x.p, mean.A.vertical[,j], type = "l")
#     }
#     mean.a.vertical <- rowMeans(mean.A.vertical)
#     plot(x.p, mean.a.vertical, type = "l",xlab = expression("Time ( % Stride )"), ylab = expression("Acceleration vertical ("~m/s^2~")"))
# #     mean.a.vertical.shifted <- c(mean.a.vertical[(length(mean.A.vertical[, 1])/2 + 1):length(mean.A.vertical[, 1])], mean.a.vertical[1:(length(mean.A.vertical[, 1])/2)])
# #     plot(x.p, mean.a.vertical.shifted, xlab = expression("Time ( % Stride )"), ylab = expression("Acceleration vertical ("~m/s^2~")"))
#     
#     mean.A.horizontal <- ComputeCycleMeanMatrix(A, 4, mid.swing.indexes)
#     par(mfcol = c(2, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
#     x.p <- 1:length(mean.A.horizontal[, 1]) / length(mean.A.horizontal[, 1]) * 100
#     plot(x.p,  mean.A.horizontal[, 1], type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Acceleration horizontal ("~m/s^2~")"))
#     for (j in 2:length(mean.A.horizontal[1, ])) {
#       lines(x.p, mean.A.horizontal[,j], type = "l")
#     }
#     mean.a.horizontal <- rowMeans(mean.A.horizontal)
#     plot(x.p, mean.a.horizontal, type = "l",xlab = expression("Time ( % Stride )"), ylab = expression("Acceleration horizontal ("~m/s^2~")"))
# #     mean.a.horizontal.shifted <- c(mean.a.horizontal[(length(mean.A.horizontal[, 1])/2 + 1):length(mean.A.horizontal[, 1])], mean.a.horizontal[1:(length(mean.A.horizontal[, 1])/2)])
# #     plot(x.p, mean.a.horizontal.shifted, xlab = expression("Time ( % Stride )"), ylab = expression("Acceleration horizontal ("~m/s^2~")"))
#     
#     # A[, 2] <- filtfilt(lowpass.filter, A[, 2])
#     
#     par(mfcol = c(2, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
#     fc <- 6.5
#     W <- fc/fn
#     lp.vertical <- butter(n, W)
#     lp.vertical.freg <- freqz(lp.vertical, Fs = fs)
#     plot(lp.vertical.freg$f, abs(lp.vertical.freg$h), type = "l", xlim = c(0, 30), xlab = "Frequency ( Hz )", ylab = "Magnitude Response")
#     A[, 3] <- filtfilt(lp.vertical, A[, 3])
#     
#     fc <- 7.5
#     W <- fc/fn
#     lp.horizontal <- butter(n, W)
#     lp.horizontal.freg <- freqz(lp.horizontal, Fs = fs)
#     plot(lp.horizontal.freg$f, abs(lp.horizontal.freg$h), type = "l", xlim = c(0, 30), xlab = "Frequency ( Hz )", ylab = "Magnitude Response")
#     A[, 4] <- filtfilt(lp.horizontal, A[, 4])
#     
#     mean.A.vertical <- ComputeCycleMeanMatrix(A, 3, mid.swing.indexes)
#     par(mfcol = c(3, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
#     x.p <- 1:length(mean.A.vertical[, 1]) / length(mean.A.vertical[, 1]) * 100
#     plot(x.p,  mean.A.vertical[, 1], type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Acceleration vertical ("~m/s^2~")"))
#     for (j in 2:length(mean.A.vertical[1, ])) {
#       lines(x.p, mean.A.vertical[,j], type = "l")
#     }
#     mean.a.vertical <- rowMeans(mean.A.vertical)
#     plot(x.p, mean.a.vertical, type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Acceleration vertical ("~m/s^2~")"))
# #     mean.a.vertical.shifted <- c(mean.a.vertical[(length(mean.A.vertical[, 1])/2 + 1):length(mean.A.vertical[, 1])], mean.a.vertical[1:(length(mean.A.vertical[, 1])/2)])
# #     plot(x.p, mean.a.vertical.shifted, xlab = expression("Time ( % Stride )"), ylab = expression("Acceleration vertical ("~m/s^2~")"))
#     mean.j.vertical <- c(NA, CalculateJerk(x.p, mean.a.vertical))
#     plot(x.p, mean.j.vertical, type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Jerk vertical ("~m/s^3~")"))
#     
#     mean.A.horizontal <- ComputeCycleMeanMatrix(A, 4, mid.swing.indexes)
#     par(mfcol = c(3, 1), mar = c(3.5, 4, 3.5, 4) + 0.1, mgp = c(2.5, 1, 0))
#     x.p <- 1:length(mean.A.horizontal[, 1]) / length(mean.A.horizontal[, 1]) * 100
#     plot(x.p,  mean.A.horizontal[, 1], type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Acceleration horizontal ("~m/s^2~")"))
#     for (j in 2:length(mean.A.horizontal[1, ])) {
#       lines(x.p, mean.A.horizontal[,j], type = "l")
#     }
#     mean.a.horizontal <- rowMeans(mean.A.horizontal)
#     plot(x.p, mean.a.horizontal, type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Acceleration horizontal ("~m/s^2~")"))
# #     mean.a.horizontal.shifted <- c(mean.a.horizontal[(length(mean.A.horizontal[, 1])/2 + 1):length(mean.A.horizontal[, 1])], mean.a.horizontal[1:(length(mean.A.horizontal[, 1])/2)])
# #     plot(x.p, mean.a.horizontal.shifted, xlab = expression("Time ( % Stride )"), ylab = expression("Acceleration horizontal ("~m/s^2~")"))
#     mean.j.horizontal <- c(NA, CalculateJerk(x.p, mean.a.horizontal))
#     plot(x.p, mean.j.horizontal, type = "l", xlab = expression("Time ( % Stride )"), ylab = expression("Jerk horizontal ("~m/s^3~")"))
#     
#     # Compute output data
#     cycle.jerk.costs <- ComputeCycleJerkCosts(A, mid.swing.indexes)
#     cycle.intervals <- diff(M[, 1][mid.swing.indexes] / 1000)
#     output.data <- data.frame(t.s = M[, 1][mid.swing.indexes[2:length(mid.swing.indexes)]] / 1000, cycle.interval.s = cycle.intervals, jerk.cost.m2s5 = cycle.jerk.costs)
#     output.data <- output.data[output.data[, 2] < 1.5, ]
#     
#     # Compute mean to compare
#     jerk.cost.by.all.accelerations <- CalculateJerkCost(A[, 1] / 1000, A[, 3:4], normalized = T) 
#     jerk.cost.by.cycle.mean <- mean(output.data[, 3], na.rm = T)
#     print(paste("Jerk Cost by Cycle:            ", jerk.cost.by.cycle.mean / 10^4))
#     print(paste("Jerk Cost by all Accelerations:", jerk.cost.by.all.accelerations / 10^4))
#     
#     # Save data
#     saveData(output.data, paste(body.position, "-jerk-cost-data-", measurement, ".csv", sep = ""), preprocessed.data.directory.path, activity.directory, user.directory, date.directory, activity.start)
#     
#     readline("Press return to continue > ")
#     
#   } else {
#     print(paste("File not exits:", motion.data.path))
#   }
# } 
