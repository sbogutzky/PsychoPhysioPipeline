fs                <- 256

# Set timestamp to zero
ecg.data.subset[,1] <- (ecg.data.subset[,1] - ecg.data.subset[1, 1]) / 1000

# Interpolate
t         <- seq(ecg.data.subset[1, 1], ecg.data.subset[nrow(ecg.data.subset), 1], by = 1/fs)
ecg.rall  <- interp1(ecg.data.subset[,1], ecg.data.subset[, 2], t, method = "spline")
ecg.lall  <- interp1(ecg.data.subset[,1], ecg.data.subset[, 3], t, method = "spline")

# Write csv file
output.file.path <- "ecg-data.csv"
write.csv(data.frame(t, ecg.rall, ecg.lall), output.file.path, row.names = FALSE)