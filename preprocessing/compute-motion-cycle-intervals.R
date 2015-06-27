# Remove all variables
rm(list = ls(all = T)) 

# Load libraries
require(TSA)
require(signal)
require(flow)
require(RHRV)

# Set network directory
network.directory <- "//gangstore.ddns.net/flow/Documents/simon-bogutzky/data/"
if(file.exists("/Volumes/flow/Documents/simon-bogutzky/data"))
  network.directory <- "/Volumes/flow/Documents/simon-bogutzky/data/"

# Set preprocessed data directory
preprocessed.data.directory <- "./data/preprocessed-data/"

# Load fss features
fss.features        <- read.csv(paste(network.directory, "features/fss-features.csv", sep = ""), stringsAsFactors = F)

# Set body position
body.position       <- "leg"

for (i in 1:nrow(fss.features)) {
  
  properties      <- fss.features[i, c(6:12)]
  activity        <- properties[, 1]
  activity.start  <- properties[, 2]
  measurement     <- properties[, 5]
  last.name       <- properties[, 6]
  first.name      <- properties[, 7]
  if(measurement == 1) {
    date.directory  <- strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S")
  }
  
  # Read motion data
  motion.data.path <- paste(preprocessed.data.directory, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, "/", body.position, "-motion-data-", measurement,  ".csv", sep="")
  if(file.exists(motion.data.path)) {
  
    # Load motion data
    motion.data <- read.csv(motion.data.path)
  
    # Number of data row
    n <- nrow(motion.data)
  
    # Find cyclic movement   
    rotation.rate.x.spectrum <- TSA::periodogram(motion.data[, 5], plot = F)
    rotation.rate.y.spectrum <- TSA::periodogram(motion.data[, 6], plot = F)
    rotation.rate.z.spectrum <- TSA::periodogram(motion.data[, 7], plot = F)
  
    axis <- which.max(c(max(rotation.rate.x.spectrum[[2]]), max(rotation.rate.y.spectrum[[2]]), max(rotation.rate.z.spectrum[[2]])))
    rm(rotation.rate.x.spectrum, rotation.rate.y.spectrum, rotation.rate.z.spectrum)
  
    # Upsampling
    fs            <- 2000
    t             <- seq(motion.data[1, 1], motion.data[n, 1], by = 1000/fs)
    rotation.rate <- interp1(motion.data[, 1], -motion.data[, axis + 4], t, method = "spline")
  
    # Determine filter frequency
    periodogram   <- TSA::periodogram(rotation.rate, plot = F)
    freqs         <- periodogram[[1]] * fs
    specs         <- periodogram[[2]]
    index         <- which.max(specs)
    main.freq     <- freqs[index]
    filt.freq     <- ceiling(main.freq)
    rm(periodogram, freqs, specs, index)
  
    # Low pass signal
    low.pass.filter         <- butter(3, 1/(fs/2) * filt.freq, "low")
    filtered.rotation.rate  <- filtfilt(low.pass.filter, rotation.rate)
  
    # Search for minima
    minima <- SearchExtrema(filtered.rotation.rate, which = "minima")
  
    # Remove some minima
    m <- mean(filtered.rotation.rate[minima])
    s <- sd(filtered.rotation.rate[minima])
    minima <- minima[m + s * 4 > filtered.rotation.rate[minima]]
    rm(m, s)
    

    # Compute time and intervals
    t.s               <- t[minima] / 1000
    #cycle.interval.s  <- c(NA, diff(t.s))
    
    cycle.data          <- CreateHRVData()
    cycle.data          <- SetVerbose(cycle.data, T)
    cycle.data$Beat     <- data.frame("Time" = t.s)
    
    # Build not interpolated data
    cycle.data          <- BuildNIHR(cycle.data)
    
    # Filter SPM automatically
    s                 <- sd(cycle.data$Beat$niHR)
    m                 <- mean(cycle.data$Beat$niHR)
    min.spm           <- m - 3 * s
    max.spm           <- m + 3 * s
    cycle.data        <- FilterNIHR(cycle.data, long=50, last=13, minbpm=min.spm, maxbpm=max.spm)
    
    # Remove artefact manually
    cycle.data          <- EditNIHR(cycle.data)
    
    t.s               <- cycle.data$Beat$Time
    cycle.interval.s  <- cycle.data$Beat$RR / 1000

    # Plot barplot and tachogramm
    par(mfcol=c(3, 1))
    plot(t / 1000, filtered.rotation.rate, type="l", xlab="t [s]", ylab=paste("Filtered Rotation Rate [deg/s]", c("X", "Y", "Z")[axis]), xlim = c(300,315))
    abline(v = t[minima] / 1000)
    title(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y/%m/%d %H:%M"))
    hist(cycle.interval.s, xlab="Motion Cycle Interval [s]", main="", breaks = 100)
    plot(t.s, cycle.interval.s, type="l", xlab="t [s]", ylab="Motion Cycle Interval [s]")

    # Create directory, if needed
    output.directory <- paste(network.directory, "processed-data/", tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, sep="")
    if(!file.exists(output.directory)) {
      dir.create(paste(output.directory, "/", sep = ""), recursive = TRUE)
    }

    # Write csv file
    output.file.path <- paste(output.directory, "/", body.position, "-motion-time-data-", measurement, ".csv", sep = "")
    op <- options(digits.secs=3)
    con <- file(output.file.path, 'w') 
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d"), con = con)
    writeLines(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%H:%M:%OS"), con = con)
    write.csv(data.frame(t.s, cycle.interval.s), file = con, row.names = FALSE)
    close(con)
    options(op) #reset options
    print(paste("Wrote:", output.file.path))
    
  } else {
    print("No Motion data")
  }
}
