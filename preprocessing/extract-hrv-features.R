# Remove all variables
rm(list = ls(all = T))  

# Set working directory
setwd("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/preprocessing")

# Load libraries
library(RHRV)

# Set paths
feature.path            <- "../data/features/"
preprocessed.data.path  <- "../data/preprocessed-data/"

# How many minutes before the activity end the measurement started?
measurement.started.before  <- 5

# How many minutes before the activity end the measurement ended?
measurement.ended.before  <- 0

# Load fss features
fss.features        <- read.csv("../data/features/fss-features.csv", stringsAsFactors = F)

# Create feature data frame as result
hrv.features  <- data.frame()

for (i in 1:nrow(fss.features)) {
  properties      <- fss.features[i, c(6:13)]
  activity        <- properties[, 1]
  activity.start  <- properties[, 2]
  activity.end    <- properties[, 3]
  measurement     <- properties[, 5]
  last.name       <- properties[, 6]
  first.name      <- properties[, 7]
  date.of.birth   <- properties[, 8]
  
  if(measurement == 1) {
    current.data.path   <- paste(preprocessed.data.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", strftime(activity.start, format = "%Y-%m-%d--%H-%M-%S"), "/",  sep = "")
  }
  current.file.name   <- paste("ecg-data-", measurement, "_hrv.txt", sep = "")
  current.file.path   <- paste(current.data.path, current.file.name, sep = "")
  
  if(file.exists(current.file.path)) {
    
    # Load beat times from KubiosHRV txt file
    beat.times <- read.csv(current.file.path, header = F, na.strings = "", fill = T, skip = 117, stringsAsFactors = F, col.names = c("", "Time", "RR.interval", "FFT.Frequency", "FFT.PSD", "AR.Frequency", "AR.PSD", "VLF.comp.", "LF.comp.", "HF.comp.", ""))[,2]
    beat.times <- beat.times[complete.cases(beat.times)]
    
    # Create hrv data
    hrv.data          <- CreateHRVData()
    hrv.data          <- SetVerbose(hrv.data, F)
    hrv.data$Beat     <- data.frame("Time" = beat.times)
    
    # Build not interpolated heart rates
    hrv.data          <- BuildNIHR(hrv.data)
    #PlotNIHR(hrv.data)
    
    # Remove artefact manually
    #hrv.data          <- EditNIHR(hrv.data)
    #PlotNIHR(hrv.data)
    
    # Filter niHR automatically
    s                 <- sd(hrv.data$Beat$niHR)
    m                 <- mean(hrv.data$Beat$niHR)
    minbpm            <- m - 3 * s
    maxbpm            <- m + 3 * s
    hrv.data          <-  FilterNIHR(hrv.data, long=50, last=13, minbpm=minbpm, maxbpm=maxbpm)
    #PlotNIHR(hrv.data)
    
    # Linear Interpolate the data by 4 Hz (default)
    hrv.data          <- InterpolateNIHR(hrv.data)
    #PlotHR(hrv.data)
    
    # Create time analysis
    #hrv.data          <- CreateTimeAnalysis(hrv.data, size=60)
    
    # Plot spectogram with Short-time Fourier transform 30 seconds window with 1 second shift for LF
    #PlotSpectrogram(hrv.data, size=30, shift=1, freqRange=c(0.04, 0.15))
    
    # Plot spectogram with Short-time Fourier transform 30 seconds window with 1 second shift for HF-VHF
    #PlotSpectrogram(hrv.data, size=10, shift=1, freqRange=c(0.15, 1))
    
    # Create Frequency analysis (CWT) with least asymmetric Daubechies of width 8 for ULF, VLF, LF and (HF + VHF) as HF 
    hrv.data          <- CreateFreqAnalysis(hrv.data)
    hrv.data          <- CalculatePowerBand(hrv.data, indexFreqAnalysis=1, type="wavelet", wavelet="la8", bandtolerance=0.005, ULFmin=0, ULFmax=0.0033, VLFmin=0.0033, VLFmax=0.04, LFmin=0.04, LFmax=0.15, HFmin=0.15, HFmax= 1)
    
    # Plot bands
    #PlotPowerBand(hrv.data, indexFreqAnalysis=1, hr=TRUE)
    
    # Calculate HRV features
    ulf.power.a       <- mean(hrv.data$FreqAnalysis[[1]]$ULF)
    vlf.power.a       <- mean(hrv.data$FreqAnalysis[[1]]$VLF)
    lf.power.a        <- mean(hrv.data$FreqAnalysis[[1]]$LF)
    hf.power.a        <- mean(hrv.data$FreqAnalysis[[1]]$HF)
    total.power       <- ulf.power.a + vlf.power.a + lf.power.a + hf.power.a
    ulf.power.r       <- ulf.power.a/total.power * 100
    vlf.power.r       <- vlf.power.a/total.power * 100
    lf.power.r        <- lf.power.a/total.power * 100
    hf.power.r        <- hf.power.a/total.power * 100
    lf.power.nu       <- lf.power.a/(lf.power.a + hf.power.a) * 100
    hf.power.nu       <- hf.power.a/(lf.power.a + hf.power.a) * 100
    lfhf              <- lf.power.a/hf.power.a
    mean.hr           <- mean(hrv.data$HR)
    
    # Create parameter vector
    hrv.parameters      <- data.frame(mean.hr, lf.power.a, hf.power.a, total.power, lf.power.r, hf.power.r, lf.power.nu, hf.power.nu, lfhf)
    
    # Clean up
    rm(ulf.power.a, vlf.power.a, lf.power.a, hf.power.a, total.power, ulf.power.r, vlf.power.r, lf.power.r, hf.power.r, lf.power.nu, hf.power.nu, lfhf, mean.hr)
    
    # Compute start and end of the measurement
    measurement.start <- strftime(as.POSIXct(activity.end) - measurement.started.before * 60, format = "%Y-%m-%d %H:%M:%S")
    measurement.end   <- strftime(as.POSIXct(activity.end) - measurement.started.before * 60, format = "%Y-%m-%d %H:%M:%S")
    
    # Add parameter to feature vector
    hrv.features    <- rbind(hrv.features, data.frame(round(hrv.parameters, 2), activity, activity.start, activity.end, measurement.start, measurement.end, measurement, last.name, first.name, date.of.birth))
  }
}

# Write to csv file
if(file.exists("../data/features/hrv-features.csv")) {
  features <- read.csv("../data/features/hrv-features.csv", stringsAsFactors = F)
  write.csv(rbind(features, hrv.features), "../data/features/hrv-features.csv", row.names = F)
} else {
  write.csv(hrv.features, "../data/features/hrv-features.csv", row.names = F)
}