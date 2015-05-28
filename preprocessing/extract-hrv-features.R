# Remove all variables
rm(list = ls(all = T))  

# Set working directory
setwd("~/Entwicklung/bogutzky/repositories/non-disruptive-flow-measures/preprocessing")

# Load libraries
library(RHRV)

# Set properties
root.data.path          <- "../data/preprocessed-data/"
first.name              <- "Patrick"
last.name               <- "Buse"
date.of.birth           <- "1984-05-05"
activity                <- "Baseline" #Running
measurement.start.after <- 10

# Load all file names
data.path             <- paste(root.data.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/",  sep = "")
kubios.hrv.file.names <- list.files(path = data.path, pattern = "ecg-data-[1-9]_hrv.txt", recursive = T)

# Create hrv feature data frame
hrv.features  <- data.frame()

# Fill hrv features data frame
for (kubios.hrv.file.name in kubios.hrv.file.names) {
  
  # Load Kubios HRV txt data
  kubios.hrv.data   <- read.csv(paste(data.path, kubios.hrv.file.name, sep = ""), header = F, na.strings = "", fill = T, skip = 117, stringsAsFactors = FALSE, col.names = c("", "Time", "RR.interval", "FFT.Frequency", "FFT.PSD", "AR.Frequency", "AR.PSD", "VLF.comp.", "LF.comp.", "HF.comp.", ""))[,2:10]
  
  # Get time and rr-intervals
  time              <- kubios.hrv.data[, 1] - kubios.hrv.data[1, 1]
  time              <- time[complete.cases(time)]
  rr.interval       <- kubios.hrv.data[, 2]
  rr.interval       <- rr.interval[complete.cases(rr.interval)]
  
  # Create hrv data
  hrv.data          <- CreateHRVData()
  hrv.data          <- SetVerbose(hrv.data, FALSE)
  hrv.data$Beat     <- data.frame("Time"=time)
  
  # Build not interpolated heart rates
  hrv.data          <- BuildNIHR(hrv.data)
  #PlotNIHR(hrv.data)
  
  # Remove artefact manually
  #hrv.data          <- EditNIHR(hrv.data)
  #PlotNIHR(hrv.data)
  
  # Filter niHR
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
  
  # Extract properties
  activity.start    <- as.POSIXct(strptime(regmatches(kubios.hrv.file.name, regexpr("[0-9]{4}-[0-9]{2}-[0-9]{2}--[0-9]{2}-[0-9]{2}-[0-9]{2}", kubios.hrv.file.name)), "%Y-%m-%d--%H-%M-%S"))
  measurement.start <- activity.start + measurement.start.after * 60
  measurement.end   <- measurement.start + time[length(time)]
  measurement       <- substr(regmatches(kubios.hrv.file.name, regexpr("[1-9]{1}_", kubios.hrv.file.name)), 1, 1)
  
  activity.start    <- strftime(activity.start, format = "%Y-%m-%d %H:%M:%S")
  measurement.start <- strftime(measurement.start, format = "%Y-%m-%d %H:%M:%S")
  activity.end      <- measurement.end <- strftime(measurement.end, format = "%Y-%m-%d %H:%M:%S")
  activity.start    <- strftime(activity.start, format = "%Y-%m-%d %H:%M:%S")
  
  # Add parameter to feature vector
  hrv.features    <- rbind(hrv.features, data.frame(hrv.parameters, activity, activity.start, activity.end, measurement.start, measurement.end, measurement, last.name, first.name, date.of.birth))
}

# Write to csv file
if(file.exists("../data/features/hrv-features.csv")) {
  features <- read.csv("../data/features/hrv-features.csv", stringsAsFactors = FALSE)
  write.csv(rbind(features, hrv.features), "../data/features/hrv-features.csv", row.names = FALSE)
} else {
  write.csv(hrv.features, "../data/features/hrv-features.csv", row.names = FALSE)
}