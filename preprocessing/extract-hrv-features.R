# Remove all variables
rm(list = ls(all = T)) 

# Load libraries
require(RHRV)

# Set root data directory path
root.data.directory.path <- ""
if(file.exists("C:/Users/Simon Bogutzky/Documents/flow/data"))
  root.data.directory.path <- "C:/Users/Simon Bogutzky/Documents/flow/data/"
if(file.exists("/Volumes/flow/Documents/simon-bogutzky/data"))
  root.data.directory.path <- "/Volumes/flow/Documents/simon-bogutzky/data/"
if(file.exists("//gangstore.ddns.net/flow/Documents/simon-bogutzky/data"))
  root.data.directory.path <- "//gangstore.ddns.net/flow/Documents/simon-bogutzky/data/"

# Set preprocessed data directory path
preprocessed.data.directory.path <- "./data/preprocessed-data/"

# Set processed data directory path
processed.data.directory.path <- paste(root.data.directory.path, "processed-data/", sep = "")

# Set features directory path
features.directory.path <- paste(root.data.directory.path, "features/", sep = "")

# Load fss features
fss.features <- read.csv(paste(features.directory.path, "fss-features.csv", sep = ""), stringsAsFactors = F)

# Create hrv feature data frame
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
    date.directory  <- paste(strftime(as.POSIXct(activity.start / 1000, origin = "1970-01-01", tz="CET"), format="%Y-%m-%d--%H-%M-%S"), "/", sep ="")
  }
  
  hrv.time.data.path <- paste(processed.data.directory.path, tolower(activity), "/", tolower(last.name), "-", tolower(first.name), "/", date.directory, "hrv-time-data-", measurement, ".csv", sep="")
  if(file.exists(hrv.time.data.path)) {
    
    # Load time data
    hrv.time.data <- read.csv(hrv.time.data.path, skip = 2)
    
    # Set beat times
    beat.times <- hrv.time.data[, 1]
    
    # Create hrv data
    hrv.data          <- CreateHRVData()
    hrv.data          <- SetVerbose(hrv.data, T)
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
    hrv.data          <- CreateTimeAnalysis(hrv.data)
    
    
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
    
    # Add parameter to feature vector
    hrv.features    <- rbind(hrv.features, data.frame(round(hrv.parameters, 2), activity, activity.start, activity.end, measurement, last.name, first.name, date.of.birth))
  }
}

# Create output directory, if needed
output.directory.path <- features.directory.path
if(!file.exists(output.directory.path)) {
  dir.create(output.directory.path, recursive = TRUE)
}

# Write to csv file
output.file.path <- paste(output.directory.path, "hrv-features.csv", sep = "")
if(file.exists(output.file.path)) {
  features <- read.csv(output.file.path, stringsAsFactors = FALSE)
  write.csv(unique(rbind(features, hrv.features)), output.file.path, row.names = FALSE)
} else {
  write.csv(hrv.features, output.file.path, row.names = FALSE)
}