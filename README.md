# PsychoPhysioPipeline: A Processing and Analysis Pipeline for Psychophysiological Research
Authors: Simon Bogutzky, Phillip Marsch

License: [MIT](https://opensource.org/licenses/MIT)

Version: 2.0.1

Document version: 1.0.5 

Date: 09/04/2016

[![DOI](https://zenodo.org/badge/23671/sbogutzky/PsychoPhysioPipeline.svg)](https://zenodo.org/badge/latestdoi/23671/sbogutzky/PsychoPhysioPipeline)
[![status](http://joss.theoj.org/papers/6eb8fe64344fa4671a66f49eed43fad5/status.svg)](http://joss.theoj.org/papers/6eb8fe64344fa4671a66f49eed43fad5)

## What is the PsychoPhysioPipeline for R?
The PsychoPhysioPipeline (PPP) is a pipeline of several small R programs for segmenting the collected data of the [PsychoPhysioCollector](https://github.com/sbogutzky/PsychoPhysioCollector/), for identifying potential implicit flow characteristics and for their analysis.

## Using the PsychoPhysioPipeline

### Obtain Data for the PsychoPhysioPipeline
To obtain data for the PsychoPhysioPipeline, we recommend to use the Android App „PsychoPhysioCollector“. The latest release is available on [GitHub](https://github.com/sbogutzky/PsychoPhysioCollector/releases/latest).

### Installation Instructions
1) Install the following packages: 

```r
install.packages(c("zoom", "signal", "pracma", "car"))
```

*I used R version 3.3.0*

2) Compile the flow package or get the [latest version](https://github.com/sbogutzky/PsychoPhysioPipeline/releases/latest) (2.0.0) from the package directory

3) Install devtools and use their build() and install() functions, which do automatically install the dependencies.

4) Change the working directory to the processing directory

### Processing Steps
1) Place the data in the following stucture ROOT\_DIR/raw-data/ACTIVITY\_DIR/USER\_DIR

*You will find example data in the current repository: ecg.csv = ECG data; imu-rn42-bc98.csv = Kinematic data from the left leg; imu-rn42-3b70.csv = Kinematic data from the right leg. Run each R program and follow the instructions in the console window. The data come from a 29 year-old man. He ran 4.5 km and answered a questionnaire at the end of the session (Flow Short-Scale).*

2) Use processing/get-fss-features.R to get the flow short scale dimensions. The input is not case-sensitive. The result can be find ROOT\_DIR/features/ACTIVITY\_DIR/USER\_DIR.

*Example data: First name = Simon; Last name = Bogutzky; Birthday = 1984-02-23; Activity = Running. You get one file with the results of one questionnaire. If you have more surveys in one session, you will get more results with timestamp in one file.*

3) Use processing/subset-data.R to subset the data. The input is not case-sensitive. The result can be find ROOT\_DIR/processed-data/ACTIVITY\_DIR/USER\_DIR/DATE\_DIR. The visualization at the end is only for checking the segments and its data. The range of each segment of data is based on the times of self-reports in self-report.csv. Example: If you have a session with motion data and 4 self-reports at minute 10' 20' 30' and 40', you will get four segments of data.

*Example data: First name = Simon; Last name = Bogutzky; Activity = Running; Filename = imu-rn42-3b70 OR imu-rn42-bc98 OR ecg OR gps-position (GPS data = Y).*

4) Use KubiosHRV to get RR intervals and store them in ROOT\_DIR/processed-data/ACTIVITY\_DIR/USER\_DIR/DATE\_DIR/SENSOR_NAME_hrv.txt. Store it with comma seperation (see preferences).

*KubiosHRV is a free software for the analysis of the heart rate varibility (HRV). We need it to extract RR intervals (heartbeat to heartbeat intervals). You will get it on [their website](http://kubios.uef.fi). See their documentation for more details, how to use it.*

5) Use processing/get-stride-data.R to get the stride data. The input is not case-sensitive. In the first plot you can select anomalies to get rid of these anomalies. In the following process you can add missing events or remove wrong events. The plots after the adding and removing are control plots. The result can be find ROOT\_DIR/processed-data/ACTIVITY\_DIR/USER\_DIR/DATE\_DIR.

*Example data: First name = Simon; Last name = Bogutzky; Activity = Running; Filename = imu-rn42-3b70 OR imu-rn42-bc98. The script uses the x rotation in deg/s to detect the midswing (MS) event of the gait cycle (step). A good threshold is 200 deg/s. The result can be differ from the example data (depends on the selected MS events; around 1992 MS events in each file)*

6) Use processing/compute-optimal-cut-off-frequencies.R to compute the optimal cut off frequencies for the jerk-cost calculation. The input is not case-sensitive. If you have more participant the script calculate average cut off frequencies (longer processing time). There is no result file (please note you the cut off frequencies for each axis for next step). 

*Example data: Activity = Running; Filename = imu-rn42-3b70 OR imu-rn42-bc98.*

7) Use processing/get-jerk-cost-data.R to get the jerk-cost data. The input is not case-sensitive. The result can be find ROOT\_DIR/processed-data/ACTIVITY\_DIR/USER\_DIR/DATE\_DIR.

*Example data: First name = Simon; Last name = Bogutzky; Activity = Running; Filename = imu-rn42-3b70 OR imu-rn42-bc98; Cut off frequency X = 2.5, Y = 4.5, Z = 6. The result can be differ from the example data (depends on the selected MS events and the cut off frequencies; control the console output -- mean stride around 76 1/min and mean jerk-cost around 40000 m^2/s^-5)*

8) After step 4 and step 5, use processing/get-cardiolocomotor-phase-synchronization.R to get the cardiolocomotor phase synchronization data. The input is not case-sensitive. The visualization at the end is only for checking the cardiolocomotor phase synchronization. If there is high sync, you will see horizontal lines and the indices are high (up to one). The result can be find ROOT\_DIR/processed-data/ACTIVITY\_DIR/USER\_DIR/DATE\_DIR.

*Example data: First name = Simon; Last name = Bogutzky; Activity = Running; Filenames = ecg AND imu-rn42-3b70 AND imu-rn42-bc98; Time window = 30. The result can be differ from the example data (depends on the selected MS events and R peaks detected in Kubios HRV; control the console output -- mean Normalized Shannon Entropy Index 0.02 and mean Coherence Index around 0.02)*

*Note: You will have x times data files for each questionnaire in your session*

9) Make a report. You find an example in report/example.Rmd (Change the working directory to the report directory)

*Last step: Analysis effects or correlations. The file is only a template for an analysis report.*

### Example Usage
The pilot deployment was successfully used in the research project Flow-Machines ("Flow-Machines: Body Movement and Sound", 2012-2015) at the University of Applied Sciences Bremen and funded by German Federal Ministry of Education and Research (BMBF; Förderkennzeichen: 03FH084PX2).

## Author and Contribution
As by the License this is free software released by the University of Applied Sciences Bremen. The authors (Simon Bogutzky and Phillip Marsch) welcome external contributors to freely use and extend this software. If you need some help, please write an [issue](https://github.com/sbogutzky/PsychoPhysioPipeline/issues). 

## Acknowledgement
This work is part of the research project Flow-Machines ("Flow-Machines: Body Movement and Sound", 2012-2015) at the University of Applied Sciences Bremen and funded by German Federal Ministry of Education and Research (BMBF; Förderkennzeichen: 03FH084PX2). 
