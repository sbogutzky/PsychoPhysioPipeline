# Load hrv, jerk cost and cls data
kubios.hrv.data.file.path <- paste(root.path, processed.data.directory, activity.directory, user.directory, date.directory, kubios.hrv.data.file.name, sep = "")
source("../processing/code-snippets/get-kubios-hrv-data.R")
jc.data.r <- read.csv(paste(root.path, processed.data.directory, activity.directory, user.directory, date.directory, jc.data.file.name.r, sep = ""))
jc.data.l <- read.csv(paste(root.path, processed.data.directory, activity.directory, user.directory, date.directory, jc.data.file.name.l, sep = ""))
jc.data <- rbind(jc.data.r, jc.data.l)
jc.data <- jc.data[order(jc.data[, 1]),]
cls.phase.data <- read.csv(paste(root.path, processed.data.directory, activity.directory, user.directory, date.directory, cls.phase.data.file.name, sep = ""))
cls.index.data <- read.csv(paste(root.path, processed.data.directory, activity.directory, user.directory, date.directory, cls.index.data.file.name, sep = ""))

n <- max(cls.phase.data[complete.cases(cls.phase.data), 1]) - 13 * 60 * 1000   # min(cls.data[complete.cases(cls.data), 1]) 
m <- max(cls.phase.data[complete.cases(cls.phase.data), 1]) - 12 * 60 * 1000  

# Subset data
jc.data <- jc.data[jc.data[, 1] > n & jc.data[, 1] < m, ]
cls.phase.data <- cls.phase.data[cls.phase.data[, 1] > n & cls.phase.data[, 1] < m, ]
cls.index.data <- cls.index.data[cls.index.data[, 1] > n & cls.index.data[, 1] < m, ]
kubios.hrv.data <- kubios.hrv.data[kubios.hrv.data[, 1] * 1000 > n & kubios.hrv.data[, 1] * 1000 < m, ]

stride.times <- c(jc.data[1, 1] - jc.data[1, 1], jc.data[, 1])

# Plot data
plot(jc.data[, 1] / 1000, 60 / diff(stride.times / 1000), xlab = "", ylab = "Mittlere SF ($1/min$)", xaxs = "i", yaxs = "i", ylim = c(135, 205), pch = 23, bg = rgb(0/255, 152/255, 199/255), xlim = c(min(jc.data[, 1]), max(jc.data[, 1]))/1000)
grid(col = rgb(186/255, 187/255, 194/255))
box()

heart.beat.times <- c(kubios.hrv.data[1, 1] - kubios.hrv.data[1, 1], kubios.hrv.data[, 1])
plot(kubios.hrv.data[, 1], 60 / diff(heart.beat.times), xlab = "", ylab = "Mittlere HR ($1/min$)", xaxs = "i", yaxs = "i", ylim = c(160, 180), pch = 21, bg = rgb(229/255, 66/255, 66/255), xlim = c(min(jc.data[, 1]), max(jc.data[, 1]))/1000)
grid(col = rgb(186/255, 187/255, 194/255))
box()

plot(cls.phase.data[, 1] / 1000, cls.phase.data[, 3], xlab = "", ylab = "Rel. Phase ($Psi(t)$)", xaxs = "i", yaxs = "i", ylim = c(0, 1), pch = 24, bg = rgb(96/255, 65/255, 79/255), xlim = c(min(jc.data[, 1]), max(jc.data[, 1]))/1000)
grid(col = rgb(186/255, 187/255, 194/255))
box()

plot(cls.index.data[, 1] / 1000, cls.index.data[, 3], type = "l", xlab = "Zeit (s)", ylab = "N. Shan. Entr. Index", xaxs = "i", yaxs = "i", ylim = c(0, 1), xlim = c(min(jc.data[, 1]), max(jc.data[, 1]))/1000)
grid(col = rgb(186/255, 187/255, 194/255))
box()

# Clean up
rm(kubios.hrv.data.file.path, kubios.hrv.data, jc.data.r, jc.data.l, jc.data, cls.phase.data, cls.index.data, n, m, stride.times)
