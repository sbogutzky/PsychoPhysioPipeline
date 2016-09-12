n <- max(cls.phase.data[complete.cases(cls.phase.data), 1]) - 120000 # min(cls.data[complete.cases(cls.data), 1]) 
m <- max(cls.phase.data[complete.cases(cls.phase.data), 1]) - 60000 

jc.data <- jc.data[jc.data[, 1] > n & jc.data[, 1] < m, ]
cls.phase.data <- cls.phase.data[cls.phase.data[, 1] > n & cls.phase.data[, 1] < m, ]
cls.index.data <- cls.index.data[cls.index.data[, 1] > n & cls.index.data[, 1] < m, ]
kubios.hrv.data <- kubios.hrv.data[kubios.hrv.data[, 1] * 1000 > n & kubios.hrv.data[, 1] * 1000 < m, ]

stride.times <- c(jc.data[1, 1] - jc.data[1, 1], jc.data[, 1])
plot(jc.data[, 1] / 1000, 60 / diff(stride.times /1000), xlab = "", ylab = "Mittlere Doppelschrittfrequenz ($1/min$)", xaxs = "i", yaxs = "i", ylim = c(80, 100), pch = 23, bg = rgb(0/255, 152/255, 199/255), xlim = c(min(jc.data[, 1]), max(jc.data[, 1]))/1000)
grid(col = rgb(186/255, 187/255, 194/255))
box()

heart.beat.times <- c(kubios.hrv.data[1, 1] - kubios.hrv.data[1, 1], kubios.hrv.data[, 1])
plot(kubios.hrv.data[, 1], 60 / diff(heart.beat.times), xlab = "", ylab = "Mittlere HR ($1/min$)", xaxs = "i", yaxs = "i", ylim = c(150, 200), pch = 21, bg = rgb(229/255, 66/255, 66/255), xlim = c(min(jc.data[, 1]), max(jc.data[, 1]))/1000)
grid(col = rgb(186/255, 187/255, 194/255))
box()

plot(cls.phase.data[, 1] / 1000, cls.phase.data[, 3], xlab = "", ylab = "Rel. Phase ($Psi(t)$)", xaxs = "i", yaxs = "i", ylim = c(0, 1), pch = 24, bg = rgb(96/255, 65/255, 79/255), xlim = c(min(jc.data[, 1]), max(jc.data[, 1]))/1000)
grid(col = rgb(186/255, 187/255, 194/255))
box()

plot(cls.index.data[, 1] / 1000, cls.index.data[, 3], type = "l", xlab = "Zeit (s)", ylab = "Norm. Shannon Entropie Index", xaxs = "i", yaxs = "i", ylim = c(0, 1), xlim = c(min(jc.data[, 1]), max(jc.data[, 1]))/1000)
grid(col = rgb(186/255, 187/255, 194/255))
box()

