
athletic.data <- read.table("http://mkahn.webspace.wheatoncollege.edu/math151/Datasets/athletics.txt",
                        header=TRUE)
attach(athletic.data)

t.test(GPA ~ Group, conf.level = 0.95, var.equal=TRUE)

summary(athletic.data)

IntramNon <- c(62,60,78,62,49,67,80,48)

athlete <- lm(Group ~ GPA, data = athletic.data)


summary(athlete)

confint(athlete)




heartattack.data <- read.table("http://mkahn.webspace.wheatoncollege.edu/math151/Datasets/heart-attack.txt",
                            header=TRUE)


boxplot(heartattack.data$MPV ~ heartattack.data$Diagnosis,
        xlab = "Diagnosis" , ylab = "MPV (fL)")


summary(aov(heartattack.data$MPV ~ heartattack.data$Diagnosis))


library("pwr")
pwr.p.test(h=ES.h(.50,.60), n=NULL,
           alternative="greater",
           power=.85,
           sig.level=0.05)


pwr.p.test(h=ES.h(.50,.60), n=NULL,
           alternative="greater",
           power=.85,
           sig.level=0.05)





density.data <- read.table("http://mkahn.webspace.wheatoncollege.edu/math151/Datasets/HDL.txt",
                       header = TRUE)
attach(density.data)

plot(Triglyc, HDL,
     xlab="Triglyc (mg/DL)", ylab="High-Density lipoprotein",
     pch = 20, col="blue")

density.data.c <- lm(Triglyc~HDL)

density.data.c <- lm(HDL~Triglyc+SPB)

abline(density.data.c$coefficients,
       col="black", lwd=3)

summary(density.data.c)
confint(density.data.c)










