# Project: Hospital Quality of Care Scores Association with Clinical Outcomes in MIDAS   
# Author: Davit Sargsyan
# Created: 08/19/2016
# Modified: 02/04/2017
#*********************************************
require(data.table)
require(survival)
require(ggplot2)
require(gridExtra)

# Data----
# Scores
# Sheet1 of 'NJ HOSPITALS_Heart Attack Scores 02042017.xlsx' in docs
scores.mi <- fread("data\\NJ HOSPITALS_Heart Attack Scores 02042017.csv")
scores.mi <- droplevels(subset(scores.mi, 
                               subset = !is.na(scores.mi$`Report Year`),
                               select = c(1:3, 5, 7:9)))

# Hospital information
# Sheet2 of 'NJ HOSPITALS_Heart Attack Scores 02042017.xlsx' in docs
hosp.info <- fread("data\\NJ HOSPITALS Info 02042017.csv")
hosp.info <- unique(droplevels(subset(hosp.info, 
                                      subset = !is.na(hosp.info$`Report Year`),
                                      select = -1)))

# Merge the tables
dt1 <- merge(scores.mi,
             hosp.info,
             by = "Hospital  Name",
             all = TRUE)

# Remove 2014 data: incomplete
dt1 <- subset(dt1,
              `Report Year` != 2014)
  
  
# Convert variables
dt1$`Report Year` <- factor(dt1$`Report Year`)
dt1$`Hospital Number` <- factor(as.numeric(dt1$`Hospital Number`))
dt1$Teach <- factor(dt1$Teach)
dt1$BEDS <- as.numeric(as.character(dt1$BEDS))

dt1$`Overal Score (%)` <- as.numeric(as.character(dt1$`Overal Score (%)`))
dt1$`Asprin Discharge (%)` <- as.numeric(as.character(dt1$`Asprin Discharge (%)`))
dt1$`Beta Blocker Discharge (%)` <- as.numeric(as.character(dt1$`Beta Blocker Discharge (%)`))
dt1$`ACEI/ARB Discharge (%)` <- as.numeric(as.character(dt1$`ACEI/ARB Discharge (%)`))
dt1$`Smoking Cessation Advice (%)` <- as.numeric(as.character(dt1$`Smoking Cessation Advice (%)`))

summary(dt1)

# Number of hospitals that collected scores by year----
dt1[, nOveral := sum(!is.na(`Overal Score (%)`)),
    by = `Report Year`]
dt1[, nAspDsch := sum(!is.na(`Asprin Discharge (%)`)),
    by = `Report Year`]
dt1[, nBBDsch := sum(!is.na(`Beta Blocker Discharge (%)`)),
    by = `Report Year`]
dt1[, nACEI := sum(!is.na(`ACEI/ARB Discharge (%)`)),
    by = `Report Year`]
dt1[, nSmoke := sum(!is.na(`Smoking Cessation Advice (%)`)),
    by = `Report Year`]

t1 <- unique(subset(dt1,
                    select = c(2, 19:23)))
setkey(t1, `Report Year`)
names(t1)[1] <- "year"
t1

# Plot average scores over time----
# a. Overall----
avg.scores <- aggregate(dt1$`Overal Score (%)`,
                        FUN = mean,
                        by = list(dt1$`Report Year`),
                        na.rm = TRUE)
names(avg.scores) <- c("year", "overall.mu")
avg.scores$overall.sd <- aggregate(dt1$`Overal Score (%)`,
                                   FUN = sd,
                                   by = list(dt1$`Report Year`),
                                   na.rm = TRUE)$x

avg.scores$asp.mu <- aggregate(dt1$`Asprin Discharge (%)`,
                               FUN = mean,
                               by = list(dt1$`Report Year`),
                               na.rm = TRUE)$x
avg.scores$asp.sd <- aggregate(dt1$`Asprin Discharge (%)`,
                               FUN = sd,
                               by = list(dt1$`Report Year`),
                               na.rm = TRUE)$x

avg.scores$bb.mu <- aggregate(dt1$`Beta Blocker Discharge (%)`,
                               FUN = mean,
                               by = list(dt1$`Report Year`),
                               na.rm = TRUE)$x
avg.scores$bb.sd <- aggregate(dt1$`Beta Blocker Discharge (%)`,
                               FUN = sd,
                               by = list(dt1$`Report Year`),
                               na.rm = TRUE)$x

avg.scores$ace.mu <- aggregate(dt1$`ACEI/ARB Discharge (%)`,
                               FUN = mean,
                               by = list(dt1$`Report Year`),
                               na.rm = TRUE)$x
avg.scores$ace.sd <- aggregate(dt1$`ACEI/ARB Discharge (%)`,
                               FUN = sd,
                               by = list(dt1$`Report Year`),
                               na.rm = TRUE)$x

avg.scores$smoke.mu <- aggregate(dt1$`Smoking Cessation Advice (%)`,
                               FUN = mean,
                               by = list(dt1$`Report Year`),
                               na.rm = TRUE)$x
avg.scores$smoke.sd <- aggregate(dt1$`Smoking Cessation Advice (%)`,
                               FUN = sd,
                               by = list(dt1$`Report Year`),
                               na.rm = TRUE)$x

# Merge with counts
avg.scores <- merge(avg.scores,
                    t1,
                    by = "year")
avg.scores$year <- as.numeric(as.character(avg.scores$year))

avg.scores

# Plot average scores----
# a. Overal Score (%)
tiff(filename = "tmp/plot1.tiff",
     width = 8,
     height = 8,
     units = "in",
     res = 400)
ggplot(avg.scores) +
  geom_line(aes(x = year,
                y = overall.mu,
                colour = "blue")) +
  geom_point(aes(x = year,
                 y = overall.mu,
                 color = "blue"),
             size = 3) +
  scale_x_continuous("Year", 
                     breaks = 2004:2013) +
  scale_y_continuous("Score") +
  ggtitle("Average Overall MI Scores with 95% C.I. Over Time \n") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) + 
  geom_errorbar(aes(x = year,
                    ymin = overall.mu - qt(0.975, nOveral - 1)*overall.sd/sqrt(nOveral),
                    ymax = overall.mu + qt(0.975, nOveral - 1)*overall.sd/sqrt(nOveral)),
                color = "black",
                width = 0.1)
graphics.off()

# b. Asprin Discharge (%)
tiff(filename = "tmp/plot2.tiff",
     width = 8,
     height = 8,
     units = "in",
     res = 400)
ggplot(avg.scores) +
  geom_line(aes(x = year,
                y = asp.mu,
                colour = "blue")) +
  geom_point(aes(x = year,
                 y = asp.mu,
                 color = "blue"),
             size = 3) +
  scale_x_continuous("Year", 
                     breaks = 2004:2013) +
  scale_y_continuous("Score") +
  ggtitle("Average Asprin Discharge MI Scores with 95% C.I. Over Time \n") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) + 
  geom_errorbar(aes(x = year,
                    ymin = asp.mu - qt(0.975, nAspDsch - 1)*asp.sd/sqrt(nAspDsch),
                    ymax = asp.mu + qt(0.975, nAspDsch - 1)*asp.sd/sqrt(nAspDsch)),
                color = "black",
                width = 0.1)
graphics.off()

# c. Beta Blocker Discharge (%)
tiff(filename = "tmp/plot3.tiff",
     width = 8,
     height = 8,
     units = "in",
     res = 400)
ggplot(avg.scores) +
  geom_line(aes(x = year,
                y = bb.mu,
                colour = "blue")) +
  geom_point(aes(x = year,
                 y = bb.mu,
                 color = "blue"),
             size = 3) +
  scale_x_continuous("Year", 
                     breaks = 2004:2013) +
  scale_y_continuous("Score") +
  ggtitle("Average Beta Blocker Discharge MI Scores with 95% C.I. Over Time \n") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) + 
  geom_errorbar(aes(x = year,
                    ymin = bb.mu - qt(0.975, nBBDsch - 1)*bb.sd/sqrt(nBBDsch),
                    ymax = bb.mu + qt(0.975, nBBDsch - 1)*bb.sd/sqrt(nBBDsch)),
                color = "black",
                width = 0.1)
graphics.off()

# d. ACEI/ARB Discharge (%)
tiff(filename = "tmp/plot4.tiff",
     width = 8,
     height = 8,
     units = "in",
     res = 400)
ggplot(avg.scores) +
  geom_line(aes(x = year,
                y = ace.mu,
                colour = "blue")) +
  geom_point(aes(x = year,
                 y = ace.mu,
                 color = "blue"),
             size = 3) +
  scale_x_continuous("Year", 
                     breaks = 2004:2013) +
  scale_y_continuous("Score") +
  ggtitle("Average ACEI/ARB Discharge MI Scores with 95% C.I. Over Time \n") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) + 
  geom_errorbar(aes(x = year,
                    ymin = ace.mu - qt(0.975, nACEI - 1)*ace.sd/sqrt(nACEI),
                    ymax = ace.mu + qt(0.975, nACEI - 1)*ace.sd/sqrt(nACEI)),
                color = "black",
                width = 0.1)
graphics.off()

# e. Smoking Cessation Advice (%)
tiff(filename = "tmp/plot5.tiff",
     width = 8,
     height = 8,
     units = "in",
     res = 400)
ggplot(avg.scores) +
  geom_line(aes(x = year,
                y = smoke.mu,
                colour = "blue")) +
  geom_point(aes(x = year,
                 y = smoke.mu,
                 color = "blue"),
             size = 3) +
  scale_x_continuous("Year", 
                     breaks = 2004:2013) +
  scale_y_continuous("Score") +
  ggtitle("Average Smoking Cessation Advice MI Scores with 95% C.I. Over Time \n") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45,
                                   hjust = 1)) + 
  geom_errorbar(aes(x = year,
                    ymin = smoke.mu - qt(0.975, nSmoke - 1)*smoke.sd/sqrt(nSmoke),
                    ymax = smoke.mu + qt(0.975, nSmoke - 1)*smoke.sd/sqrt(nSmoke)),
                color = "black",
                width = 0.1)
graphics.off()

#*********************************************
