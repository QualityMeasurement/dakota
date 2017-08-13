# |------------------------------------------------------------------------|
# | Project1: Hospital Quality of Care Scores Association                  |
# |           with Clinical Outcomes in MIDAS                              |
# | Script: Analysis                             |
# | Authors: Georgia Barbayannis, I-ming Chiu, Noah Michel, Davit Sargsyan |   
# | Created: 05/12/2017                                                    |
# | Modified: 07/22/2017                                                   |
# |------------------------------------------------------------------------|
# Header----
# Move up one directory
wd <- getwd()
setwd("..")
DATA_HOME <- paste(getwd(),
                   "data/dakota",
                   sep = "/")
# Reset working directory
setwd(wd)
getwd()

require(data.table)
require(survival)
require(ggplot2)
require(gridExtra)
require(knitr)

#**********************************************************
# PART I: Data----
## a. MIDAS data----
# Load data
load(file.path(DATA_HOME, "case_07222017.RData"))

## b. Myocardial Infarction (MI) scores----
# CSV saved from an Excel file on 06/02/2017
scores.mi <- fread(file.path(DATA_HOME,
                             "NJ HOSPITALS_Heart Attack Scores 2017.csv"))

# Convert variables
names(scores.mi)
scores.mi$`Hospital Division` <- as.numeric(as.character(scores.mi$`Hospital Division`))
scores.mi$Teach <- factor(as.numeric(scores.mi$Teach))
scores.mi$`Overall Score (%)` <- as.numeric(scores.mi$`Overall Score (%)`)
scores.mi$`Asprin Discharge (%)` <- as.numeric(scores.mi$`Asprin Discharge (%)`)
scores.mi$`Beta Blocker Discharge (%)` <- as.numeric(scores.mi$`Beta Blocker Discharge (%)`)
scores.mi$`ACEI/ARB Discharge (%)` <- as.numeric(scores.mi$`ACEI/ARB Discharge (%)`)

# Keep only useful variables
scores.mi <- subset(scores.mi,
                    select = c(1:4, 12:13, 15, 17:18))
summary(scores.mi)

## c. Merge the 2 dataset
names(scores.mi)[c(1, 3, 4)] <- c("dschyear",
                                  "HOSP",
                                  "DIV")
dt1 <- merge(scores.mi,
             case,
             by = c("dschyear",
                    "HOSP",
                    "DIV"))
dt1
summary(dt1)

#**********************************************************
# Endpoints----
# a. All-cause death----
dt1$dead <- FALSE
dt1$dead[!is.na(dt1$NEWDTD)] <- TRUE
addmargins(table(dt1$dead))

# Censor the data: if patients did not die until 01/01/2016, censor
summary(dt1$NEWDTD)
dt1$NEWDTD[!dt1$dead] <- as.Date("2016-01-01")

# Days to death
dt1$days2death <- as.numeric(as.character(difftime(dt1$NEWDTD,
                                                   dt1$DSCHDAT,
                                                   units = "days")))
hist(dt1$days2death, 100)
summary(dt1$days2death)

# b. CV death---
# Source: http://www.health.state.ok.us/stats/Vital_Statistics/Death/039_causes.shtml
# |-------------------------------------------------------------------|
# | Major cardiovascular diseases        | I00-I78                    |
# | Diseases of heart	                   | I00-I09, I11, I13, I20-I51 |
# | Hypertensive heart disease with      |                            | 
# |    or without renal disease          | I11,I13                    |
# | Ischemic heart diseases	             | I20-I25                    |
# | Other diseases of heart	             | I00-I09,I26-I51            |
# | Essential (primary) hypertension     |                            |
# |    and hypertensive renal disease	   | I10,I12                    |
# | Cerebrovascular diseases	           | I60-I69                    |
# | Atherosclerosis	                     | I70                        |
# | Other diseases of circulatory system | I71-I78                    |
# |-------------------------------------------------------------------|
unique(dt1$CAUSE)
# Define CV death----
dt1$cvdead <- FALSE
dt1$cvdead[dt1$dead & substr(dt1$CAUSE,1, 1) == "I"] <- TRUE

# Days to CV death----
dt1$days2cvdeath <- dt1$days2death
dt1$days2cvdeath[!dt1$cvdead] <- as.Date("2016-01-01")

# CV death vs All-cause death----
t1 <- table(all_cause_death = dt1$dead,
            cv_death = dt1$cvdead)
t1
kable(t1)
# |      | FALSE|  TRUE|
# |:-----|-----:|-----:|
# |FALSE | 77938|     0|
# |TRUE  | 19242| 20139|

kable(round(100*t1/sum(t1), 1))
# |      | FALSE| TRUE|
# |:-----|-----:|----:|
# |FALSE |  66.4|  0.0|
# |TRUE  |  16.4| 17.2|

# c. AMI readmissions----
# Censor the data: if patients did not have second MI until 01/01/2016, censor
dt1$post.ami.dx1.dat[!dt1$post.ami.dx1] <- as.Date("2016-01-01")

# Days to MI readmisson
dt1$days2mi2 <- as.numeric(as.character(difftime(dt1$post.ami.dx1.dat,
                                                 dt1$DSCHDAT,
                                                 units = "days")))
hist(dt1$days2mi2, 100)
summary(dt1$days2mi2)

dt1$DEC = round(dt1$AGE/10)

# Part II: Models----
# 1. Logistic models for 30-day AMI readmission, aspirin discharge----
# NOTE: the denominator might include people who did not survive the 30-day period
dt1$ami.readm.30 <- FALSE
dt1$ami.readm.30[dt1$days2mi2 < 31] <- TRUE
dt1$ami.readm.30 <- factor(dt1$ami.readm.30)
# a. Outcome vs. scores----
m1a <- glm(ami.readm.30 ~ `Asprin Discharge (%)`,
           family = binomial(logit),
           data = dt1)
s1a <- summary(m1a)
s1a
coeff <- s1a$coefficients
res1a <- data.table(nn = rownames(coeff),
                    est = round(exp(coeff[, 1]), 3),
                    ub = round(exp(coeff[, 1] + 1.96*coeff[, 2]), 3),
                    lb = round(exp(coeff[, 1] - 1.96*coeff[, 2]), 3),
                    pval = round(coeff[, 4], 3))
res1a <- res1a[-1, ]
res1a
res1a.plot <- res1a

res1a.plot$nn <- factor(res1a.plot$nn,
                       levels = res1a.plot$nn)

p1a <- ggplot(res1a.plot, 
             aes(x = nn, 
                 y = est)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lb, 
                    ymax = ub), 
                width = .1) +
  geom_hline(yintercept = 1,
             linetype = "dashed") +
  scale_x_discrete("") +
  scale_y_continuous("Odds Ratio",
                     limits = c(min(res1c$lb)-0.2, max(res1c$ub)+0.2),
                     breaks = seq(min(res1c$lb)-0.2, max(res1c$ub)+0.2,
                                  by = 0.1)) +
  ggtitle("1-Month Risk of AMI Readmission") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))
p1a


m1b <- glm(ami.readm.30 ~ Teach,
           family = binomial(logit),
           data = dt1)
s1b <- summary(m1b)
s1b
coeff <- s1b$coefficients
res1b <- data.table(nn = rownames(coeff),
                    est = round(exp(coeff[, 1]), 3),
                    ub = round(exp(coeff[, 1] + 1.96*coeff[, 2]), 3),
                    lb = round(exp(coeff[, 1] - 1.96*coeff[, 2]), 3),
                    pval = round(coeff[, 4], 3))
res1b <- res1b[-1, ]
res1b
res1b.plot <- res1b

res1b.plot$nn <- factor(res1b.plot$nn,
                        levels = res1b.plot$nn)

p1b <- ggplot(res1b.plot, 
              aes(x = nn, 
                  y = est)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lb, 
                    ymax = ub), 
                width = .1) +
  geom_hline(yintercept = 1,
             linetype = "dashed") +
  scale_x_discrete("") +
  scale_y_continuous("Odds Ratio",
                     limits = c(min(res1c$lb)-0.2, max(res1c$ub)+0.2),
                     breaks = seq(min(res1c$lb)-0.2, max(res1c$ub)+0.2,
                                  by = 0.1)) +
  ggtitle("1-Month Risk of AMI Readmission") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))
p1b


m1c <- glm(ami.readm.30 ~ `Asprin Discharge (%)`+
             Teach +
             SEX +
             DEC +
             PRIME +
             RACE +  
             HISPAN,
           family = binomial(logit),
           data = dt1)
s1c <- summary(m1c)
s1c

coeff <- s1c$coefficients
res1c <- data.table(nn = rownames(coeff),
                    est = round(exp(coeff[, 1]), 3),
                    ub = round(exp(coeff[, 1] + 1.96*coeff[, 2]), 3),
                    lb = round(exp(coeff[, 1] - 1.96*coeff[, 2]), 3),
                    pval = round(coeff[, 4], 3))
res1c <- res1c[-c(1,5), ]
res1c
res1c.plot <- res1c

res1c.plot$nn <- factor(res1c.plot$nn,
                        levels = res1c.plot$nn)

p1c <- ggplot(res1c.plot, 
              aes(x = nn, 
                  y = est)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lb, 
                    ymax = ub), 
                width = .1) +
  geom_hline(yintercept = 1,
             linetype = "dashed") +
  scale_x_discrete("") +
  scale_y_continuous("Odds Ratio",
                     limits = c(min(res1c$lb)-0.2, max(res1c$ub)+0.2),
                     breaks = seq(min(res1c$lb)-0.2, max(res1c$ub)+0.2,
                                  by = 0.1)) +
  ggtitle("1-Month Risk of AMI Readmission") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))
p1c

m1d <- glm(ami.readm.30 ~ `Asprin Discharge (%)`+
             Teach +
             SEX +
             DEC +
             PRIME +
             RACE +  
             HISPAN +
             hchf.acute +
             hchf.chron +
             hhyp +
             hdiab +
             hcld +
             hckd +
             hcopd +
             hlipid,
           family = binomial(logit),
           data = dt1)

s1d <- summary(m1d)
s1d

coeff <- s1d$coefficients
res1d <- data.table(nn = rownames(coeff),
                    est = round(exp(coeff[, 1]), 3),
                    ub = round(exp(coeff[, 1] + 1.96*coeff[, 2]), 3),
                    lb = round(exp(coeff[, 1] - 1.96*coeff[, 2]), 3),
                    pval = round(coeff[, 4], 3))
res1d <- res1d[-c(1,5), ]
res1d
res1d.plot <- res1d

res1d.plot$nn <- factor(res1d.plot$nn,
                        levels = res1d.plot$nn)

p1d <- ggplot(res1d.plot, 
              aes(x = nn, 
                  y = est)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lb, 
                    ymax = ub), 
                width = .1) +
  geom_hline(yintercept = 1,
             linetype = "dashed") +
  scale_x_discrete("") +
  scale_y_continuous("Odds Ratio",
                     limits = c(min(res1c$lb)-0.2, max(res1c$ub)+0.2),
                     breaks = seq(min(res1c$lb)-0.2, max(res1c$ub)+0.2,
                                  by = 0.1)) +
  coord_flip()
  ggtitle("1-Month Risk of AMI Readmission") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))
p1d



CONTINUE HERE! (08/11/2017)
2. Define a mulilevel responses for 30, 90, 180 days and 1 year for all-cause death, CV death and AMI readmission.  
3. See notes form 07/21/2017 meeting in README file for more

dt1$HOSP <- factor(dt1$HOSP)
dt1$dschyear <- factor(dt1$dschyear)


dt1$ami.or.cv.death <- FALSE
dt1$ami.or.cv.death[dt1$days2cvdeath < 31 |
                      dt1$days2mi2 < 31] <- TRUE
table(dt1$ami.or.cv.death)











Surv(dt1$days2mi2, dt1$mi2.dx1)
m0 <- coxph(Surv(days2mi2, mi2.dx1) ~ HOSP*dschyear,
            data = dt1)

Surv(dt1$days2mi2)
m0 <- coxph(Surv(days2mi2) ~ HOSP*dschyear,
            data = dt1)

m0
summary(m0)

#**********************************************************
# Risk factors----
# Age by decades
dt1$dec <- round(dt1$AGE/10)
hist(dt1$dec)

# Hispanic: reorder the levels----
dt1$HISPAN <- factor(as.character(dt1$HISPAN),
                     levels =   c("Non-hispanic",
                                  "Hispanic", 
                                  "Unknown"))

#**********************************************************
# PART II: Models----
summary(dt1)
# Model1: logistic----
# # Alternatively: quartiles
# quantile(dt2$hincome,
#          c(0.25, 0.5, 0.75))
# # 25%   50%   75% 
# # 41566 53827 67487 
# dt2$income.cat <- "No more than $40k"
# dt2$income.cat[dt2$hincome > 40000 &
#                  dt2$hincome <= 50000] <- "$40k to $50k"
# dt2$income.cat[dt2$hincome > 50000 &
#                  dt2$hincome <= 70000] <- "$50k to $70k"
# dt2$income.cat[dt2$hincome > 70000] <- "Over $70k"
# dt2$income.cat <- factor(dt2$income.cat, 
#                          levels = c("No more than $40k",
#                                     "$40k to $50k",
#                                     "$50k to $70k",
#                                     "Over $70k"))
table(dt2$income.cat)

# a. AMI readmission within 1 year of first MI----

dt1$days2mi_1m = FALSE
dt1[dt1$days2mi2 <= 30,]$days2mi_1m = TRUE
dt1$days2mi_3m = FALSE
dt1[dt1$days2mi2 <= 60,]$days2mi_3m = TRUE
dt1$days2mi_6m = FALSE
dt1[dt1$days2mi2 <= 180,]$days2mi_6m = TRUE
dt1$days2mi_1y = FALSE
dt1[dt1$days2mi2 <= 365,]$days2mi_1y = TRUE

dt1$days2death_1y = FALSE
dt1[dt1$days2cvdeath <= 365,]$days2death_1y = TRUE

m1 <- glm(dt1$days2death_1y ~ 
            dschyear +
            SEX + 
            RACE + 
            HISPAN +
            dec +
            PRIME,
          family = binomial(logit),
          data = dt1)
s1 <- summary(m1)
s1
coeff <- s1$coefficients
res1 <- data.table(nn = rownames(coeff),
                  est = round(exp(coeff[, 1]), 3),
                  ub = round(exp(coeff[, 1] + 1.96*coeff[, 2]), 3),
                  lb = round(exp(coeff[, 1] - 1.96*coeff[, 2]), 3),
                  pval = round(coeff[, 4], 3))
res1 <- res1[-1, ]
res1

















# Plot ORs----
res1.plot <- res1[-c(3, 6, 8, 11),]
res1.plot$nn <- factor(res1.plot$nn,
                       levels = res1.plot$nn,
                       labels = c("Income > $50k",
                                  "Bill > $40k",
                                  "Male",
                                  "Black",
                                  "Hispanic",
                                  "Age (x10)",
                                  "Commercial Insurance"))

p1 <- ggplot(res1.plot, 
             aes(x = nn, 
                 y = est)) + 
  coord_flip() +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lb, 
                    ymax = ub), 
                width = .1) +
  geom_hline(yintercept = 1,
             linetype = "dashed") +
  scale_x_discrete("") +
  scale_y_continuous("Odds Ratio",
                     limits = c(0.6, 1.8),
                     breaks = seq(0.6, 1.8,
                                  by = 0.2)) +
  ggtitle("1-Year Risk of AMI Readmission") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))
p1

# b. All-cause death within 1 year of first MI----
m2 <- glm(dt2$dead.1y ~ income.cat +
            bill +
            dschyear +
            SEX + 
            RACE + 
            HISPAN +
            dec +
            PRIME,
          family = binomial(logit),
          data = dt2)
s2 <- summary(m2)
s2
coeff <- s2$coefficients
res2 <- data.table(nn = rownames(coeff),
                  est = round(exp(coeff[, 1]), 3),
                  ub = round(exp(coeff[, 1] + 1.96*coeff[, 2]), 3),
                  lb = round(exp(coeff[, 1] - 1.96*coeff[, 2]), 3),
                  pval = round(coeff[, 4], 3))
res2 <- res2[-1, ]
res2

# Plot ORs----
res2.plot <- res2[-c(3, 6, 8, 11),]
res2.plot$nn <- factor(res2.plot$nn,
                       levels = res2.plot$nn,
                       labels = c("Income > $50k",
                                  "Bill > $40k",
                                  "Male",
                                  "Black",
                                  "Hispanic",
                                  "Age (x10)",
                                  "Commercial Insurance"))

p2 <- ggplot(res2.plot, 
             aes(x = nn, 
                 y = est)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lb, 
                    ymax = ub), 
                width = .1) +
  geom_hline(yintercept = 1,
             linetype = "dashed") +
  scale_x_discrete("") +
  scale_y_continuous("Odds Ratio",
                     limits = c(0.6, 1.8),
                     breaks = seq(0.6, 1.8,
                                  by = 0.2)) +
  ggtitle("1-Year Risk of All-Cause Death") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))
p2

# Model2: Survival----
# a. AMI readmissions----
m3 <- coxph(Surv(days2mi2, mi2.dx1) ~ HOPS*dschyear +
              SEX + 
              RACE + 
              HISPAN +
              dec +
              PRIME,
            data = dt2)
m3
s3 <- summary(m3)
coeff <- s3$coefficients
res3 <- data.table(nn = rownames(coeff),
                   round(s3$conf.int[, -2], 3),
                   pval = round(coeff[, 5], 3))
names(res3)[2:4] <- c("est", "lb", "ub")
res3

# Plot HRs----
res3.plot <- res3[-c(3, 6, 8, 11),]
res3.plot$nn <- factor(res3.plot$nn,
                       levels = res3.plot$nn,
                       labels = c("Income > $50k",
                                  "Bill > $40k",
                                  "Male",
                                  "Black",
                                  "Hispanic",
                                  "Age (x10)",
                                  "Commercial Insurance"))

p3 <- ggplot(res3.plot, 
             aes(x = nn, 
                 y = est)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lb, 
                    ymax = ub), 
                width = .1) +
  geom_hline(yintercept = 1,
             linetype = "dashed") +
  scale_x_discrete("") +
  scale_y_continuous("Hazard Ratio",
                     limits = c(0.85, 1.2),
                     breaks = seq(0.85, 1.2,
                                  by = 0.05)) +
  ggtitle("Hazard Ratios, AMI Readmission") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))
p3

# Create a Survival Fit object
sf1 <- survfit(Surv(days2mi2) ~ income.cat +
                 bill,
               data = dt2)
sf1

# Extract data from the fit 
surv1 <- data.table(Time = sf1$time,
                    HR = sf1$surv,
                    Group = rep(names(sf1$strata),
                                sf1$strata))
surv1

# Plot
p3.1 <- ggplot(surv1,
             aes(x = Time,
                 y = HR,
                 colour = Group,
                 group = Group)) +
  geom_step() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous("Years",
                     breaks = c(0:17)*365.25,
                     labels = 0:17) +
  scale_y_continuous("Survival",
                     limits = c(0, 1)) +
  ggtitle("AMI Readmission") +
  guides(fill = guide_legend(title = "Group",
                             title.position = "top",
                             nrow = 1))
p3.1

# b. All-cause death----
m4 <- coxph(Surv(days2death) ~ income.cat +
              bill +
              dschyear +
              SEX + 
              RACE + 
              HISPAN +
              dec +
              PRIME,
            data = dt2)
m4
s4 <- summary(m4)
coeff <- s4$coefficients
res4 <- data.table(nn = rownames(coeff),
                   round(s4$conf.int[, -2], 3),
                   pval = round(coeff[, 5], 3))
names(res4)[2:4] <- c("est", "lb", "ub")
res4

# Plot HRs----
res4.plot <- res4[-c(3, 6, 8, 11),]
res4.plot$nn <- factor(res4.plot$nn,
                       levels = res4.plot$nn,
                       labels = c("Income > $50k",
                                  "Bill > $40k",
                                  "Male",
                                  "Black",
                                  "Hispanic",
                                  "Age (x10)",
                                  "Commercial Insurance"))

p4 <- ggplot(res4.plot, 
             aes(x = nn, 
                 y = est)) + 
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lb, 
                    ymax = ub), 
                width = .1) +
  geom_hline(yintercept = 1,
             linetype = "dashed") +
  scale_x_discrete("") +
  scale_y_continuous("Hazard Ratio",
                     limits = c(0.85, 1.2),
                     breaks = seq(0.85, 1.2,
                                  by = 0.05)) +
  ggtitle("Hazard Ratios, All-Cause Death") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        plot.title = element_text(hjust = 0.5))
p4

# Create a Survival Fit object
sf2 <- survfit(Surv(days2death) ~ income.cat +
                 bill,
               data = dt2)
sf2

# Extract data from the fit 
surv2 <- data.table(Time = sf2$time,
                    HR = sf2$surv,
                    Group = rep(names(sf2$strata),
                                sf2$strata))
surv2

# Plot
p4.1 <- ggplot(surv2,
             aes(x = Time,
                 y = HR,
                 colour = Group,
                 group = Group)) +
  geom_step() +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous("Years",
                     breaks = c(0:17)*365.25,
                     labels = 0:17) +
  scale_y_continuous("Survival",
                     limits = c(0, 1)) +
  ggtitle("Survival") +
  guides(fill = guide_legend(title = "Group",
                             title.position = "top",
                             nrow = 1))
p4.1

# Combine all 4 plots
tiff(filename = "tmp/abstract_plot1_06112017.tiff",
     height = 8,
     width = 8,
     units = 'in',
     res = 300,
     compression = "lzw+p")
grid.arrange(p1, p2, p3, p4)
graphics.off()