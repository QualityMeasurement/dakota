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
dt1$NEWDTD[!dt1$cvdead] <- as.Date("2016-01-01")

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

dt1$days2both <- as.numeric(as.character(min( difftime(dt1$post.ami.dx1.dat,
                                                 dt1$DSCHDAT,
                                                 units = "days"), difftime(dt1$NEWDTD,
                                                                         dt1$DSCHDAT,
                                                                         units = "days") )))
}
}

hist(dt1$days2mi2, 100)
summary(dt1$days2mi2)

dt1$cvd30 <- dt1$days2cvdeath<31
dt1$cvd30<-as.numeric(dt1$cvd30)
dt1$cvd30

m3 <- glm(cvd30 ~ `Asprin Discharge (%)` +
  Teach +
  hhyp +
  hcld +
  hckd +
  hcopd +
  RACE + 
  HISPAN +
  AGE +
  PRIME, family= binomial (link="logit"),
  data = dt1)
m3
summary(m3)

dt1$cvd90 <- dt1$days2cvdeath<91
dt1$cvd90<-as.numeric(dt1$cvd90)
dt1$cvd90

m4 <- glm(cvd90 ~ `Asprin Discharge (%)` +
            Teach +
            hhyp +
            hcld +
            hckd +
            hcopd +
            dschyear+
            RACE + 
            HISPAN +
            AGE +
            PRIME, family= binomial (link="logit"),
          data = dt1)
m4
summary(m4)

dt1$cvd180 <- dt1$days2cvdeath<181
dt1$cvd180<-as.numeric(dt1$cvd180)
dt1$cvd180

m5 <- glm(cvd180 ~ `Asprin Discharge (%)` +
            Teach +
            hhyp +
            hcld +
            hckd +
            hdiab + 
            hcopd +
            dschyear+
            RACE + 
            HISPAN +
            AGE +
            PRIME, family= binomial (link="logit"),
          data = dt1)
m5
summary(m5)

dt1$cvd1y <- dt1$days2cvdeath<366
dt1$cvd1y <-as.numeric(dt1$cvd1y)
dt1$cvd1y

m6 <- glm(cvd1y ~ `Asprin Discharge (%)` +
            Teach +
            hhyp +
            hcld +
            hckd +
            hdiab + 
            hcopd +
            dschyear+
            RACE + 
            HISPAN +
            AGE +
            PRIME, family= binomial (link="logit"),
          data = dt1)
m6
summary(m6)