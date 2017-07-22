
# Project: Dakota      
# Author: Georgia Barbayannis; Davit Sargsyan; I-ming Chiu; Noah Michel
# Created:  05/12/2017
#**********************************************************
# Header----
# Left computer
DATA_HOME <- "C:/Users/ds752/Documents/git_local/data/dakota"
# Center computer
DATA_HOME <- "C:/git_local/data/dakota"
require(data.table)
require(survival)
require(ggplot2)
require(gridExtra)

#**********************************************************
# PART I: Data----
## a. MIDAS data----
# Load data
load(file.path(DATA_HOME, "case_05122017.RData"))
case$ZIP <- as.numeric(substr(case$ZIP, 1, 5))
length(unique(case$ZIP))

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


## d. Merge all 3 datasets
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
# Outcomes----
# a. All-cause death----
dt1$dead <- FALSE
dt1$dead[!is.na(dt1$NEWDTD)] <- TRUE
addmargins(table(dt1$dead))

# Censor the data: if patients did not die until 01/01/2015, censor
dt1$NEWDTD[!dt1$dead] <- as.Date("2015-01-01")

# Days to death
dt1$days2death <- as.numeric(as.character(difftime(dt1$NEWDTD,
                                                   dt1$DSCHDAT,
                                                   units = "days")))
hist(dt1$days2death, 100)
summary(dt1$days2death)

# b. AMI readmission----
dt1$mi2.dx1 <- FALSE
dt1$mi2.dx1[is.finite(dt1$post.ami.dat)] <- TRUE
addmargins(table(dt1$mi2.dx1))

# Censor the data: if patients did not have second MI until 01/01/2015, censor
dt1$post.ami.dat[!dt1$mi2.dx1] <- as.Date("2015-01-01")

# Days to MI readmisson
dt1$days2mi2 <- as.numeric(as.character(difftime(dt1$post.ami.dat,
                                                   dt1$DSCHDAT,
                                                   units = "days")))
hist(dt1$days2mi2, 100)
summary(dt1$days2mi2)

#**********************************************************
# Risk factors----
# Age by decades
dt1$dec <- round(dt1$AGE/10)
hist(dt1$dec)

# Hispanic
dt1$HISPAN <- factor(as.character(dt1$HISPAN),
                     levels =   c("Non-hispanic",
                                  "Hispanic", 
                                  "Unknown"  ))
dt1[,rnk:= frank(`Overall Score (%)`),
    by=c("HOSP",
         "DIV", 
         "dschyear")]
dt1$rnk
#**********************************************************
#a<-is.na(z1)
#a<-as.numeric(a)
#y<-c(1:204903)
#a<-a*y
#z1[a]<-31
#z1[1:100]

#a<-z1>31
#a<-as.numeric(a)
#a
#y<-c(1:204903)
#a<-a*y
#a[1:100]
#z1[1:100]
#z1[a]<-31
#z1[1:100]

q1<-[TIME]

z1<-difftime(dt1$NEWDTD,
             dt1$DSCHDAT, 
             units = "days")

a<-is.na(z1)
a<-as.numeric(a)
y<-c(1:119866)
a<-a*y

z2<-difftime(dt1$NEWDTD,
             dt1$DSCHDAT, 
             units = "days")

b<-is.na(z2)
b<-as.numeric(b)
b<-b*y
z2[b]<-q1

b<-z2>=q1
b<-as.numeric(b)
b<-b*y
z2[b]<-q1

b<-z2<q1
b<-as.numeric(b)
b[a]<-0
dt3<-cbind(dt1, z2, b)
#**********************************************************
# PART II: Models----
summary(dt1)

# a. AMI readmission within 1 year of first MI----
m1 <- glm(post.ami.dx1.1y ~ `Asprin Discharge (%)` +
            `Beta Blocker Discharge (%)` +
            `ACEI/ARB Discharge (%)` +
            AGE +
            RACE +
            HISPAN +
            SEX +
            PRIME +
            hhyp +
            hdiab +
            hckd +
            hcopd +
            hlipid +
            dschyear,
          family = binomial(logit),
          data = dt2)
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
m3 <- coxph(Surv(days2mi2) ~ income.cat +
              bill +
              dschyear +
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

#New plot from Noah
#m3 <- glm(as.numeric(z2),b) ~ `Asprin Discharge (%)` +
             # `Beta Blocker Discharge (%)` +
             # `ACEI/ARB Discharge (%)` +
             # dschyear +
             # SEX + 
             # RACE + 
             # HISPAN +
             # dec +
              #PRIME,
            #data = dt3)
#m3

#The first subscore model, eventaully dropping Beta and ACEI bc they are less significant
m3 <- glm(b ~ `Asprin Discharge (%)` +
`Beta Blocker Discharge (%)` +
 `ACEI/ARB Discharge (%)` +
dschyear +
SEX + 
RACE + 
HISPAN +
dec +
PRIME,
data = dt3)
m3
s3 <- summary(m3)
#My most recent attempt at a good subscore model, with the factors that Kostis mentioned
m3 <- glm(b ~ `Asprin Discharge (%)` +
            Teach +
            hhyp +
            hcld +
            hckd +
            hcopd +
            dschyear +
            SEX + 
            RACE + 
            HISPAN +
            dec +
            PRIME,
          data = dt3)
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