# Project: Dakota      
# Author: Georgia Barbayannis; Davit Sargsyan; I-ming Chiu; Noah Michel
# Created:  05/12/2017
#**********************************************************
# PART I----
DATA_HOME <- "C:/Users/ds752/Documents/git_local/data/dakota"
require(data.table)
require(survival)
require(ggplot2)

# Load data----
load(file.path(DATA_HOME, "case_05052017.RData"))

length(unique(as.numeric(case$ZIP)))
summary(as.numeric(case$ZIP))

# Myocardial Infarction (MI) scores----
scores.mi <- fread(file.path(DATA_HOME, "NJ HOSPITALS_Heart Attack Scores.csv"))
scores.mi <- droplevels(subset(scores.mi, 
                               subset = !is.na(scores.mi$`Report Year`) &
                                 scores.mi$`Hospital Number` != 999 &
                                 scores.mi$Secondary == 0, 
                               select = c(1:3, 8, 13:21)))

# Convert variables----
scores.mi$`Report Year` <- factor(scores.mi$`Report Year`)
scores.mi$`Hospital Number` <- factor(scores.mi$`Hospital Number`)
scores.mi$Teach <- factor(scores.mi$Teach)
scores.mi$BEDS <- as.numeric(as.character(scores.mi$BEDS))

scores.mi$`Overall Score (%)` <- as.numeric(as.character(scores.mi$`Overall Score (%)`))
scores.mi$`Aspirin Arrival (%)` <- as.numeric(as.character(scores.mi$`Aspirin Arrival (%)`))
scores.mi$`Asprin Discharge (%)` <- as.numeric(as.character(scores.mi$`Asprin Discharge (%)`))
scores.mi$`Beta Blocker Arrival (%)` <- as.numeric(as.character(scores.mi$`Beta Blocker Arrival (%)`))
scores.mi$`Beta Blocker Discharge (%)` <- as.numeric(as.character(scores.mi$`Beta Blocker Discharge (%)`))
scores.mi$`ACEI/ARB Discharge (%)` <- as.numeric(as.character(scores.mi$`ACEI/ARB Discharge (%)`))
scores.mi$`Smoking Cessation Advice (%)` <- as.numeric(as.character(scores.mi$`Smoking Cessation Advice (%)`))
scores.mi$`Statin Prescribed at Discharge (%)` <- as.numeric(as.character(scores.mi$`Statin Prescribed at Discharge (%)`))

summary(scores.mi)

# Table: number of hospitals with scores per year----
scores.mi[, nOveral := sum(!is.na(`Overall Score (%)`)),
          by = `Report Year`]
scores.mi[, nAspArrl := sum(!is.na(`Aspirin Arrival (%)`)),
          by = `Report Year`]
scores.mi[, nAspDsch := sum(!is.na(`Asprin Discharge (%)`)),
          by = `Report Year`]
scores.mi[, nBBArrl := sum(!is.na(`Beta Blocker Arrival (%)`)),
          by = `Report Year`]
scores.mi[, nBBDsch := sum(!is.na(`Beta Blocker Discharge (%)`)),
          by = `Report Year`]
scores.mi[, nACEI := sum(!is.na(`ACEI/ARB Discharge (%)`)),
          by = `Report Year`]
scores.mi[, nSmoke := sum(!is.na(`Smoking Cessation Advice (%)`)),
          by = `Report Year`]
scores.mi[, nStatin := sum(!is.na(`Statin Prescribed at Discharge (%)`)),
          by = `Report Year`]

t1 <- unique(subset(scores.mi,
                    select = c(1, 14:21)))
t1

# Rename column to merge with MIDAS----
scores <- scores.mi
names(scores)[1:6] <- c("dschyear", 
                        "hosp.name", 
                        "HOSP",
                        "beds", 
                        "teach",
                        "overall")
scores$beds <- as.numeric(scores$beds)
scores$teach <- factor(scores$teach)
scores$overall <- as.numeric(scores$overall)
scores$HOSP <- as.numeric(scores$HOSP)
scores$dschyear <- as.numeric(as.character(scores$dschyear))

# Remove OVERALL = NA records
scores <- droplevels(subset(scores, !is.na(overall)))

# Score range for each hospital, from 2004 to current year
scores[, delta.overall := diff(range(overall, na.rm = TRUE)), by = HOSP]
scores
summary(scores)

length(unique(scores$hosp.name))
length(unique(scores$HOSP))
uhs <- unique(scores$HOSP)[order(unique(scores$HOSP))]
uhs

# Merge MIDAS with MI scores----
dt1 <- merge(case, 
             scores,
             by = c("HOSP", 
                    "dschyear"),
             all.x = TRUE)

# Create a death indicator
dt1$dead <- FALSE
dt1$dead[!is.na(dt1$NEWDTD)] <- TRUE
addmargins(table(dt1$dead))

# Censor the data
dt1$NEWDTD[!dt1$dead] <- as.Date("2016-01-01")

# Days to death
dt1$days2death <- as.numeric(as.character(difftime(dt1$NEWDTD,
                                                   dt1$DSCHDAT,
                                                   units = "days")))
hist(dt1$days2death, 100)

# Model1: survival----
# Subset data to just 2 years----
dt1.00.01 <- droplevels(subset(dt1,
                               dschyear %in% c(2000, 2001)))

# m1 <- coxph(Surv(days2death, dead) ~ dschyear,
#             data = dt1.00.01)
# m1

# Create a Survival Fit object
sf1 <- survfit(Surv(days2death, dead) ~ dschyear, 
               data = dt1.00.01)

# Extract data from the fit 
dt.surv <- data.table(Time = sf1$time,
                      HR = sf1$surv,
                      Group = rep(names(sf1$strata),
                                  sf1$strata))

# Plot
ggplot(dt.surv,
       aes(x = Time,
           y = HR,
           colour = Group,
           group = Group)) +
  geom_step() +
  # scale_colour_manual(values = c("green",
  #                                "black",
  #                                "red",
  #                                "blue")) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous("Years",
                     breaks = c(0:17)*365.25,
                     labels = 0:17) +
  scale_y_continuous("Survival",
                     limits = c(0.5, 1)) +
  ggtitle("Survival Curves") +
  guides(fill = guide_legend(title = "Group",
                             title.position = "top",
                             nrow = 1))

# Compute and plot logit----
dt.surv[, Logit := log(HR/(1 - HR))]

ggplot(dt.surv,
       aes(x = Time,
           y = Logit,
           colour = Group,
           group = Group)) +
  geom_step() +
  scale_colour_manual(values = c("green",
                                 "black",
                                 "red",
                                 "blue")) +
  scale_x_continuous("Years",
                     breaks = c(0:17)*365.25,
                     labels = 0:17) +
  scale_y_continuous("Logit of Survival") +
  ggtitle("Logit") +
  guides(fill = guide_legend(title = "Group",
                             title.position = "top",
                             nrow = 1))

# Model2: logistic
# a. MI eadmission within 1 year of first MI
m2 <- glm(post.ami.dx1.1y ~ SEX,
             family = binomial(logit),
             data = dt1)
summary(m2)

# b. Death within 1 year of forst MI admission
m3 <- glm(dt1$dead.1y ~ SEX,
          family = binomial(logit),
          data = dt1)
summary(m3)
