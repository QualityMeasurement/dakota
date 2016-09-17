require(data.table)
require(survival)
require(ggplot2)
# require(gtable)
# require(grid)
require(gridExtra)

# Scores
scores.mi <- fread("data\\NJ HOSPITALS_Heart Attack Scores.csv")
scores.hf <- fread("data\\NJ HOSPITALS_Heart Attack Scores.csv")

scores.mi <- droplevels(subset(scores.mi, 
                               subset = !is.na(scores.mi$`Report Year`) &
                                 scores.mi$`Hospital Number` != 999 &
                                 scores.mi$Secondary == 0, 
                               select = c(1:3, 8, 13:14)))

scores.hf <- droplevels(subset(scores.hf, 
                               subset = !is.na(scores.hf$`Report Year`) &
                                 scores.hf$`Hospital Number` != 999 &
                                 scores.hf$Secondary == 0, 
                               select = c(1:3, 8, 13:14)))

names(scores.mi) <- names(scores.hf) <- c("year", "hosp", "hn", "beds", "teach", "overall")

# MIs
scores <- scores.mi
scores$beds <- as.numeric(scores$beds)
scores$teach <- factor(scores$teach)
scores$overall <- as.numeric(scores$overall)
scores$hn <- as.numeric(scores$hn)
scores$year <- as.numeric(scores$year)
scores
summary(scores)

length(unique(scores$hosp))
length(unique(scores$hn))
uhs <- unique(scores$hn)[order(unique(scores$hn))]
uhs

# # NOTE: REMOVE THIS PART WHEN HOSP DIVISIONS ARE ADDED TO MD DATASET!
# scores[, mu := mean(overall, na.rm = TRUE),
#        by = c("year", "hn")]
# scores[, mu.smoke := mean(smoke, na.rm = TRUE),
#        by = c("year", "hn")]
# scores <- unique(scores[, -c(2, 4:5), with = FALSE])
# scores

# Plot scores, four hospitals at the time
# Source: http://stackoverflow.com/questions/18394391/r-custom-legend-for-multiple-layer-ggplot
# j <- uhs[1:4]
# ggplot(subset(scores,
#               hn %in% j)) + 
#   facet_wrap(~ hn) +
#   geom_line(aes(x = year, 
#                 y = overall,
#                 color = "black")) +
#   geom_point(aes(x = year, 
#                  y = overall,
#                  color = "black"),
#              size = 3) +
#   geom_line(aes(x = year + 0.2, 
#                 y = smoke,
#                 color = "red")) +
#   geom_point(aes(x = year + 0.2, 
#                  y = smoke,
#                  color = "red"),
#              size = 3) +
#   scale_colour_manual(name = "Scores:", 
#                       values =c("black" = "black",
#                                 "red" = "red"),
#                       labels = c("Overall",
#                                  "Smoking")) +
#   theme(legend.position = "top")

# DS 08/19/2016: New data from Joel located on his scomputer in:
# C:/SAS/QC Data
# Contains records from March 1985 to the end of
# md <- fread("data/miqcdatafullcomorb.csv")
# save(md, file = "data/md.R")
load("data/md.R")

# Format data
md$FIRSTMIDATE <- as.Date(md$FIRSTMIDATE, "%m/%d/%Y")
md$FIRSTMIDSCDATE <- as.Date(md$FIRSTMIDSCDATE, "%m/%d/%Y")
md$SECONDMIDATE <- as.Date(md$SECONDMIDATE, "%m/%d/%Y")
md$SECONDMIDSCDATE <- as.Date(md$SECONDMIDATE, "%m/%d/%Y")
md$THIRDMIDATE <- as.Date(md$THIRDMIDATE, "%m/%d/%Y")
md$THIRDMIDSCDATE <- as.Date(md$THIRDMIDSCDATE, "%m/%d/%Y")

range(md$FIRSTMIDATE, na.rm = TRUE)
range(md$FIRSTMIDSCDATE, na.rm = TRUE)
range(md$SECONDMIDATE, na.rm = TRUE)
range(md$SECONDMIDSCDATE, na.rm = TRUE)
range(md$THIRDMIDATE, na.rm = TRUE)
range(md$THIRDMIDSCDATE, na.rm = TRUE)

# Birthdate
md$PATBDTE <- as.Date(md$PATBDTE, origin = "1960-01-01")
range(md$PATBDTE, na.rm = TRUE)

# Age at 1st MI
md$FIRSTMIAGE <- as.numeric(as.character(floor(difftime(md$FIRSTMIDATE, md$PATBDTE, units = "days")/365.25)))
hist(md$FIRSTMIAGE)
range(md$FIRSTMIAGE, na.rm = TRUE)
# Remove anybody younger than 20
md <- subset(md, FIRSTMIAGE >= 20)
hist(md$FIRSTMIAGE)

# Death
md$NEWDTD <- as.Date(md$NEWDTD, origin = "1960-01-01")
sum(!is.na(md$NEWDTD))
range(md$NEWDTD, na.rm = TRUE)
md$DEATHAGE <- as.numeric(as.character(floor(difftime(md$NEWDTD, md$PATBDTE, units = "days")/365.25)))
hist(md$DEATHAGE)

# Hospital number
md$FIRSTMIHOSP <- as.numeric(as.character(md$FIRSTMIHOSP))
md$SECONDMIHOSP <- as.numeric(as.character(md$SECONDMIHOSP))
md$THIRDMIHOSP <- as.numeric(as.character(md$THIRDMIHOSP))

#############################################################
maxdat <- as.Date("2015-01-01")
# CHECKPOINT
sum(is.na(md$FIRSTMIDATE))
nsubj <- length(unique(md$PATIENT_ID))
nsubj 
sum(is.na(md$SECONDMIDATE))
nmi2 <- sum(!is.na(md$SECONDMIDATE))
nmi2
ndeath <- sum(!is.na(md$NEWDTD))
ndeath

# Censor
md$SECONDMIDATE[is.na(md$SECONDMIDATE) | 
                     md$SECONDMIDATE > maxdat] <- maxdat
ndx.dead <- which(!is.na(md$NEWDTD) & 
                    md$NEWDTD < md$SECONDMIDATE &
                    md$NEWDTD > md$FIRSTMIDATE &
                    md$NEWDTD < maxdat) 
md$SECONDMIDATE[ndx.dead] <- md$NEWDTD[ndx.dead] 

dd <- data.table(days2mi2 = as.numeric(difftime(md$SECONDMIDATE,
                                                md$FIRSTMIDATE,
                                                units = "days")),
                 m2date = md$SECONDMIDATE,
                 year = as.numeric(format(md$FIRSTMIDATE,"%Y")),
                 age = md$FIRSTMIAGE,
                 sex = md$SEX,
                 hn = md$FIRSTMIHOSP)

# Days to 2nd MI
sum(is.na(dd$days2mi2))
summary(dd$days2mi2)

t1 <- table(dd$days2mi2)
hist(t1)
ndx <- 1:14
addmargins(t1[ndx])
xloc <- barplot(t1[ndx],
                col = heat.colors(14),
                main = paste(nsubj,
                             "Admissions between",
                             min(md$FIRSTMIDATE),
                             "and",
                             maxdat,
                             "\n With", 
                             nmi2, 
                             "Readmissions and",
                             ndeath,
                             "Deaths"),
                ylab = "Number of MI Readmissions and Deaths",
                xlab = "Days to MI Readmission or Death")
text(x = xloc,
     y = t1[ndx]/2, 
     labels = t1[ndx], 
     srt = 90)
legend("topright", 
       legend = c(paste(sum(t1[ndx]),
                        "Readmissions and Deaths"),
                  "Within 2 Weeks of 1st MI Admission"))

############################################################
t2 <- table(dd$days2mi2, dd$year)
t3 <- data.table(t2[1:14, c(20:ncol(t2))])
t3$Day <- 0:13
t3 <- melt(t3, 
           id.vars = "Day",
           variable.name = "Year",
           value.name = "Readmissions")
t3
number_ticks <- function(n) {function(limits) pretty(limits, n)}
ggplot(t3,
       aes(x = Day,
           y = Readmissions,
           fill = Year)) +
  geom_bar(position = "stack",
           stat="identity") +
  scale_x_continuous(name = "Days From First MI",
                     breaks = 0:13) +
  ggtitle("MI Readmissons by Year of First MI") 

# Matching with 'Diagnoses in Timing of 30-Day Readmissions 
# After Hospitalization for Heart Failure, Acute Myocardial Infarction, 
# or Phneumonia', Dharmarajan et al 2013

# 1. Years: 2007 to 2009 (included)
# 2. Medicare Fee-for-service Claims data
# 3. Time to readmission from first MI
# 4. Ages at first MI: 
# 65-74 | 75-84 | >=85
# 34.0  | 33.8  | 35.6

tmp <- droplevels(subset(dd,age > 64 &
                           year > 2006 &
                           year < 2010))
dd$agegrp <- "65 to 74"
dd$agegrp[dd$age > 74] <- "75 to 84"
dd$agegrp[dd$age > 84] <- ">=85"
dd$agegrp <- factor(dd$agegrp, levels = c("65 to 74", "75 to 84", ">=85"))

dd$readm30 <- 0
dd$readm30[dd$days2mi2 <31 & dd$days2mi2 < maxdat] <- 1
tt <- table(dd$agegrp, dd$readm30)
tt
tt <- data.table(Age = levels(dd$agegrp),
                 Dharmarajan = c(34.0, 33.8, 35.6),
                 MIDAS = round(100*tt[, 2]/tt[, 1], 1))
tt <- melt.data.table(tt, id.vars = "Age", 
                      variable.name = "Source",
                      value.name = "% Readmitted")
tt$Age <- factor(tt$Age, levels = levels(dd$agegrp))
p1 <- ggplot(tt,
             aes(x = Age,
                 y = `% Readmitted`,
                 fill = Source)) +
  geom_bar(position = position_dodge(),
           stat="identity") +
  scale_x_discrete(name = "Age At First MI") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 0,
                                   hjust = 1)) +
  ggtitle("Comparison of 30 Day Readmissions") 

# By gender
tt <- table(dd$sex, dd$readm30)
tt
tt <- data.table(Sex = c("Female", "Male"),
                 Dharmarajan = c(34.0, 34.8),
                 MIDAS = round(100*tt[1:2, 2]/tt[1:2, 1], 1))
tt <- melt.data.table(tt, id.vars = "Sex", 
                      variable.name = "Source",
                      value.name = "% Readmitted")

p2 <- ggplot(tt,
             aes(x = Sex,
                 y = `% Readmitted`,
                 fill = Source)) +
  geom_bar(position = position_dodge(),
           stat="identity") +
  scale_x_discrete(name = "Sex") +
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 0,
                                   hjust = 1)) +
  ggtitle("Comparison of 30 Day Readmissions") 

gridExtra::grid.arrange(p1, p2, nrow = 1)

# NOTE: FIX THIS LATER!
# Remove records with same date for 1st and 2nd MI
#dd <- droplevels(subset(dd, days2mi2 > 7))

summary(dd$days2mi2)
hist(dd$days2mi2, 10000)
barplot(t1[-c(1:15)])

# Second MI indicator
dd$m2 <- FALSE
dd$m2[dd$m2date < maxdat] <- TRUE
summary(dd$m2)

# Merge with scores
dd <- merge(dd, scores, by = c("hn", "year"))
dd$year <- factor(dd$year)

# Overall model: all years against 2004
dd$years.from.2004 <- as.numeric(as.character(dd$year)) - 2004
m1 <- coxph(Surv(days2mi2, m2) ~ years.from.2004 + overall, data = dd)
m1
summary(m1)
plot(survfit(m1),
     mark.time = FALSE,
     col = "blue",
     lty = 1,
     lw = 2,
     main = "MI Readmissons or Deaths After 1st MI \n
             Adjusted for Years-from-2004 and Overall Score",
     xlab = "Days from 1st MI",
     ylab = "Propostion Without Events")

m2 <- coxph(Surv(days2mi2, m2) ~ year, data = dd)
m2
summary(m2)
m2 <- survfit(Surv(days2mi2, m2) ~ year, data = dd)
plot(m2,
     mark.time = FALSE,
     col = rainbow(nlevels(dd$year)),
     lty = 1,
     lw = 2,
     main = "MI Readmissons or Deaths After 1st MI \n
     Adjusted for the Year of 1st MI",
     xlab = "Days from 1st MI",
     ylab = "Propostion Without Events")
legend("bottomleft", 
       legend = levels(dd$year),
       lty = 1,
       col = rainbow(nlevels(dd$year)),
       ncol = 2)

##############################################################
# Score range, per hospital
dd[, score.range := diff(range(overall, na.rm = TRUE)), by = hn]

plot(dd$overall ~ dd$score.range,
     xlab = "Score Range",
     ylab = "Overall Score")

m3 <- coxph(Surv(days2mi2, m2) ~ as.numeric(year) + age + sex + teach + score.range + overall, data = dd)
m3
summary(m3)

##############################################################
# Compare only 2001 and 2003
tmp <- droplevels(subset(dd, year %in% c(2005, 2007)))
m2 <- coxph(Surv(days2mi2, m2) ~ overall + year, data = tmp)
m2
summary(m2)

m21 <- survfit(Surv(days2mi2, m2) ~ year, data = tmp)
plot(m21, 
     mark.time = FALSE,
     conf.int = TRUE,
     col = c("red", "blue"))

legend("topright", 
       legend = levels(tmp$year),
       lty = 1,
       col = c("red", "blue"))

###############################################
# Loop through years
pp <- list()
for(j in 1:4) {
  out <- list()
  dlta <- j
  for(i in 1:(nlevels(dd$year) - dlta)) {
    tmp <- droplevels(subset(dd, year %in% c(levels(dd$year)[i],
                                             levels(dd$year)[i + dlta])))
    m1 <- summary(coxph(Surv(days2mi2, m2) ~ year, data = tmp))
    out[[i]] <- c(as.numeric(levels(dd$year)[i + dlta]),
                  m1$conf.int[1, -2], 
                  m1$coef[1, 5])
  }
  out <- do.call("rbind", out)
  colnames(out) <- c("Year", "HR", "LL95", "UL95", "p-Val")
  out <- data.frame(out)
  out
  
  pp[[j]] <- ggplot(out) +
    geom_point(aes(x = Year,
                   y = HR,
                   color = "red"),
               size = 3) +
    geom_hline(yintercept = 1,
               linetype = "dashed") +
    ggtitle(paste("HR of Current and -", j, "Years")) +
    theme(legend.position = "none") + 
    geom_errorbar(aes(x = Year,
                      ymin = LL95,
                      ymax = UL95),
                  colour = "black",
                  width = .1)
}
grid.arrange(pp[[1]],
             pp[[2]],
             pp[[3]],
             pp[[4]])

scores[, mu := mean(overall, na.rm = TRUE),
       by = year]
scores[, sd := sd(overall, na.rm = TRUE),
       by = year]
scr <- unique(subset(scores, select = c(1, 7, 8)))
scr$lb <- scr$mu - qt(0.975, nrow(scr) - 1)*scr$sd/sqrt(nrow(scr))
scr$ub <- scr$mu + qt(0.975, nrow(scr) - 1)*scr$sd/sqrt(nrow(scr))
names(scr)[1:2] <- c("Year", "Score")
scr

ggplot(scr) +
  geom_line(aes(x = Year,
                 y = Score,
                 color = "blue")) +
  geom_point(aes(x =Year,
                 y = Score,
                 color = "blue"),
             size = 3) +
  ggtitle("Average Overall Scores with 95% C.I.") +
  theme(legend.position = "none") + 
  geom_errorbar(aes(x = Year,
                    ymin = lb,
                    ymax = ub),
                color = "black",
                width = .1)

###############################################
###############################################
###############################################
# hn=1
# y1=2001
# y2=2003
hospfun <- function(md, hn, y1, y2){
  # CHECK WITH JAVIER!
  maxdat2 <- "2015-01-01"
  
  md.i <- md[md$FIRSTMIHOSP == hn, ]
  md.i$dt1disc <- md.i$FIRSTMIDSCDATE
  md.i$dtbirth <- md.i$PATBDTE
  md.i$age <- md.i$FIRSTMIAGE
  md.i$dtdeath <- md.i$NEWDTD
  md.i$agedeath <- md.i$DEATHAGE
  md.i$dt2mi <- md.i$SECONDMIDSCDATE
  md.i$censored <- (!is.na(md.i$dtdeath) & 
                      (md.i$dtdeath >= md.i$dt1disc)) & 
    is.na(md.i$dt2mi)
  
  # Death year
  dy.i = substring(md.i$dt1disc, 1, 4)
  dy.i.a = dy.i == as.character(y1)
  dy.i.a[is.na(dy.i.a)] = F
  md.i.a = md.i[dy.i.a,]
  sdd.i = md.i.a$dt2mi
  i1 = is.na(sdd.i)
  sdd.i[i1] = maxdat2
  md.i.a$smi = sdd.i
  md.i.a$smidd = as.numeric(md.i.a$smi-md.i.a$dt1disc) 
  
  md.i.b = md[md$FIRSTMIHOSP == hn,]
  dd.i <- md.i.b$FIRSTMIDSCDATE
  bdt = md.i.b$PATBDTE
  md.i.b$age <- md.i.b$FIRSTMIAGE
  dy.i = substring(dd.i,1,4)
  dy.i.b = dy.i == as.character(y2)
  dy.i.b[is.na(dy.i.b)] = F
  md.i.b$dd = dd.i
  md.i.b = md.i.b[dy.i.b,]
  sdd.i = md.i.b$SECONDMIDSCDATE
  i2 = is.na(sdd.i)
  sdd.i[i2] = maxdat2
  #correct for death 
  md.i.b$smi = sdd.i
  md.i.b$smidd = as.numeric(md.i.b$smi-md.i.b$dd) 
  
  # # CHECKPOINT:
  # boxplot(md.i.a$smidd[!is.na(md.i.a$smidd)],
  #         md.i.b$smidd[!is.na(md.i.b$smidd)],
  #         names = as.character(c(y1,y2)),
  #         sub = hn)
  # t.test(md.i.a$smidd[!is.na(md.i.a$smidd)],
  #        md.i.b$smidd[!is.na(md.i.b$smidd)])
  
  data = data.frame(status = 1-c(i1,i2),
                    rbind(cbind(age = md.i.a$age,
                                year = y1,
                                sdmi = md.i.a$smidd),
                          cbind(age = md.i.b$age,
                                year = y2,
                                sdmi = md.i.b$smidd)))
  a = summary(aa <- coxph(Surv(sdmi, status) ~ age+ factor(year), data=data))
  c(a$coef[2,], a$conf.int[2,])
}

# Hospitals
hosp <- unique(c(md$FIRSTMIHOSP, 
                 md$SECONDMIHOSP,
                 md$THIRDMIHOSP))
hosp <- hosp[!is.na(hosp)]
hosp <- hosp[order(hosp)]
hosp

res <- list()
for (j in 1:length(hosp)) {
  out <- list()
  for(i in 2003:2014) {
    try({
      a <- hospfun(md, j, i-2, i)
      out[[i]] <- c(hn = j, year = i, a)
    })
  }
  res[[j]] <- do.call("rbind", out)
}
hr <- do.call("rbind", res)
hr <- data.table(hr)

# Remove bad fits
hr <- hr[is.finite(hr$`upper .95`),]
hrr <- subset(hr, select = c(1:2, 8, 10:11))
hrr <- merge(hrr, scores, by = c("hn", "year"), all = TRUE)
names(hrr)[3:5] <- c("hr", "lb", "ub")
hrr

# REMOVE ANYTHING BEFORE 2003
hrr <- droplevels(subset(hrr, year > 2002))

# Merge hr and scores
setkeyv(scores, c("hn", "year"))
setkeyv(hrr, c("hn", "year"))

scores$year <- as.numeric(scores$year)
scores$hn <- as.numeric(scores$hn)

d.all <- merge(hrr, scores, c("hn", "year"))
tmp <- droplevels(subset(d.all, hn == 1))
plot(tmp$hr ~ tmp$year)

# #############################################################
# # PLOT
# Rescale function
# Source: http://stackoverflow.com/questions/6142944/how-can-i-plot-with-2-different-y-axes
rescale <- function(vec, lim1 = range(vec), lim2 = c(0, 1)) {
  # find the coeficients of transforming linear equation
  # that maps the lims range to (0, 1)
  slope <- diff(lim2) / diff(lim1)
  intercept <- lim2[1] - slope * lim1[1]
  
  xformed <- slope * vec + intercept
  
  xformed
}

length(hosp)
jj <- matrix(hosp,
             nrow = 6)
# for (i in 1:ncol(jj)) {
for (i in hosp) {
  out <- try({
    # j <- hosp[jj[, i]]
    # tmp <- subset(hrr, hn %in% j)
    tmp <- subset(hrr, hn == i)
    
    # Rescale
    tmp$hr.scaled <- rescale(tmp$hr,
                             range(c(tmp$lb, 
                                     tmp$ub), 
                                   na.rm = TRUE),
                             range(c(tmp$overall, 
                                     tmp$smoke), 
                                   na.rm = TRUE))
    tmp$lb.scaled <- rescale(tmp$lb,
                             range(c(tmp$lb, 
                                     tmp$ub), 
                                   na.rm = TRUE),
                             range(c(tmp$overall, 
                                     tmp$smoke), 
                                   na.rm = TRUE))
    tmp$ub.scaled <- rescale(tmp$ub,
                             range(c(tmp$lb, 
                                     tmp$ub), 
                                   na.rm = TRUE),
                             range(c(tmp$overall, 
                                     tmp$smoke), 
                                   na.rm = TRUE))
    hr.1 <- rescale(1,
                    range(c(tmp$lb, 
                            tmp$ub), 
                          na.rm = TRUE),
                    range(c(tmp$overall, 
                            tmp$smoke), 
                          na.rm = TRUE))
    p1 <- ggplot(tmp) +
      # facet_wrap(~ hn,
      #            ncol = 3) +
      geom_line(aes(x = year,
                    y = overall,
                    color = "blue")) +
      geom_point(aes(x = year,
                     y = overall,
                     color = "blue"),
                 size = 3) +
      geom_line(aes(x = year,
                    y = smoke,
                    color = "green")) +
      geom_point(aes(x = year,
                     y = smoke,
                     color = "green"),
                 size = 3) +
      geom_line(aes(x = year,
                    y = hr.scaled,
                    color = "red")) +
      geom_point(aes(x = year,
                     y = hr.scaled,
                     color = "red"),
                 size = 3) +
      geom_hline(yintercept = hr.1,
                 linetype = "dashed") +
      ggtitle(unique(tmp$hosp[!is.na(tmp$hosp)])[1]) +
      annotate("text",
               x = 2010,
               y = hr.1,
               label = "HR = 1") +
      scale_colour_manual(name = "Scores:",
                          values = c("blue",
                                     "green",
                                     "red"),
                          labels = c("Overall",
                                     "Smoking",
                                     "MI HR")) +
      theme(legend.position = "top") + 
      geom_errorbar(aes(x = year,
                        ymin = lb.scaled,
                        ymax = ub.scaled),
                    colour = "black",
                    width = .1)
    # # Second y-axis: FIND A WAY!
    # p2 <- ggplot(data.frame(y = c(tmp$lb, tmp$ub),
    #                         x = rep(0, 2*nrow(tmp))),
    #              aes(x, y)) +
    #   geom_blank() +
    #   theme(axis.line.x=element_blank(),
    #         axis.text.x=element_blank(),
    #         axis.ticks.x=element_blank(),
    #         axis.title.x=element_blank(),
    #         panel.grid.minor.x=element_blank(),
    #         panel.grid.major.x=element_blank())
    #   
    # grid.arrange(p1, p2,cols=2)
  })
  if(class(out)[1] != "try-error") {
    try({
      png(paste("tmp/plot",
                i,
                ".png"))
      print(p1)
    })
  }
  graphics.off()
}

# Averages
tmp <- copy(hrr)
setkey(tmp, year)
tmp[, mu.hr := mean(hr, na.rm = TRUE), by = year]
tmp[, mu.overall := mean(overall, na.rm = TRUE), by = year]
tmp[, mu.smoke := mean(smoke, na.rm = TRUE), by = year]
tmp <- unique(subset(tmp, select = c(2, 9:11)))

range(tmp$mu.hr)


tmp$mu.hr.scaled <- rescale(tmp$mu.hr,
                            range(tmp$mu.hr, 
                                  na.rm = TRUE),
                            range(c(tmp$mu.overall, 
                                    tmp$mu.smoke), 
                                  na.rm = TRUE))
hr.1 <- rescale(1,
                range(tmp$mu.hr, 
                      na.rm = TRUE),
                range(c(tmp$mu.overall, 
                        tmp$mu.smoke), 
                      na.rm = TRUE))
p2 <- ggplot(tmp) +
  geom_line(aes(x = year,
                y = mu.overall,
                color = "blue")) +
  geom_point(aes(x = year,
                 y = mu.overall,
                 color = "blue"),
             size = 3) +
  geom_line(aes(x = year,
                y = mu.smoke,
                color = "green")) +
  geom_point(aes(x = year,
                 y = mu.smoke,
                 color = "green"),
             size = 3) +
  geom_line(aes(x = year,
                y = mu.hr.scaled,
                color = "red")) +
  geom_point(aes(x = year,
                 y = mu.hr.scaled,
                 color = "red"),
             size = 3) +
  geom_hline(yintercept = hr.1,
             linetype = "dashed") +
  ggtitle("Averages of All Hospitals") +
  annotate("text",
           x = 2010,
           y = hr.1,
           label = "HR = 1") +
  scale_colour_manual(name = "Scores:",
                      values = c("blue",
                                 "green",
                                 "red"),
                      labels = c("Overall",
                                 "Smoking",
                                 "MI HR")) +
  theme(legend.position = "top")
p2
# ToDo 08/19/2016
# ADD:
#   
# 2nd y-axis if possible
# x and y axis labels
# multiple panels with free scale