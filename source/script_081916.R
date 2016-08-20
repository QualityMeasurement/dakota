library(data.table)
library(survival)
require(ggplot2)
require(gtable)
require(grid)

# Scores
scores <- fread("data\\NJ HOSPITALS_Heart Attack Scores.csv")
scores <- droplevels(subset(scores, 
                            subset = !is.na(scores$`Report Year`) & scores$`Hospital Number` != 999, 
                            select = c(1:4, 10)))
names(scores) <- c("year", "hosp", "hn", "overall", "smoke")
scores$overall <- as.numeric(scores$overall)
scores$smoke <- as.numeric(scores$smoke)
scores

length(unique(scores$hosp))
length(unique(scores$hn))
uhs <- unique(scores$hn)[order(unique(scores$hn))]
uhs

# NOTE: REMOVE THIS PART WHEN HOSP DIVISIONS ARE ADDED TO MD DATASET!
scores[, mu := mean(overall, na.rm = TRUE),
       by = c("year", "hn")]
scores[, mu.smoke := mean(smoke, na.rm = TRUE),
       by = c("year", "hn")]
scores <- unique(scores[, -c(2, 4:5), with = FALSE])
scores

# Plot scores, four hospitals at the time
# Source: http://stackoverflow.com/questions/18394391/r-custom-legend-for-multiple-layer-ggplot
j <- unique(scores$hn)[5:8]
ggplot(subset(scores,
              hn %in% j)) + 
  facet_wrap(~ hn) +
  geom_line(aes(x = year, 
                y = mu,
                color = "black")) +
  geom_point(aes(x = year, 
                 y = mu,
                 color = "black"),
             size = 3) +
  geom_line(aes(x = year + 0.2, 
                y = mu.smoke,
                color = "red")) +
  geom_point(aes(x = year + 0.2, 
                 y = mu.smoke,
                 color = "red"),
             size = 3) +
  scale_colour_manual(name = "Scores:", 
                      values =c("black" = "black",
                                "red" = "red"),
                      labels = c("Overall",
                                 "Smoking")) +
  theme(legend.position = "top")
# geom_errorbar(aes(ymin = lb, 
#                   ymax = ub),
#               colour = "black", 
#               width = .1) 

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
md$FIRSTMIAGE <- as.numeric(as.character(floor(difftime(md$FIRSTMIDSCDATE, md$PATBDTE, units = "days")/365.25)))
hist(md$FIRSTMIAGE)
range(md$FIRSTMIAGE, na.rm = TRUE)
# Remove anybody younger than 20
md <- subset(md, FIRSTMIAGE >= 20)
hist(md$FIRSTMIAGE)

# Death
md$NEWDTD <- as.Date(md$NEWDTD, origin = "1960-01-01")
range(md$NEWDTD, na.rm = TRUE)
md$DEATHAGE <- as.numeric(as.character(floor(difftime(md$NEWDTD, md$PATBDTE, units = "days")/365.25)))
hist(md$DEATHAGE)

# Hospital number
md$FIRSTMIHOSP <- as.numeric(as.character(md$FIRSTMIHOSP))
md$SECONDMIHOSP <- as.numeric(as.character(md$SECONDMIHOSP))
md$THIRDMIHOSP <- as.numeric(as.character(md$THIRDMIHOSP))

#############################################################
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
  a =summary(aa<-coxph(Surv(sdmi, status) ~ age+ factor(year), data=data))
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
  for(i in 1995:2013) {
    try({
      a <- hospfun(md, j, i-1, i+1)
      out[[i]] <- c(hn = j, year = i, a)
    })
  }
  res[[j]] <- do.call("rbind", out)
}
hr <- do.call("rbind", res)
hr <- data.table(hr)

# Remove bad fits
hr <- hr[is.finite(hr$`upper .95`),]
hrr <- subset(hr, select = c(1:2, 9:11))
hrr <- merge(hrr, scores, by = c("hn", "year"), all = TRUE)
names(hrr)[3:5] <- c("hr", "lb", "ub")

#############################################################
# PLOT
j <-hosp[3]
tmp <- subset(hrr, hn %in% j)

# Scale HR
r.hr <- range(tmp$hr, na.rm = TRUE)
r.mu  <- range(c(tmp$mu, tmp$mu.smoke), na.rm = TRUE)
mm <- diff(r.mu)/diff(r.hr)
tmp$hr.scaled <- mm*tmp$hr + min(r.mu, na.rm = TRUE) - min(mm*tmp$hr, na.rm = TRUE)
hr.1 <- mm + min(r.mu, na.rm = TRUE) - min(mm*tmp$hr, na.rm = TRUE)

ggplot(tmp) + 
  facet_wrap(~ hn) +
  geom_line(aes(x = year, 
                y = mu,
                color = "blue")) +
  geom_point(aes(x = year, 
                 y = mu,
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
                y = hr.scaled,
                color = "red")) +
  geom_point(aes(x = year, 
                 y = hr.scaled,
                 color = "red"),
             size = 3) +
  geom_hline(yintercept = hr.1,
             linetype = "dashed") +
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
  # geom_errorbar(aes(x = year + 0.4, 
  #                   ymin = lb,
  #                   ymax = ub),
  #               colour = "black",
  #               width = .1)
ToDo 08/19/2016
ADD:
  
2nd y-axis if possible
x and y axis labels
multiple panels with free scale