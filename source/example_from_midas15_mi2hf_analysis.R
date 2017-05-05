# Author:   Davit Sargsyan
# Created:  04/29/2016
# Last modified: 07/09/2016
# Project: HF after first MI, Jen Wellings
#**********************************************************
# PART I: Load data----
DATA_HOME <- "C:/Users/ds752/Documents/git_local/data/midas.mi2hf"
require(data.table)
require(ggplot2)

load(file.path(DATA_HOME, "case_02052017.RData"))

# Make age by decade
case$decade <- floor(case$AGE/10)
summary(case$decade)

# roc.plot <- function(mod.name, main) {
#   c.stat <- list()
#   for (i in 1:length(mod.name)) {
#     out <- eval(parse(text = paste("roc(predictions = predict(",
#                                    mod.name[i],
#                                    ", type = 'response'), labels = factor(",
#                                    mod.name[i],
#                                    "$y))",
#                                    sep = "")))
#     if (i == 1) {
#       plot(out,
#            col=i,
#            lty = i,
#            lwd=2,
#            main = main)
#     } else {
#       plot(out,
#            add = T,
#            col = i,
#            lty = i,
#            lwd = 2)
#     }
#     
#     c.stat[[i]] <- auc(out)
#   }
#   c.stat <- round(do.call("c", c.stat), 3)
#   c.stat
#   
#   legend("bottomright",
#          legend = paste(mod.name, ", c stat =", c.stat),
#          col = 1:length(c.stat),
#          lty = 1:length(c.stat))
# }

#**********************************************************
# PART II: Rates----
# 30 days----
t1 <- addmargins(table(case$dschyear,
            case$post.chf.acute.dx1.30))
t1 <- data.table(year = rownames(t1),
                 hf30 = t1[, 2],
                 N = t1[ , 3])
t1[, rate := round(100*hf30/N, 2)]

# Remove total and 2015
t1 <- t1[-c((nrow(t1) - 1):nrow(t1)),]
t1$year <- as.numeric(t1$year)

# 90 days----
t2 <- addmargins(table(case$dschyear,
                       case$post.chf.acute.dx1.90))
t2 <- data.table(year = rownames(t2),
                 hf90 = t2[, 2],
                 N = t2[ , 3])
t2[, rate := round(100*hf90/N, 2)]

# Remove total and 2015
t2 <- t2[-c((nrow(t2) - 1):nrow(t2)),]
t2$year <- as.numeric(t2$year)

# 180 days----
t3 <- addmargins(table(case$dschyear,
                       case$post.chf.acute.dx1.180))
t3 <- data.table(year = rownames(t3),
                 hf180 = t3[, 2],
                 N = t3[ , 3])
t3[, rate := round(100*hf180/N, 2)]

# Remove total and 2015
t3 <- t3[-c((nrow(t3) - 1):nrow(t3)),]
t3$year <- as.numeric(t3$year)

# 1 year----
t4 <- addmargins(table(case$dschyear,
                       case$post.chf.acute.dx1.1y))
t4 <- data.table(year = rownames(t4),
                 hf1y = t4[, 2],
                 N = t4[ , 3])
t4[, rate := round(100*hf1y/N, 2)]

# Remove total and 2015
t4 <- t4[-c((nrow(t4) - 1):nrow(t4)),]
t4$year <- as.numeric(t4$year)

# Combine and plot----
tt1 <- data.table(year = t1$year,
                  rate.30 = t1$rate,
                  rate.90 = t2$rate,
                  rate.180 = t3$rate,
                  rate.1y = t4$rate)
tt1
write.csv(tt1, 
          file = "tmp/hf_after_ami_rates.csv",
          row.names = FALSE)
plot(tt1)

tt1.l <- melt.data.table(data = tt1, 
                         id.vars = "year",
                         measure.vars = c(2:5))
tt1.l$variable <- factor(tt1.l$variable,
                         levels = rev(levels(tt1.l$variable)))

# Plot HF rates over time----
tiff(filename = "tmp/hf_after_ami_rates.tiff",
     height = 5,
     width = 6,
     units = 'in',
     res = 300,
     compression = "lzw+p")
ggplot(tt1.l,
       aes(x = year,
           y = value,
           colour = variable,
           group = variable)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0,
             linetype = "dashed") +
  # geom_vline(xintercept = 2000,
  #            linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1)) +
  scale_x_continuous("Year",
                     breaks = unique(tt1.l$year),
                     labels = unique(tt1.l$year)) +
  scale_y_continuous("Rate (%)",
                     limits = c(0, 8.5)) +
  scale_colour_manual(label = c("1 Year",
                                "180 Days",
                                "90 Days",
                                "30 Days"),
                      values = unique(tt1.l$variable)) +
  ggtitle("HF Admission After AMI Discharge") +
  guides(colour = guide_legend(title = "Follow-up"))
graphics.off()

#**********************************************************
# PART III: Models----
s1 <- list()

# Model1.30: Acute CHF, 30 days
m1.30 <- glm(post.chf.acute.dx1.30 ~ decade + 
               SEX +
               dschyear + 
               hhyp + 
               hdiab + 
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data  = case)
s1$d30 <- summary(m1.30)
s1$d30

# Model1.90: Acute CHF, 90 days
m1.90 <- glm(post.chf.acute.dx1.90 ~ decade + 
               SEX +
               dschyear + 
               hhyp + 
               hdiab + 
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data  = case)
s1$d90 <- summary(m1.90)
s1$d90

# Model1.180: Acute CHF, 180 days
m1.180 <- glm(post.chf.acute.dx1.180 ~ decade + 
                SEX +
                dschyear + 
                hhyp + 
                hdiab + 
                hckd + 
                hcopd +
                hlipid,
              family = binomial(logit),
              data  = case)
s1$d180 <- summary(m1.180)
s1$d180

# Model1.1y: Acute CHF, 1 year
m1.1y <- glm(post.chf.acute.dx1.1y ~ decade + 
               SEX +
               dschyear + 
               hhyp + 
               hdiab + 
               hckd + 
               hcopd +
               hlipid,
             family = binomial(logit),
             data  = case)
s1$y1 <- summary(m1.1y)
s1$y1

# Combine
t.col.names <- c("Age (by Decade)",
                 "Sex (Male)",
                 "Discharge Year",
                 "History of Hypertension",
                 "History of Diabetes",
                 "History of Chronic Kidney Disease",
                 "History of COPD",
                 "History of Disorder of Lipoid Metabolism")

tm0 <- lapply(s1, function(a) {
  out <- data.table(names = rownames(a$coefficients)[-1],
                    m1.cf = exp(a$coefficients[-1, 1]),
                    m1.lb = exp(a$coefficients[-1, 1] - 1.96*a$coefficients[-1, 2]),
                    m1.ub = exp(a$coefficients[-1, 1] + 1.96*a$coefficients[-1, 2]),
                    pval = ifelse(a$coefficients[-1, 4] >= 0.001,
                                  round(a$coefficients[-1, 4], 3),
                                  "<0.001"),
                    star = ifelse(a$coefficients[-1, 4] <= 0.05, 
                                  ifelse(a$coefficients[-1, 4] <= 0.01,
                                         "**",
                                         "*"), ""))
  rownames(out) <- NULL
  out$names <- t.col.names
  return(out)
})

tm1 <- do.call("rbind", tm0)
tm1 <- data.table(cutoff = rep(c("30 Days",
                                 "90 Days",
                                 "180 Days",
                                 "1 Year"),
                               each = length(t.col.names)),
                  tm1)
tm1$names <- factor(tm1$names, levels = unique(tm1$names))
tm1$cutoff <- factor(as.character(tm1$cutoff),
                     levels = c("30 Days", "90 Days", "180 Days",  "1 Year"))
tm1
write.csv(tm1, 
          file = "tmp/hf_after_ami_or.csv",
          row.names = FALSE)
# Plot OR----
tiff(filename = "tmp/hf_after_ami_or.tiff",
     height = 8,
     width = 8,
     units = 'in',
     res = 300,
     compression = "lzw+p")
ggplot(tm1, 
       aes(x = names, 
           y = m1.cf)) +
  facet_wrap( ~ cutoff, 
             ncol = 2) + 
  geom_hline(yintercept = 1,
             colour = "grey",
             size = 1.1,
             linetype = 2) +
  geom_errorbar(aes(ymin = m1.lb,
                    ymax = m1.ub,
                    colour = names),
                size = 1.1,
                width = 0.2) +
  geom_point(aes(x = names, 
                 y = m1.cf),
             size = 2) +
  scale_colour_discrete(guide = FALSE) +
  scale_x_discrete("Risk Factors") + 
  scale_y_continuous("Odds Ratios") + 
  ggtitle("Risk of Acute Congestive Heart Failure After 1st Acute MI") +
  theme(axis.text.x = element_text(size = 12,
                                   angle = 45, 
                                   hjust = 1),
        axis.text.y = element_text(size = 12),
        text = element_text(size = 12))
graphics.off()

CONTINUE HERE 02/05/2017!!!!
########################################
t3.1 <- addmargins(table(case$dschyear, case$post.chf.acute.30))
t3 <- data.table(Year = rownames(t3.1),
                 n = t3.1[, 2],
                 N = t3.1[, 1],
                 Sum = t3.1[, 3], 
                 Rate = round(100*t3.1[, 2]/t3.1[, 3], 2))
t3

t4.1 <- addmargins(table(case$dschyear, case$post.chf.acute.90))
t4 <- data.table(Year = rownames(t4.1),
                 n = t4.1[, 2],
                 N = t4.1[, 1],
                 Sum = t4.1[, 3], 
                 Rate = round(100*t4.1[, 2]/t4.1[, 3], 2))
t4

t5.1 <- addmargins(table(case$dschyear, case$post.chf.acute.180))
t5 <- data.table(Year = rownames(t5.1),
                 n = t5.1[, 2],
                 N = t5.1[, 1],
                 Sum = t5.1[, 3], 
                 Rate = round(100*t5.1[, 2]/t5.1[, 3], 2))
t5

t6.1 <- addmargins(table(case$dschyear, case$post.chf.acute.1y))
t6 <- data.table(Year = rownames(t6.1),
                 n = t6.1[, 2],
                 N = t6.1[, 1],
                 Sum = t6.1[, 3], 
                 Rate = round(100*t6.1[, 2]/t6.1[, 3], 2))
t6

write.csv(cbind(t3, t4, t5, t6), file = "tmp/tm2.csv")

########################################
# Combined plot
tiff(filename = "tmp/plot2.tiff",
     width = 14.58,
     height = 6.77,
     units = "in",
     res = 400)
# 30 days
plot(t3$Rate[-13] ~ as.numeric(t3$Year[-13]),
     type = "l",
     main = "Trends in Heart Failure Admission Following Acute Myocardial Infarction",
     ylim = c(0, 25),
     ylab = "HF Rate (%)",
     xlab = "Year of Acute MI Admission",
     col = "green",
     xaxt = "n")
points(t3$Rate[-13] ~ as.numeric(t3$Year[-13]),
       col = "green",
       pch = 16)
# 90 days
lines(t4$Rate[-13] ~ as.numeric(t4$Year[-13]),
      col = "blue")
points(t4$Rate[-13] ~ as.numeric(t4$Year[-13]),
      col = "blue",
      pch = 16)
# 180 days
lines(t5$Rate[-13] ~ as.numeric(t5$Year[-13]),
      col = "red")
points(t5$Rate[-13] ~ as.numeric(t5$Year[-13]),
       col = "red",
       pch = 16)
# 1 year
lines(t6$Rate[-13] ~ as.numeric(t6$Year[-13]),
      col = "black")
points(t6$Rate[-13] ~ as.numeric(t6$Year[-13]),
       col = "black",
       pch = 16)
axis(side = 1,
     at = 2000:2011,
     labels = 2000:2011)
legend("bottomright",
       legend = rev(c("30 Days Follow-up",
                      "90 Days Follow-up",
                      "180 Days Follow-up",
                      "1 Year Follow-up")),
       lty = 1,
       pch = 16,
       col = rev(c("green",
                   "blue",
                   "red",
                   "black")))
graphics.off()