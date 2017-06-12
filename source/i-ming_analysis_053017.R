

#Prepared for Dakota research group (Georgia Barbayannis, Davit Sargsyan, I-Ming Chiu, and Noah Michel)

require(data.table)
require(ggplot2)
require(stringr)
require(broom)
require(survival)

# assuming zip contains old 4-digit zip codes
da1 = read.csv("nj_income.csv", header = T) #this is the NJ average household income data I emailed to you
head(da1)
da1$ZIP <- str_pad(da1$zip, width=5, side='left', pad='0')

da2 = read.csv("final_0526.csv", header = T) #this is the data constructed on 05/26 based on Davit's code
head(da2)

da = merge(da1, da2, by = "ZIP")
dim(da)
head(da)

out = summary(da$hincome) #hincome: average household income in each zip code in NJ
da$SES = NA #SES:socio-economic status; four levels are selcted based on the quartiles of hincome

da[da$hincome > out[5],]$SES = c("A")
da[da$hincome > out[3] & da$hincome <= out[5],]$SES = c("B")
da[da$hincome > out[2] & da$hincome <= out[3],]$SES = c("C")
da[da$hincome <= out[2],]$SES = c("D")

#write.csv(da, "dakotadata_0528.csv", row.names = F) #finalized data (everything)

#Data visualization for covariates (nStatin is not plotted yet)
#Figure 1.1
par(mfrow = c(3,1))
barplot(prop.table(table(da$SEX)))
barplot(prop.table(table(da$PRIME)))
barplot(prop.table(table(da$RACE)))

#Figure 1.2
par(mfrow=c(2,1))
hist(da$hincome, breaks = 8, main = "Household Income", xlab = "Average Household Income", freq = F)
hist(da$AGE, breaks = 8, main = "Age", xlab = "Age", freq = F)

#Kaplan-Meier
obj1 = with(da, Surv(days2death, dead))
obj2 = with(da, Surv(days2mi2, post.ami))
km1 = survfit(obj1 ~ 1,  type="kaplan-meier", conf.type="log", data = da)
km2 = survfit(obj2 ~ 1,  type="kaplan-meier", conf.type="log", data = da)
kmses1 = survfit(obj1 ~ SES,  type="kaplan-meier", conf.type="log", data = da)
kmses2 = survfit(obj2 ~ SES,  type="kaplan-meier", conf.type="log", data = da)

#Figure 2: Kaplan-Meier plot without covariate (comparing time to death to readmission)
plot(km1, main= "Kaplan-Meier-Estimate \n (time to death vs. readmission)", xlab="Days", ylab="Survival", col = "blue", ylim=c(0.4,1))
lines(km2, col = "red")
legend("topright", col= c("blue", "red"), lwd=2, legend=c("death", "readmission"))

#Figure 3.1: Kaplan-Meier plot with Social Economic Status (time to death)
plot(kmses1, main= "Kaplan-Meier-Estimate \n (time to death) \n Social Economic Status", xlab="Days", ylab="Survival",col = 1:4, ylim = c(0.4,1))
legend("topright", col=1:4, lwd=2, legend= c("income > 75%", "50% < income < 75%", "25% < income < 50%", "income < 25%"))

#Figure 3.2: Kaplan-Meier plot with Social Economic Status (time to readmission)
plot(kmses2, main= "Kaplan-Meier-Estimate \n (time to readmission) \n Social Economic Status", xlab="Days", ylab="Survival",col = 1:4, ylim = c(0.65,1))
legend("topright", col=1:4, lwd=2, legend= c("income > 75%", "50% < income < 75%", "25% < income < 50%", "income < 25%"))


#After testing difference hospital quality scores, only one score stands out (i.e.Statin).
#The full model (time to death vs. time to readmission)
m1 <- coxph(Surv(days2death, dead) ~ SEX + PRIME + RACE + AGE + SES + nStatin, data = da)
m2 <- coxph(Surv(days2mi2, post.ami) ~ SEX + PRIME + RACE + AGE + SES + nStatin, data = da)
table.m1 = tidy(m1) #the selected model where the response is time to death
table.m2 = tidy(m2) #the selected model where the response is time to 2nd admission
write.csv(table.m1, "table01.csv") #Table 1 in the report
write.csv(table.m2, "table02.csv") #Table 2 in the report

#Figure 4: Survial plot for the full model (time to death vs. time to readmission)
plot(survfit(m1), xlab = "Days", ylab = "Survival", col = "blue", ylim=c(0.5,1))
lines(survfit(m2), col = "red")
legend("bottomleft", legend=c("death", "readmission"), col = c("blue", "red"), lty=c(1,1))

#-----------------------------
                  
              
