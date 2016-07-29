library(data.table)
library(survival)
#HOSPITAL CODE (Not in Current Workspace and not code for most current hospital dataset) 
x = read.csv("HDS.csv")
x = read.csv("Excel Tables/HDS_J_030813.csv")
q = read.csv("E:/Data/Excel Tables/HDS_J_052813.csv")


hid = unique(x[,2]); hid
OHATS = array(NA, dim=c(8,72))
f3=(as.numeric(as.character(x[,3])))
for(i in 1:72) { print(i);OHATS[,i] = f3[x[,2] == hid[i]]}
matplot(2004:2011,log( OHATS/(101-OHATS)) ,type="l", xlab = "YEAR", ylab= "LOGIT-PCT")
matplot(2004:2011,OHATS ,type="l", xlab = "YEAR", ylab= "SCORE")

#Need to find change point for each hospital (max 2 years) 
year = 2004:2011

Delta I
i  y0    y1  
1 2004 2006 
2 2004 2005
3 2004 2006
4 2004 2006
5 2007 2009
6 2004 2006
7 2004 2005
8 2006 2007
9 2004 2005
10 2005 2007
11 2004 2005
12 2005 2007
13 2004 2005
14 2007 2009
15 2004 2006
16 2007 2008
17 2004 2006
18 2006 2007
19 2004 2006
20 2007 2009
21 2005 2007
22 2005 2006
23 2005 2007
24 2004 2005 
25 2004 2006
26 2004 2006
27 2005 2007
28 2006 2007
29 2005 2007
30 2007 2008
31 2007 2008
32 2005 2007
33 2005 2007
34 2006 2008
35 2006 2008
36 2008 2009
37 2005 2007
38 2005 2007
39 2004 2006
40 2007 2008
41 2004 2005
42 2004 2005
43 2004 2006
44 2004 2005
45 2004 2006
46 2008 2009
47 2005 2006
48 2005 2007
49 2006 2008
50 2004 2006
51 2004 2006 
52 2004 2006
53 2005 2007
54 2004 2005
55 2004 2006 
56 2005 2007 
57 2006 2008
58 2008 2009
59 2004 2005
60 2004 2006
61 2007 2009
62 2004 2006
63 2005 2007 
64 2005 2007
65 2004 2006
66 2004 2005
67 2005 2007
68 2004 2005
69 2007 2009
70 2005 2006
71 2004 2005
72 2004 2006


pdf("grphs.pdf", width=9,height=7.5)
par(mfrow=c(6,6),mar=c(4,3,1,1))
for(i in 1:72)
plot(year,OHATS[,i],pch=20,col=2,xlab=as.character(hid[i]))
dev.off()

library(rpart)
rpart(OHATS[,1]~year,control=rpart.control(minbucket=2,minsplit=6,cp=0.00001))


z = array(NA, dim=c(8,72))
f3=(as.numeric(as.character(hds[,3])))	
for(i in 1:72) { print(i);z[,i] = f3[hds[,2] == u[i]]}
z[,i] = c(NA,f3[(x[,2] == u[i])])
matplot(2004:2011,log( z/(101-z)) ,type="l", xlab = "YEAR", ylab= "LOGIT-PCT",main=?Overall Score?)
matplot(2004:2011,z ,type="l", xlab = "YEAR", ylab= "SCORE",main=?Overall Score?)


z1 = z
z2 = t(t(z1) - z[1,])
matplot(2005:2011,z2 ,type="l", xlab = "YEAR", ylab= "SCORE",main="Overall Score")


#MIDAS CODE
md2 = read.csv("E:/midas.csv")
md2 = read.csv("midas.csv")
md = read.csv("C:/MIDAS/qcmidasfull.csv")
md0 =read.csv("C:/Users/J C/Documents/Kostis/Citron/Data/Excel Tables/miqcdatafullcomorb.csv")
md0 <- as.data.frame(copy(d1))
#md00=md0;
md1 = md0[,names(md)[-29]]
# Function that does teh analysis
hospfun = function(md,hn,y1,y2) {
y1= 2004 ; y2 = 2006;hn=1;
  md64 = md1[md1$FIRSTMIHOSP == hn,]
md64$dt1disc = as.Date(as.character(md64[,5]),"%m/%d/%Y")
md64$dtbirth= as.Date(md64[,2],origin = "1960-01-01")
md64$age = as.numeric((md64$dt1disc-md64$dtbirth)/365)
md64$dtdeath = as.Date(md64[,1],origin = "1960-01-01")
#md64$dtdeath = as.Date(as.character(md64[,1]),"%d%B%Y")
md64$agedeath = as.numeric((md64$dtdeath-md64$dtbirth)/365)
md64$dt2mi = as.Date(as.character(md64[,10]),"%m/%d/%Y")
md64$censored = (!is.na(md64$dtdeath) & (md64$dtdeath>=md64$dt1disc)) & is.na(md64$dt2mi)

dy64 = substring(md64$dt1disc,1,4)
dy64a = dy64 == as.character(y1)
dy64a[is.na(dy64a)] = F
md64a = md64[dy64a,]
sdd64 = md64a$dt2mi
i1 = is.na(sdd64)
sdd64[i1] = maxdat2
md64a$smi = sdd64
md64a$smidd = as.numeric(md64a$smi-md64a$dt1disc) 

md64b = md[md$FIRSTMIHOSP == hn,]
d64 = as.character(md64b[,5])
dd64 = as.Date(d64,"%d%B%Y")
bdt = as.Date(as.character(md64b[,2]),"%d%B%Y")
md64b$age = as.numeric((dd64-bdt)/365)
dy64 = substring(dd64,1,4)
dy64a = dy64 == as.character(y2)
dy64a[is.na(dy64a)] = F
md64b$dd = dd64
md64b = md64b[dy64a,]
sdd64 = as.Date(as.character(md64b[,10]),"%d%B%Y")
i2 = is.na(sdd64)
sdd64[i2] = maxdat2
#correct for death 
md64b$smi = sdd64
md64b$smidd = as.numeric(md64b$smi-md64b$dd) 
#boxplot(narm(md64a$smidd), narm(md64b$smidd),names=as.character(c(y1,y2)),sub=hn)
#t.test(narm(md64b$smidd), narm(md64a$smidd))
data = data.frame(status=1-c(i1,i2),rbind( cbind(age=md64a$age,year=y1,sdmi=md64a$smidd),
       cbind(age=md64b$age,year=y2,sdmi=md64b$smidd )))
 a =summary(aa<-coxph(Surv(sdmi, status) ~ age+ factor(year), data=data))
c(a$coef[2,], a$conf.int[2,])
}
res = NULL
par(mfrow=c(3,3),mar=c(5,4,2,2))
hn=64
yy= 2004
for(yy in 2000:2008){
a = hospfun(md0,hn,yy,yy+2)
res=rbind(res,c(hn=hn, year=yy,a))
#res = rbind(res, c(hn=hn,year=yy, round(c(u$est,cidiff=u$conf, pv=u$p.v),5)))
}

#Death
bdt = as.Date(as.character(md64[,2]),"%d%B%Y")
dth64 = as.Date(as.character(md64[,1]),"%d%B%Y")
md64$death = as.numeric((dth64-bdt)/365)
sd64 = as.Date(as.character(md64[,10]),"%d%B%Y")
dnomi = (!is.na(dth64) & is.na(sd64))

hospfunc = function(md,hn,y1,y2) {
md64 = md[md$FIRSTMIHOSP == hn,]
md64$dt1disc = as.Date(as.character(md64[,5]),"%d%B%Y")
md64$dtbirth = as.Date(as.character(md64[,2]),"%d%B%Y")
md64$age = as.numeric((md64$dt1disc-md64$dtbirth)/365)
md64$dtdeath = as.Date(as.character(md64[,1]),"%d%B%Y")
md64$agedeath = as.numeric((md64$dtdeath-md64$dtbirth)/365)
md64$dt2mi = as.Date(as.character(md64[,10]),"%d%B%Y")
md64$censored = (!is.na(md64$dtdeath) & (md64$dtdeath>=md64$dt1disc)) & is.na(md64$dt2mi)

dy64 = substring(md64$dt1disc,1,4)
dy64a = dy64 == as.character(y1)
dy64a[is.na(dy64a)] = F
md64a = md64[dy64a,]
sdd64 = md64a$dt2mi
i1 = is.na(sdd64)
sdd64[i1] = maxdat2
md64a$smi = sdd64
md64a$smidd = as.numeric(md64a$smi-md64a$dt1disc) 

dy64b = dy64 == as.character(y2)
dy64b[is.na(dy64b)] = F
md64b = md64[dy64b,]
sdd64b = md64b$dt2mi
i2 = is.na(sdd64b)
sdd64b[i2] = maxdat2
md64b$smi = sdd64b
md64b$smidd = as.numeric(md64b$smi-md64b$dt1disc) 

#boxplot(narm(md64a$smidd), narm(md64b$smidd),names=as.character(c(y1,y2)),sub=hn)
#t.test(narm(md64b$smidd), narm(md64a$smidd))
data = data.frame(status=1-c(i1,i2),rbind( cbind(age=md64a$age,year=y1,sdmi=md64a$smidd),
       cbind(age=md64b$age,year=y2,sdmi=md64b$smidd )))
 a =summary(coxph(Surv(sdmi, status) ~ age+ factor(year), data=data))
c(a$coef[2,], a$conf.int[2,])
}
library(survival)
par(mfrow=c(3,3),mar=c(5,4,2,2))

#res = NULL
for( hn in hospn[84:86]) {
for(yy in 2000:2008){
u=hospfunc(md,hn,yy,yy+2)
res = rbind(res, c(hn=hn,year=yy, round(u,5)))
}
}
 hospn = nospn[!is.na(nospn)] 
 nospn = unique(md$FIRSTMIHOSP)



hhospn1 = sort(unique(round(x$Hospital)))

pdf(file="a.pdf",paper="letter")
par(mfrow=c(4,5),mar=c(4,2,1,1),cex=0.5)
for( hn in hhospn1[-c(42,45)]) {
scoreTot= as.numeric(as.character(x[x$Hospital==hn,6]))
yea= as.numeric(as.character(x[x$Hospital==hn,1]))
names(scoreTot)= yea
par(cex=0.7)
plot(scoreTot[as.character(res$year[res$hn==hn])], res$z[res$hn==hn],
     xlab=hn,ylab="Tstats",type="l",sub=hn)
text(scoreTot[as.character(res$year[res$hn==hn])], res$z[res$hn==hn], 
       substr(as.character(res$year[res$hn==1]),4,4))
par(cex=0.5)
print(hn)
}
dev.off()

###################################################################################################################################################
hospfunc = function(md,hn,y1,y2) {
md64 = md[md$FIRSTMIHOSP == hn,]
md64$dt1disc = as.Date(as.character(md64[,5]),"%d%B%Y")
md64$dtbirth = as.Date(as.character(md64[,2]),"%d%B%Y")
md64$age = as.numeric((md64$dt1disc-md64$dtbirth)/365)
md64$dtdeath = as.Date(as.character(md64[,1]),"%d%B%Y")
md64$agedeath = as.numeric((md64$dtdeath-md64$dtbirth)/365)
md64$dt2mi = as.Date(as.character(md64[,10]),"%d%B%Y")
md64$censored = (!is.na(md64$dtdeath) & (md64$dtdeath>=md64$dt1disc)) & is.na(md64$dt2mi)

dy64 = substring(md64$dt1disc,1,4)
dy64a = dy64 == as.character(y1)
dy64a[is.na(dy64a)] = F
md64a = md64[dy64a,]
sdd64 = md64a$dt2mi
i1 = is.na(sdd64)
sdd64[i1] = maxdat2
md64a$smi = sdd64
md64a$smidd = as.numeric(md64a$smi-md64a$dt1disc) 

dy64b = dy64 == as.character(y2)
dy64b[is.na(dy64b)] = F
md64b = md64[dy64b,]
sdd64b = md64b$dt2mi
i2 = is.na(sdd64b)
sdd64b[i2] = maxdat2
md64b$smi = sdd64b
md64b$smidd = as.numeric(md64b$smi-md64b$dt1disc) 

#boxplot(narm(md64a$smidd), narm(md64b$smidd),names=as.character(c(y1,y2)),sub=hn)
#t.test(narm(md64b$smidd), narm(md64a$smidd))
data = data.frame(status=1-c(i1,i2),rbind(cbind(age=md64a$age,year=y1,sdmi=md64a$smidd),
       cbind(age=md64b$age,year=y2,sdmi=md64b$smidd )))
 a =summary(coxph(Surv(sdmi, status) ~ age+ factor(year), data=data))
c(a$coef[2,], a$conf.int[2,])
}
library(survival)
par(mfrow=c(3,3),mar=c(5,4,2,2))

#res = NULL
for( hn in hospn[67]) {
for(yy in 2000:2008){
u=hospfunc(md,hn,yy,yy+2)
res = rbind(res, c(hn=hn,year=yy, round(u,5)))
}
}
 hospn = nospn[!is.na(nospn)] 
 nospn = unique(md$FIRSTMIHOSP)

res01 = NULL
#for( hn in hospn[72]) {
hn=72
for(yy in 2004:2008){
u=hospfunc(md,hn,yy,yy+2)

res01 = rbind(res01, c(hn=hn,year=yy, round(u,5)))
}

yy
yy

hhospn1 = sort(unique(round(x$Hospital)))

pdf(file="bba.pdf",paper="letter")
par(mfrow=c(4,5),mar=c(4,2,1,1),cex=0.5)
for( hn in hhospn1[-c(42,45)]) {
scoreTot= as.numeric(as.character(x[x$Hospital==hn,11]))
yea= as.numeric(as.character(x[x$Hospital==hn,1]))
names(scoreTot)= yea
par(cex=0.7)
plot(scoreTot[as.character(res$year[res$hn==hn])], res$z[res$hn==hn],
     xlab=hn,ylab="Tstats",type="l",sub=hn)
text(scoreTot[as.character(res$year[res$hn==hn])], res$z[res$hn==hn], 
       substr(as.character(res$year[res$hn==1]),4,4))
par(cex=0.5)
print(hn)
}
dev.off()

# hospn
 [1]   1   2   3   5   6   8   9  10  11  12  13  14  15  16  17  18  19  20  21
[20]  22  24  25  26  27  28  29  31  32  34  36  37  38  39  40  41  42  43  44
[39]  45  47  48  49  50  51  52  54  56  57  58  60  61  62  63  64  67  69  70
[58]  72  73  74  75  76  77  78  81  83  84  86  87  88  90  91  92  93  96 105
[77] 108 110 111 112 113 115 116 118 119 120

res0 = res[c(412,184,609,381,77,551,299,566,601,126,118,59,286,100,226,500,308,5,630,278,51,33,452,443,582,519,512,399,653,542,463,110,364,15,209,351,220,342,23,268,244,263,592,176,317,324,235,425,619,470,333,672,160,142,636,44,434,662,
491,68,87,169,387,369),]

#412(Hospital 64,represents hospital 64.1, now at 71 hospitals) 
#519 (Hospital 86,represents hospital 86.1 & 86.2, now at 69 hospitals) 
#268(Hospital 39,represents hospitals 39.1, now at 68 hospitals)
#169 (Hospital 22, represents hospitals 22.1 & 22.2, now at 66 hospitals)
#No res score for hosprital 67,72 even though there are HPR scores for all years, possibly at 64 hospitals 
#Currently missing data in res0 for 8 hospitals

#Hospital 110 HPR had equal 1 point increases in scores for the years 05-06(592*)and 07-08(594), used 592 for res
#Hospital 32 HPR had equal 6 point increases in scores for the years 04-06(235) and 05-07(236*),used 235 for res
#Hospital 50 HPR had equal 4 point increases in scores for th years 04-06(333*) and 05-07(334), used 333 for res
#551*,160*,630*,636*,452*,443*,333*,592*,stat. sig.5% level* 

#Graph with Coefficients
cd = c(rbind(res0[,3]+ 1.96*res0[,5],res0[,3]-1.96*res0[,5],NA))
barplot(names=factor(res0[,1]),res0[,3],ylim=c(-4,4),cex.names=0.7,col=13,las=2)-> uu
par(mar=c(5,4,1,1))
barplot(res0[,3],ylim=c(-4,4),col=13,las=2,ylab="HR")-> uu
abline(h=0,lwd=3)
uu = c(uu)
axis(1,37,"Hospital",tick=F)
cd1 = c(rbind(uu,uu,NA))
lines(cd1,cd,lwd=1,col=2)
box()

#Graph with HR
cd2 = c(rbind(res0[,10],res0[,11],NA))
barplot(names=factor(res0[,1]),res0[,3],ylim=c(-4,4),cex.names=0.7,col=13,las=2)-> uu
par(mar=c(5,4,1,1))
barplot(res0[,4],ylim=c(0,6),col=13,las=2,ylab="HR")-> uu
abline(h=1,lwd=3)
uu = c(uu)
axis(1,37,"Hospital",tick=F)
cd1 = c(rbind(uu,uu,NA))
lines(cd1,cd2,lwd=1,col=2)
box()

#Merging of X and Res0 for Delta by HR graph
names(x)[6]<-"hn"
resx = merge(x,res0,by="hn")
#Removing of Deltas
resx1 = resx[!is.na(resx$Delta),]
indic = array(NA, dim=c(64,1))
indic[resx1[,24]<0.05]=0
indic[resx1[,24]>=0.05]=1
indic = as.factor(indic)
plot(resx1$Delta, resx1$exp.coef., main="Grade Change by Hazard Ratio", xlab="Grade Change", col=c(2,1)[indic], ylab="HR")

#abline(lsfit(resx1$Delta, resx1$exp.coef.))
cor(resx1$Delta, resx1$exp.coef.)
cor.test(resx1$Delta, resx1$exp.coef.)#Pearsons Correlation Test##

#############################################################################################################################################################################################################################
##Need to control for Age, Gender(md2,151), Race(md2,120), Cancer (md2,49), COPD(md2,75), KidenyDisease(md2,83), Hyper Tension(md2,71), High Cholesterol(md2,85), HF(md2,81), Anemia(md2,53), Afib(md2,51), and Diabetes(md2,63)
##Need to then control for Age, Gender, Race, Cancer, COPD, KidenyDisease, Hyper Tension, High Cholesterol HF, Anemia, Afib, and Diabetes + Hosp. Chara. (Beds,Location, Teaching, Delta?)    

#Race
md2$race = NA
md2$race[md2$RACE == '.']= NA
md2$race[md2$RACE == '1']= '0White'
md2$race[md2$RACE == '2']= '1Black'
md2$race[md2$RACE != '1' & md2$RACE!= '2' & md2$RACE!='.']= '3Other'
table(md2$race)


#Gender
md2$gender=NA
md2$gender[md2$SEX =='F'] = 'Female'
md2$gender[md2$SEX =='M'] = 'Male'
md2$gender[md2$SEX =='U'] = NA
table(md2$gender)

#Diabetes 
md2$dbadm = as.character(md2[,63],"%d/%b/%Y")
md2$dt1adm = as.character(md2[,1],"%d/%b/%Y")
md2$dbcomb[md2$dbadm<=md2$dt1adm]=1
md2$dbcomb[md2$dbadm>md2$dt1adm]= 0
md2$dbcomb[md2$dbadm=='']=0

#Afib
md2$afibadm = as.character(md2[,51],"%d/%b/%Y")
md2$dt1adm = as.character(md2[,1],"%d/%b/%Y")
md2$afibcomb[md2$afibadm<=md2$dt1adm]=1
md2$afibcomb[md2$afibadm>md2$dt1adm]=0
md2$afibcomb[md2$afibadm=='']=0

#Anemia
md2$anemadm = as.character(md2[,53],"%d/%b/%Y")
md2$dt1adm = as.character(md2[,1],"%d/%b/%Y")
md2$anemcomb[md2$anemadm<=md2$dt1adm]=1
md2$anemcomb[md2$anemadm>md2$dt1adm]=0
md2$anemcomb[md2$anemadm=='']=0

#Heart Failure
md2$hfadm = as.character(md2[,81],"%d/%b/%Y")
md2$dt1adm = as.character(md2[,1],"%d/%b/%Y")
md2$hfcomb[md2$hfadm<=md2$dt1adm]=1
md2$hfcomb[md2$hfadm>md2$dt1adm]=0
md2$hfcomb[md2$hfadm=='']=0

#Cancer
md2$canceradm = as.character(md2[,49],"%d/%b/%Y")
md2$dt1adm = as.character(md2[,1],"%d/%b/%Y")
md2$cancercomb[md2$canceradm<=md2$dt1adm]=1
md2$cancercomb[md2$canceradm>md2$dt1adm]=0
md2$cancercomb[md2$canceradm=='']=0

#COPD
md2$copdadm = as.character(md2[,75],"%d/%b/%Y")
md2$dt1adm = as.character(md2[,1],"%d/%b/%Y")
md2$copdcomb[md2$copdadm<=md2$dt1adm]=1
md2$copdcomb[md2$copdadm>md2$dt1adm]=0
md2$copdcomb[md2$copdadm=='']=0

#Kidney Disease 
md2$kdadm = as.character(md2[,83],"%d/%b/%Y")
md2$dt1adm = as.character(md2[,1],"%d/%b/%Y")
md2$kdcomb[md2$kdadm<=md2$dt1adm]=1
md2$kdcomb[md2$kdadm>md2$dt1adm]=0
md2$kdcomb[md2$kdadm=='']=0

#Hyper Tension 
md2$hytadm = as.character(md2[,71],"%d/%b/%Y")
md2$dt1adm = as.character(md2[,1],"%d/%b/%Y")
md2$hytcomb[md2$hytadm<=md2$dt1adm]=1
md2$hytcomb[md2$hytadm>md2$dt1adm]=0
md2$hytcomb[md2$hytadm=='']=0

#High Cholesterol 
md2$hcladm = as.character(md2[,85],"%d/%b/%Y")
md2$dt1adm = as.character(md2[,1],"%d/%b/%Y")
md2$hclcomb[md2$hcladm<=md2$dt1adm]=1
md2$hclcomb[md2$hcladm>md2$dt1adm]=0
md2$hclcomb[md2$hcladm=='']=0
################################################################################################################################################################################
#Multivariate Regression 
	
hospfunc = function(md2,hn,y1,y2) {
md64 = md2[md2$FIRSTMIHOSP == hn,]
md64$dt1disc = as.Date(as.character(md64[,2]),"%m/%d/%Y")
md64$dtbirth = as.Date(as.character(md64[,67]),"%m/%d/%Y")
md64$age = as.numeric((md64$dt1disc-md64$dtbirth)/365)
md64$dtdeath = as.Date(as.character(md64[,68]),"%m/%d/%Y")
md64$agedeath = as.numeric((md64$dtdeath-md64$dtbirth)/365)
md64$dt2mi = as.Date(as.character(md64[,9]),"%m/%d/%Y")
md64$censored = (!is.na(md64$dtdeath) & (md64$dtdeath>=md64$dt1disc)) & is.na(md64$dt2mi)

dy64 = substring(md64$dt1disc,1,4)
dy64a = dy64 == as.character(y1)
dy64a[is.na(dy64a)] = F
md64a = md64[dy64a,]
sdd64 = md64a$dt2mi
i1 = is.na(sdd64)
sdd64[i1] = maxdat2
md64a$smi = sdd64
md64a$smidd = as.numeric(md64a$smi-md64a$dt1disc) 

dy64b = dy64 == as.character(y2)
dy64b[is.na(dy64b)] = F
md64b = md64[dy64b,]
sdd64b = md64b$dt2mi
i2 = is.na(sdd64b)
sdd64b[i2] = maxdat2
md64b$smi = sdd64b
md64b$smidd = as.numeric(md64b$smi-md64b$dt1disc) 
#browser()
#boxplot(narm(md64a$smidd), narm(md64b$smidd),names=as.character(c(y1,y2)),sub=hn)
#t.test(narm(md64b$smidd), narm(md64a$smidd))

if( nrow(md64a) ==0 | nrow(md64b)==0) return( rep(NA,9))

data = data.frame(status=1-c(i1,i2),rbind( data.frame(age=md64a$age,year=y1,sdmi=as.numeric(md64a$smidd),
gender=md64a$gender,hcl=md64a$hclcomb, hyt=md64a$hytcomb,kd=md64a$kdcomb,copd=md64a$copdcomb, cancer=md64a$cancercomb,hf=md64a$hfcomb,anem=md64a$anemcomb,afib=md64a$afibcomb,db=md64a$dbcomb,race=md64a$race),
       data.frame(age=md64b$age,year=y2,sdmi=as.numeric(md64b$smidd),
gender=md64b$gender,hcl=md64b$hclcomb,hyt=md64b$hytcomb,kd=md64b$kdcomb,copd=md64b$copdcomb,cancer=md64b$cancercomb,hf=md64b$hfcomb,anem=md64b$anemcomb,afib=md64b$afibcomb,db=md64b$dbcomb,race=md64b$race)))
 a =summary(coxph(Surv(sdmi, status) ~ age+ factor(year)+gender, data=data))
c(a$coef[2,], a$conf.int[2,])
}


library(survival)
par(mfrow=c(3,3),mar=c(5,4,2,2))

res = NULL
for( i in 87:120) {
for(yy in 2004:2008){
cat(i,yy,"\n")
t=hospfunc(md2,hospn[i],yy,yy+2)
res = rbind(res, c(hn=hospn[i],year=yy, round(t,5)))
}
}

res00 = res[c(482,194,716,440,77,653,340,671,708,131,123,59,314,104,242,590,349,5,737,303,
51,33,536,527,689,609,602,458,753,645,547,115,414,15,222,401,233,392,23,293,257,288,699,186,
358,365,248,500,726,554,383,772,171,149,743,44,509,762,581,68,87,428,179,446),]

#Graph with Coefficients
cd = c(rbind(res00[,3]+ 1.96*res00[,5],res00[,3]-1.96*res00[,5],NA))
barplot(names=factor(res00[,1]),res00[,3],ylim=c(-4,4),cex.names=0.7,col=13,las=2)-> uu
par(mar=c(5,4,1,1))
barplot(res00[,3],ylim=c(-4,4),col=13,las=2,ylab="HR")-> uu
abline(h=0,lwd=3)
uu = c(uu)
axis(1,37,"Hospital",tick=F)
cd1 = c(rbind(uu,uu,NA))
lines(cd1,cd,lwd=1,col=2)
box()

#Graph with HR
cd2 = c(rbind(res00[,10],res00[,11],NA))
barplot(names=factor(res00[,1]),res00[,3],ylim=c(-4,4),cex.names=0.7,col=13,las=2)-> uu
par(mar=c(5,4,1,1))
barplot(res00[,4],ylim=c(0,6),col=13,las=2,ylab="HR")-> uu
abline(h=1,lwd=3)
uu = c(uu)
axis(1,37,"Hospital",tick=F)
cd1 = c(rbind(uu,uu,NA))
lines(cd1,cd2,lwd=1,col=2)
box()

#Merging of X and Res00 for Delta by HR graph
names(x)[6]<-"hn"
resx = merge(x,res00,by="hn")
#Removing of Deltas
resx1 = resx[!is.na(resx$Delta),]
indic = array(NA, dim=c(64,1))
indic[resx1[,24]<0.05]=0
indic[resx1[,24]>=0.05]=1
indic = as.factor(indic)
plot(resx1$Delta, resx1[,21], main="Grade Change by Hazard Ratio", xlab="Grade Change", col=c(2,1)[indic], ylab="HR")

#abline(lsfit(resx1$Delta, resx1[,21]))
cor(resx1$Delta, resx1[,21])
cor.test(resx1$Delta, resx1[,21])#Pearsons Correlation Test##

################################################################################################################################################################################
#Multivariate Regression 
	
ff = function(yy){ 
  
hospfunc = function(md2,hn,y1,y2) {
md64 = md2[md2$FIRSTMIHOSP == hn,]
md64$dt1disc = as.Date(as.character(md64[,2]),"%m/%d/%Y")
md64$dtbirth = as.Date(as.character(md64[,67]),"%m/%d/%Y")
md64$age = as.numeric((md64$dt1disc-md64$dtbirth)/365)
md64$dtdeath = as.Date(as.character(md64[,68]),"%m/%d/%Y")
md64$agedeath = as.numeric((md64$dtdeath-md64$dtbirth)/365)
md64$dt2mi = as.Date(as.character(md64[,9]),"%m/%d/%Y")
md64$censored = (!is.na(md64$dtdeath) & (md64$dtdeath>=md64$dt1disc)) & is.na(md64$dt2mi)

dy64 = substring(md64$dt1disc,1,4)
dy64a = dy64 == as.character(y1)
dy64a[is.na(dy64a)] = F
md64a = md64[dy64a,]
sdd64 = md64a$dt2mi
i1 = is.na(sdd64)
sdd64[i1] = maxdat2
md64a$smi = sdd64
md64a$smidd = as.numeric(md64a$smi-md64a$dt1disc) 

dy64b = dy64 == as.character(y2)
dy64b[is.na(dy64b)] = F
md64b = md64[dy64b,]
sdd64b = md64b$dt2mi
i2 = is.na(sdd64b)
sdd64b[i2] = maxdat2
md64b$smi = sdd64b
md64b$smidd = as.numeric(md64b$smi-md64b$dt1disc) 
#browser()
#boxplot(narm(md64a$smidd), narm(md64b$smidd),names=as.character(c(y1,y2)),sub=hn)
#t.test(narm(md64b$smidd), narm(md64a$smidd))

if( nrow(md64a) ==0 | nrow(md64b)==0) return( rep(NA,9))

data = data.frame(status=1-c(i1,i2),rbind( data.frame(age=md64a$age,year=y1,sdmi=as.numeric(md64a$smidd),
gender=md64a$gender,hcl=md64a$hclcomb, hyt=md64a$hytcomb,kd=md64a$kdcomb,copd=md64a$copdcomb, cancer=md64a$cancercomb,hf=md64a$hfcomb,anem=md64a$anemcomb,afib=md64a$afibcomb,db=md64a$dbcomb,race=md64a$race),
       data.frame(age=md64b$age,year=y2,sdmi=as.numeric(md64b$smidd),
gender=md64b$gender,hcl=md64b$hclcomb,hyt=md64b$hytcomb,kd=md64b$kdcomb,copd=md64b$copdcomb,cancer=md64b$cancercomb,hf=md64b$hfcomb,anem=md64b$anemcomb,afib=md64b$afibcomb,db=md64b$dbcomb,race=md64b$race)))
 a =summary(coxph(Surv(sdmi, status) ~ age+ factor(year)+gender, data=data))
c(a$coef[2,], a$conf.int[2,])
}

library(survival)
hospfunc(md2=md2,hn=hn,yy,yy+2)
 }
=

ff = function(yy){ hospfunc(md2=get(md2,hn=hospn[1],yy,yy+2)) }


ff(hospn[1],2004)


library(parallel)
cl <- makeCluster(getOption("cl.cores", 4))

clusterApply( cl, 2000:2008,ff,md2,hn)

md2