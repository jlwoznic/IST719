# Author: Pan
# Purpose: IST719 WIP
#https://www.kaggle.com/joniarroba/noshowappointments
#http://www.theanalysisfactor.com/r-11-bar-charts/

#load the data
rd <-file.choose()
noshow <- read.csv(rd, header= TRUE, stringsAsFactors = FALSE)

#clean up the data and add agegroup column
noshow<-noshow[noshow$Age>=0, ]
noshow<-noshow[noshow$Age<101, ]
noshow$agegroup<-findInterval(noshow$Age, c(0,5,10,15,20, 25,30, 35,40,45,50,55,60,65,70,75,80,85,90,95,100))


#The gender distributions of all the patients in the data
library(plotrix)
pie3D(table(noshow$Gender),labels=c("Female - 200472- 66.83%","Male - 99495 -33.17% "),col=c("#F72D68","#1E7CC2"
),explode=0.1,main="Pie Chart of Genders ")

#Age group distribution
noshow$agegroup
table(noshow$agegroup)

plot (density(noshow$Age),main="Awaiting Days Distribution",xlab="Patient's Age")
polygon (density (noshow$Age), col = "orange", border = "blue")
mtext (text = "Source: JoniHoppen from kaggle.com", side=1, line=3.5, adj=1,cex=0.6)

hist(noshow$Age,ylim=c(0,30000),main="Distribution of Patient's Age",col = "#8FCAE7",cex.axis=0.8)

#The gender distribution of the patients
barplot(table(noshow$Gender), xlab="Patient's Gender", ylab="Frequency", main="Gender Distribution of the Patients")
mtext (text = "Source: JoniHoppen from kaggle.com", side=1, line=3.5, adj=1.0,cex=0.6)


#Distribution of the day of the week of these patients' appointments, list in the order from Monday to Sunday
temp <- factor(noshow$DayOfTheWeek, levels = c("Monday", "Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
plot(temp, xlab="Day of Week", ylab="Frequency", main="Day of the Week Distribution of the Appointments")
mtext (text = "Which day of the week is the appointment?", side=3, line=0.2, adj=0.5)
mtext (text = "Source: JoniHoppen from kaggle.com", side=1, line=3.5, adj=1,cex=0.6)


######
#show and no show
tmp2<-data.frame(table(noshow$Status))
lbs<-unique(noshow$Status)
tmp2[1,]
pct=paste(round(tmp2[,2]/nrow(noshow),digits=4)*100,"%",sep="")
lbs<-paste(lbs,pct,tmp2[,2])
pie(table(noshow$Status), lbs,main="How many of them have showed up?")


#2-The age and the probability of no-show
#clean up the data with the age<0
par(mfrow=c(1,1))


nrow(noshow)
colnames(noshow)
head(noshow$Age)
head(noshow$agegroup)


sort(unique(noshow$Age))
x<-function(a){
  length(which(a=="No-Show"))
}
AgeNoshow2<-tapply(noshow$Status,noshow$agegroup,x)
head(noshow)
AgeNoshowData2<-as.data.frame(AgeNoshow2)
AgeNoshowData22<-as.data.frame(table(noshow$agegroup))
colnames(AgeNoshowData22)<- c("Age","Appointments")
AgeNoshowData22$NoShow<-AgeNoshowData2$AgeNoshow2
AgeNoshowData22$NoShowProb<-AgeNoshowData22$NoShow/AgeNoshowData22$Appointments

barplot(AgeNoshowData22$NoShowProb, xlab = "patient's age", ylab="prob of them not showing up",main="age and no-show",names.arg=c("0-5","6-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50","51-55","56-60","61-65","66-70","71-75","76-80","81-85","86-90","91-95","96-100","101-105"))

#####Gender Age Plot#######################
#For Male
MaleSubset<-noshow[noshow$Gender=="M",]
MaleAgeNoshow<-tapply(MaleSubset$Status,MaleSubset$Age,x)
MaleAgeNoshowData1<-as.data.frame(MaleAgeNoshow)
MaleAgeNoshowData<-as.data.frame(table(MaleSubset$Age))
colnames(MaleAgeNoshowData)<- c("Age","Appointments")
MaleAgeNoshowData$NoShow<-MaleAgeNoshowData1$MaleAgeNoshow
MaleAgeNoshowData$NoShowProb<-MaleAgeNoshowData$NoShow/MaleAgeNoshowData$Appointments
rownames(MaleAgeNoshowData) <- MaleAgeNoshowData[,1]
MaleAgeNoshowData<-MaleAgeNoshowData[,-1]
barplot(MaleAgeNoshowData$NoShowProb, xlab = "patient's age", ylab="prob of them not showing up",main="age and no-show",ylim=c(0,0.5),col="#1E7CC2")

par(mfrow=c(2,1))
AgeNoshow2<-tapply(MaleSubset$Status,MaleSubset$agegroup,x)
AgeNoshowData2<-as.data.frame(AgeNoshow2)
AgeNoshowData22<-as.data.frame(table(MaleSubset$agegroup))
colnames(AgeNoshowData22)<- c("Age","Appointments")
AgeNoshowData22$NoShow<-AgeNoshowData2$AgeNoshow2
AgeNoshowData22$NoShowProb<-AgeNoshowData22$NoShow/AgeNoshowData22$Appointments
barplot(AgeNoshowData22$NoShowProb, xlab = "patient's age", ylab="prob of them not showing up",main="age and no-show",names.arg=c("0-5","6-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50","51-55","56-60","61-65","66-70","71-75","76-80","81-85","86-90","91-95","96-100","101-105"),col="#1E7CC2",ylim=c(0,0.5))
FemaleSubset<-noshow[noshow$Gender=="F",]
AgeNoshow3<-tapply(FemaleSubset$Status,FemaleSubset$agegroup,x)
AgeNoshowData3<-as.data.frame(AgeNoshow3)
AgeNoshowData33<-as.data.frame(table(FemaleSubset$agegroup))
colnames(AgeNoshowData33)<- c("Age","Appointments")
AgeNoshowData33$NoShow<-AgeNoshowData3$AgeNoshow3
AgeNoshowData33$NoShowProb<-AgeNoshowData33$NoShow/AgeNoshowData33$Appointments
barplot(AgeNoshowData33$NoShowProb, xlab = "patient's age", ylab="prob of them not showing up",main="age and no-show",names.arg=c("0-5","6-10","11-15","16-20","21-25","26-30","31-35","36-40","41-45","46-50","51-55","56-60","61-65","66-70","71-75","76-80","81-85","86-90","91-95","96-100","101-105"),col="#F72D68",ylim=c(0,0.5))



AgeNoshow<-tapply(noshow$Status,noshow$Age,x)
AgeNoshowData1<-as.data.frame(AgeNoshow)
AgeNoshowData<-as.data.frame(table(noshow$Age))
colnames(AgeNoshowData)<- c("Age","Appointments")
AgeNoshowData$NoShow<-AgeNoshowData1$AgeNoshow
AgeNoshowData$NoShowProb<-AgeNoshowData$NoShow/AgeNoshowData$Appointments
rownames(AgeNoshowData) <- AgeNoshowData[,1]
AgeNoshowData<-AgeNoshowData[,-1]

head(noshow$Age)

barplot(AgeNoshowData$NoShowProb, xlab = "patient's age", ylab="prob of them not showing up",main="age and no-show")


lines(AgeNoshowData$NoShowProb,col="green")



plot(AgeNoshowData$NoShowProb,type="h",xlab = "patient's age", ylab="probability of not showing up",main="age and no-show")
lines(AgeNoshowData$NoShowProb,col="green")



?barplot
#maybe i could use a density plot?

#gender?

GenderNoshow<-as.data.frame(tapply(noshow$Status,noshow$Gender,x))
GenderNoshowData<-as.data.frame(table(noshow$Gender))
GenderNoshowData$NoShow<-GenderNoshow[,1]
GenderNoshowData$NoShowProb<-GenderNoshowData$NoShow/GenderNoshowData$Freq
GenderNoshowData1 <- GenderNoshowData[c(2,1),]
barplot(GenderNoshowData1$NoShowProb, main="Gender and no-show")



#Day of week and no-show
DayNoshow<-as.data.frame(tapply(noshow$Status,noshow$DayOfTheWeek,x))
DayNoshowData<-as.data.frame(table(noshow$Day))
DayNoshowData$NoShow<-DayNoshow$`tapply(noshow$Status, noshow$DayOfTheWeek, x)`
DayNoshowData$NoShowProb<-DayNoshowData$NoShow/DayNoshowData$Freq
DayNoshowData1 <- DayNoshowData[c(2,6,7,5,1,3,4),]
barplot(DayNoshowData1$NoShowProb, main="day of the week and no-show")


#why 0 and 1 on x axis?!

#SMS and no-show
noshowsms<-noshow[noshow$Sms_Reminder<2,]
SMSNoshow<-as.data.frame(tapply(noshowsms$Status,noshowsms$Sms_Reminder,x))
SMSNoshowData<-as.data.frame(table(noshowsms$Sms_Reminder))
SMSNoshowData$NoShow<-SMSNoshow$`tapply(noshowsms$Status, noshowsms$Sms_Reminder, x)`
SMSNoshowData$NoShowProb<-SMSNoshowData$NoShow/SMSNoshowData$Freq
SMSNoshowData$Var1<-c("without text reminder", "with text reminder")
barplot(SMSNoshowData$NoShowProb, main="Text message?")


#2-overall graph that show the noshow rate of people with Diabetes, alcoholism, handcap, hypertension, tubercolosis smokes, text message

par(mfrow=c(3,2))


#Handicap?
HandicapNoshow<-as.data.frame(tapply(noshow$Status,noshow$Handcap,x))
HandicapNoshowData<-as.data.frame(table(noshow$Handcap))
HandicapNoshowData$NoShow<-HandicapNoshow[,1]
HandicapNoshowData$NoShowProb<-HandicapNoshowData$NoShow/HandicapNoshowData$Freq
HandicapNoshowData<-HandicapNoshowData[c(1:2),]
HandicapNoshowData$NoShowProb<-round(HandicapNoshowData$NoShowProb,4)
bp1<-barplot(HandicapNoshowData$NoShowProb, main="Handicap?",ylim=c(0,0.4))
text(x = bp1, y = HandicapNoshowData$NoShowProb, label = HandicapNoshowData$NoShowProb, pos = 3, cex = 0.5, col = "red")


#diabetes
DiabetesNoshow<-as.data.frame(tapply(noshow$Status,noshow$Diabetes,x))
DiabetesNoshowData<-as.data.frame(table(noshow$Diabetes))
DiabetesNoshowData$NoShow<-DiabetesNoshow$`tapply(noshow$Status, noshow$Diabetes, x)`
DiabetesNoshowData$NoShowProb<-DiabetesNoshowData$NoShow/DiabetesNoshowData$Freq
DiabetesNoshowData$NoShowProb<-round(DiabetesNoshowData$NoShowProb,4)
bp2<-barplot(DiabetesNoshowData$NoShowProb, main="Diabetes?",ylim=c(0,0.4))
text(x = bp2, y = DiabetesNoshowData$NoShowProb, label = DiabetesNoshowData$NoShowProb, pos = 3, cex = 0.5, col = "red")

#alcoholism?
AlcoholNoshow<-as.data.frame(tapply(noshow$Status,noshow$Alcoolism,x))
AlcoholNoshowData<-as.data.frame(table(noshow$Alcoolism))
AlcoholNoshowData$NoShow<-AlcoholNoshow$`tapply(noshow$Status, noshow$Alcoolism, x)`
AlcoholNoshowData$NoShowProb<-AlcoholNoshowData$NoShow/AlcoholNoshowData$Freq
AlcoholNoshowData$NoShowProb<-round(AlcoholNoshowData$NoShowProb,4)
bp3<-barplot(AlcoholNoshowData$NoShowProb, main="Alcoholism?",ylim=c(0,0.4))
text(x = bp3, y = AlcoholNoshowData$NoShowProb, label = AlcoholNoshowData$NoShowProb, pos = 3, cex = 0.5, col = "red")


#Hypertension
HTNoshow<-as.data.frame(tapply(noshow$Status,noshow$HiperTension,x))
HTNoshowData<-as.data.frame(table(noshow$HiperTension))
HTNoshowData$NoShow<-HTNoshow$`tapply(noshow$Status, noshow$HiperTension, x)`
HTNoshowData$NoShowProb<-HTNoshowData$NoShow/HTNoshowData$Freq
HTNoshowData$NoShowProb<-round(HTNoshowData$NoShowProb,4)
bp4<-barplot(HTNoshowData$NoShowProb, main="Hypertension?",ylim=c(0,0.4))
text(x = bp4, y = HTNoshowData$NoShowProb, label = HTNoshowData$NoShowProb, pos = 3, cex = 0.5, col = "red")


#TB?
TBNoshow<-as.data.frame(tapply(noshow$Status,noshow$Tuberculosis,x))
TBNoshowData<-as.data.frame(table(noshow$Tuberculosis))
TBNoshowData$NoShow<-TBNoshow$`tapply(noshow$Status, noshow$Tuberculosis, x)`
TBNoshowData$NoShowProb<-TBNoshowData$NoShow/TBNoshowData$Freq
TBNoshowData$NoShowProb<-round(TBNoshowData$NoShowProb,4)
bp5<-barplot(TBNoshowData$NoShowProb, main="Tuberculosis?",ylim=c(0,0.4))
text(x = bp5, y = TBNoshowData$NoShowProb, label = TBNoshowData$NoShowProb, pos = 3, cex = 0.5, col = "red")

#Smoking
SmokingNoshow<-as.data.frame(tapply(noshow$Status,noshow$Smokes,x))
SmokingNoshowData<-as.data.frame(table(noshow$Smokes))
SmokingNoshowData$NoShow<-SmokingNoshow$`tapply(noshow$Status, noshow$Smokes, x)`
SmokingNoshowData$NoShowProb<-SmokingNoshowData$NoShow/SmokingNoshowData$Freq
SmokingNoshowData$NoShowProb<-round(SmokingNoshowData$NoShowProb,4)
bp6<-barplot(SmokingNoshowData$NoShowProb, main="Smoking?",ylim=c(0,0.4))
text(x = bp6, y = SmokingNoshowData$NoShowProb, label = SmokingNoshowData$NoShowProb, pos = 3, cex = 0.5, col = "red")




##########################HW4####################
#The age distributions of all the patients in the data
boxplot(noshow$Age, ylab="patient's age", main="Distribution of All Patients' Age")
mtext (text = "Source: JoniHoppen from kaggle.com", side=1, line=1.5, adj=1,cex=0.6)



#The gender distribution of the patients
barplot(table(noshow$Gender), xlab="Patient's Gender", ylab="Frequency", main="Gender Distribution of the Patients")
mtext (text = "Source: JoniHoppen from kaggle.com", side=1, line=3.5, adj=1.0,cex=0.6)


# legend(13, 12, c("Label1","Label2","Label3","Label4","Label5"), cex=0.6, 
#        fill=terrain.colors(5))

#Distribution of whether the patient showed up to the appointment
barplot(table(noshow$Status), xlab="Status", ylab="Frequency", main="Status of patient's appointment")
mtext (text = "Did the patients show up?", side=3, line=0, adj=0.5)
mtext (text = "Source: JoniHoppen from kaggle.com", side=1, line=3.5, adj=1.1,cex=0.6)


#Distribution of days a patient has to wait for the appointment
noshow$AwaitingTime1<-noshow$AwaitingTime*(-1)

plot (density(noshow$AwaitingTime1),main="Awaiting Days Distribution",xlab="Number of Days",xlim=c(0,60))
polygon (density (noshow$AwaitingTime1), col = "orange", border = "blue")
mtext (text = "How many days did the patient have to wait for the appointment?", side=3, line=0, adj=0.5)
mtext (text = "Source: JoniHoppen from kaggle.com", side=1, line=3.5, adj=1,cex=0.6)
# 
# hist(noshow$AwaitingTime1,xlim=c(0,80),breaks=200, main="Awaiting Days Distribution",xlab="Number of Days")
# mtext (text = "How many days did the patient have to wait for the appointment?", side=3, line=0, adj=0.5)
# mtext (text = "Source: JoniHoppen from kaggle.com", side=1, line=3.5, adj=1.1,cex=0.6)




#radar chart for week and day
library(fmsb)
#Day of week and no-show
DayNoshow<-as.data.frame(tapply(noshow$Status,noshow$DayOfTheWeek,x))
DayNoshowData<-as.data.frame(table(noshow$Day))
DayNoshowData$NoShow<-DayNoshow$`tapply(noshow$Status, noshow$DayOfTheWeek, x)`
DayNoshowData$NoShowProb<-DayNoshowData$NoShow/DayNoshowData$Freq
DayNoshowData1 <- DayNoshowData[c(2,6,7,5,1,3,4),]
barplot(DayNoshowData1$NoShowProb, main="day of the week and no-show")
DayNoshowData1$NoShowProb<-as.vector(DayNoshowData1$NoShowProb)
DayNoshowData1$scaledNoShowProb<-rescale(DayNoshowData1$NoShowProb)
DayNoshowData1<-DayNoshowData1[1:6,]
#radar chart
par(mfrow=c(1,1))
radardata<-as.data.frame(matrix(DayNoshowData1$NoShowProb
, ncol=6))
colnames(radardata)=DayNoshowData1$Var1
radardata<-round(radardata,3)
radardata=rbind(rep(0.368,6) , rep(0.1,6) , radardata)
# The default radar chart proposed by the library:
radarchart(radardata, seg=5,pcol="#0039A6",pfcol="#00A1DE90",plwd=3,cglcol="#de4600",cglty=1, axislabcol="#de0f00", cglwd=1,vlcex=0.8,centerzero=TRUE,title="Day of the Week of the Appointment")
legend("topright",legend=c("Pct. of patients \n not showing up"),pch=15,col="#00A1DE90")
