#Lauren Palladino Olson

setwd("~/Desktop/Olson, Huffman, Gray")

library(readr)
library(dplyr)
library(haven)
library(stargazer)
library(ggplot2)
library(effects)
library(sjPlot)
library(gridExtra)
library(gridGraphics)

###NORC DATA ANALYSIS###

tess<- read_dta("NORC_Clean_Data.dta")

tess<-tess%>%
  dplyr:: rename(Legit_Trust= HUFFMAN1, Legit_Fair=HUFFMAN2, Strong_Exec= HUFFMAN3A, 
                 Pres_No_Others= HUFFMAN3B, Checks_Balances= HUFFMAN3C, treatment= DOV_EXP_HUFFMAN,
                 gender=GENDER, race=RACETHNICITY, educ=EDUC5)

#recoding
tess$treatment<-NA
tess$treatment[DOV_EXP_HUFFMAN==1]<-"Control"
tess$treatment[DOV_EXP_HUFFMAN==2]<-"Diverse"

tess$pid<-NA
tess$pid[PartyID5<=2 & PartyID5>0]<-1
tess$pid[PartyID5==3]<-2
tess$pid[tess$PartyID5>=4]<-3

tess$female<-NA
tess$female[GENDER==2]<-1
tess$female[GENDER==1]<-0
table(tess$female)

tess$race<-as.factor(NA)
levels(tess$race)<-c("White", "Asian", "Black", "Hispanic", "Other")
tess$race[RACETHNICITY==1]<-"White"
tess$race[RACETHNICITY==6]<-"Asian"
tess$race[RACETHNICITY==2]<-"Black"
tess$race[RACETHNICITY==4]<-"Hispanic"
tess$race[RACETHNICITY==3]<-"Other"
tess$race[RACETHNICITY==5]<-"Other"

tess$educ<-NA
tess$educ[EDUC5==1]<-1
tess$educ[EDUC5==2]<-2
tess$educ[EDUC5==3]<-3
tess$educ[EDUC5==4]<-4
tess$educ[EDUC5==5]<-5

tess<-tess%>%
  mutate_at( c("Legit_Fair", "Legit_Trust", "Strong_Exec", "Pres_No_Others", "Checks_Balances"),
             car::recode, "1 = 1; 2 = 3/4; 3 = 2/4; 4=1/4; 5=0; 77= NA; 98=NA; 99= NA")

#Legitimacy
Trust1<-lm(Legit_Trust~treatment*PartyID5+female+race+educ, data=tess)
Fair1<-lm(Legit_Fair~treatment*PartyID5+female+race+educ, data=tess)
trust<-tess$Legit_Trust/2
fair<-tess$Legit_Fair/2
tess$legit0<-(trust+fair)
aggregate(legit0 ~ treatment, data = tess, FUN = mean)

Legit<-lm(legit0~ treatment+PartyID5+ female+ race+ educ, data=tess)
Legit2<-lm(legit0~ treatment*PartyID5+ female+ race+ educ, data=tess)

#Democratic norms regression
SE2<-lm(Strong_Exec~treatment*PartyID5+female+race+educ, data=tess)
PNO2<-lm(Pres_No_Others~treatment*PartyID5+female+race+educ, data=tess)
CB2<-lm(Checks_Balances~treatment*PartyID5+female+race+educ, data=tess)

se<-tess$Strong_Exec/3
pno<-tess$Pres_No_Others/3
cb<-tess$Checks_Balances/3
tess$demnorms0<-(se+pno+cb)
aggregate(demnorms0 ~ treatment, data = tess, FUN = mean)

DemNorms<-lm(demnorms0~ treatment+PartyID5+ female+ race+ educ, data=tess)
DemNorms2<-lm(demnorms0~ treatment*PartyID5+ female+ race+ educ, data=tess)

stargazer(Legit, Legit2, DemNorms, DemNorms2, type="text", report=('vc*p'), omit.table.layout = "n")

###YOUGOV DATA ANALYSIS###

ygdata<-read_csv("YouGov_Clean_Data.csv")

#recoding
ygdata$female1<-NA
ygdata$female1[ygdata$female=="Female"]<-1
ygdata$female1[ygdata$female=="Male"]<-0

ygdata$educ1<-NA
ygdata$educ1[ygdata$educ=="Less than a High School Diploma"]<-1
ygdata$educ1[ygdata$educ=="High School Graduate"]<-2
ygdata$educ1[ygdata$educ=="Some College"]<-3
ygdata$educ1[ygdata$educ=="2-year College Degree"]<-4
ygdata$educ1[ygdata$educ=="4-year College Degree"]<-5
ygdata$educ1[ygdata$educ=="Post-Graduate Degree"]<-6

ygdata$newsint1<-NA
ygdata$newsint1[ygdata$newsint=="None"]<-1
ygdata$newsint1[ygdata$newsint=="Low"]<-2
ygdata$newsint1[ygdata$newsint=="Medium"]<-3
ygdata$newsint1[ygdata$newsint=="High"]<-4

ygdata$age1<-NA
ygdata$age1[ygdata$age=="18-29"]<-1
ygdata$age1[ygdata$age=="30-44"]<-2
ygdata$age1[ygdata$age=="45-64"]<-3
ygdata$age1[ygdata$age=="65+"]<-4

ygdata$pid3<-as.factor(NA)
levels(ygdata$pid3)<-c("Democrat", "Independent", "Republican")
ygdata$pid3[ygdata$pid7==1]<-"Democrat"
ygdata$pid3[ygdata$pid7==2]<-"Democrat"
ygdata$pid3[ygdata$pid7==3]<-"Democrat"
ygdata$pid3[ygdata$pid7==4]<-"Independent"
ygdata$pid3[ygdata$pid7==5]<-"Republican"
ygdata$pid3[ygdata$pid7==6]<-"Republican"
ygdata$pid3[ygdata$pid7==7]<-"Republican"

ygdata$treatment1<-as.factor(ygdata$treatment)

ygdata$Race<-as.factor(NA)
levels(ygdata$Race)<-c("White", "Asian", "Black", "Hispanic", "Other")
ygdata$Race[ygdata$race=="White"]<- "White"
ygdata$Race[ygdata$race=="Asian"]<-"Asian"
ygdata$Race[ygdata$race=="Black"]<-"Black"
ygdata$Race[ygdata$race=="Hispanic"]<-"Hispanic"
ygdata$Race[ygdata$race=="Multiracial"]<-"Other"
ygdata$Race[ygdata$race=="Native American"]<-"Other"
ygdata$Race[ygdata$race=="Middle Eastern"]<-"Other"
ygdata$Race[ygdata$race=="Other"]<-"Other"

#legitimacy
Trust1<-lm(Legit_Trust~ treatment1+ pid7+ female1+ Race+ educ1+ newsint1+ age1+ treatment1*pid7, data=ygdata)
summary(Trust1)
Fair1<-lm(Legit_Fair~ treatment1+ pid7+ female1+ Race+ educ1+ newsint1+ age1+ treatment1*pid7, data=ygdata)
summary(Fair1)
stargazer(Trust1, Fair1, type="text")

#aggregate democratic legitimacy
trust<-Legit_Trust/2
fair<-Legit_Fair/2
ygdata$legit<-(trust+fair)

aggregate(legit ~ treatment, data = ygdata, FUN = mean)

ygdata$pairwise1<-as.factor(NA)
levels(ygdata$pairwise1)<-c("Control", "Diverse")
ygdata$pairwise1[ygdata$treatment=="Control"]<-"Control"
ygdata$pairwise1[ygdata$treatment=="Diverse"]<-"Diverse"

ygdata$pairwise2<-as.factor(NA)
levels(ygdata$pairwise2)<-c("Control", "Diverse But Critiqued")
ygdata$pairwise2[ygdata$treatment=="Control"]<-"Control"
ygdata$pairwise2[ygdata$treatment=="Diverse But Critiqued"]<-"Diverse But Critiqued"

t.test(legit~pairwise1, data=ygdata)
t.test(legit~pairwise2, data=ygdata)

Legit1<-lm(legit~ treatment1+ pid7+ female1+ Race+ educ1+ age1+ treatment1*pid7, data=ygdata)
Legit2<-lm(legit~ treatment1+ pid7+ female1+ Race+ educ1+ age1, data=ygdata)

#democratic norms
SE1<-lm(Strong_Exec~treatment1+ pid7+ female1+ Race+ educ1+ newsint1+ age1+ treatment1*pid7, data=ygdata)
summary(SE1)
PNO1<-lm(Pres_No_Others~treatment1+ pid7+ female1+ Race+ educ1+ age1+ treatment1*pid7, data=ygdata)
summary(PNO1)
CB1<-lm(Checks_Balances~treatment1+ pid7+ female1+ Race+ educ1+ age1+ treatment1*pid7, data=ygdata)
summary(CB1)
stargazer(SE1, PNO1, CB1, type="text")

#aggregate democratic norms
exec<-Strong_Exec/3
pres<-Pres_No_Others/3
checks<-Checks_Balances/3
ygdata$demnorms<-(exec+pres+checks)

t.test(demnorms~pairwise1, data=ygdata)
t.test(demnorms~pairwise2, data=ygdata)

DN2<-lm(demnorms~ treatment1+ pid7+ female1+ Race+ educ1+ age1, data=ygdata)
DN1<-lm(demnorms~ treatment1+ pid7+ female1+ Race+ educ1+ age1+ treatment1*pid7, data=ygdata)

stargazer(Legit2, Legit1, DN2, DN1, type="text", report=('vc*p'), omit.table.layout = "n")

#graphs
ggplot(ygdata%>%filter(pid3!= "Not Sure"), aes(x = treatment, y = legit, color= pid3)) + 
  stat_summary(fun.y=mean, geom="point", position = position_dodge(0.9), cex=3)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2, position= position_dodge(0.9), lwd=1)+
  scale_color_grey(labels = c("Democrat", "Independent","Republican"))+
  labs(x= "Treatment", y= "Perceived Legitimacy", color= "Respondent Party")+ 
  theme_bw(base_size=15)+theme(axis.text.x = element_text(size = 19), text=element_text(size=21),
                               legend.position="bottom")
  

ggplot(ygdata%>%filter(pid3!= "Not Sure"), aes(x = treatment1, y = demnorms, color= pid3)) + 
  stat_summary(fun.y=mean, geom="point", position = position_dodge(0.9), cex=3)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2, position= position_dodge(0.9), lwd=1)+
  scale_color_grey(labels = c("Democrat", "Independent","Republican"))+
  labs(x= "Treatment", y= "Support for Democratic Norms Constraining Executive Power", color= "Respondent Party")+ 
  theme_bw(base_size = 15)+theme(axis.text.x = element_text(size = 19), text=element_text(size=21),
                                 legend.position="bottom")

p1<- ggplot(ygdata, aes(x = treatment, y = legit, color= female)) + 
  stat_summary(fun.y=mean, geom="point", position = position_dodge(0.9), cex=3)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2, position= position_dodge(0.9), lwd=1)+
  scale_color_grey(labels = c("Woman", "Man"))+
  labs(x= "Treatment", y= "Perceived Legitimacy", color= "Respondent \n Gender")+  expand_limits(y = c(0.4, 0.7))+  
  theme_bw(base_size = 15)+theme(axis.text.x = element_text(size = 19), text=element_text(size=21),
                                 legend.position="bottom")

p2<- ggplot(ygdata%>%filter(race== "White" | race== "Hispanic" | race== "Black"),
                   aes(x = treatment, y = legit, color= race)) + 
  stat_summary(fun.y=mean, geom="point", position = position_dodge(0.9), cex=3)+
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2, position= position_dodge(0.9), lwd=1)+
  scale_color_grey(labels = c("White", "Hispanic", "Black"))+
  labs(x= "Treatment", y= "Perceived Legitimacy", color= "Respondent \n Race")+  expand_limits(y = c(0.4, 0.7))+ 
  theme_bw(base_size = 15)+theme(axis.text.x = element_text(size = 19), text=element_text(size=21),
                                 legend.position="bottom")

grid.arrange(p1,p2, ncol=2)
