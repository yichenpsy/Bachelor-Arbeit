###   Reinfrocement Aufgabe   ###
# data.r: vollständig recode Daten
# data.r0: exclude vp 055

######consumption trials 
# data.r3: r0 nur consumption trials
# data.r4: r3 gültige consumption trials
# data.r5: r4 7 vp ausgeschlossen 

####### Reinforcement ACC
# data.r1: r0, ohne consumption trials
# data.r6: r1 ,7 Vp aus
# data.r2: r6, NoDev RT= -1 aus

###### Reinforcement RT
# data.r7: r6, alle RT= -1 aus

library(tidyverse) ## Alternative: library(dplyr); library(ggplot2)
library(plotrix) 
library(dplyr)
library(readxl)
library(ez)
library(xlsx)
library(reshape2)
library(effsize) 
rm(list=ls())

##bio daten VP list
setwd("/Users/yichenzhong/Desktop/")
bio.data <- data.frame()
bio.data <- read_excel("bio Daten.xlsx")

bio.ConGen <- bio.data %>%
  group_by(VP,Condition,Gender) %>%
  summarise()

bio.anzahl <-bio.ConGen  %>%
  group_by(Condition,Gender) %>%
  summarise(length(VP))

bio.age<-select(bio.data,Alter)
bio.age1<-subset(bio.age,bio.age$Alter!= "NA")
sd(bio.age1$Alter) #standard Abweichung
summary(bio.age1$Alter) #min, max

sum(bio.data$Student=="j") 
39/53 # Anteil der Studenten


##################
## Vorbereitung ##
##################
# set working directory: Arbeitsverzeichnis setzen (wo liegen die Daten?)
setwd("/Users/yichenzhong/Desktop/BA/resultR/")
# welche files (Dateien) sollen eingelesen werden?
logfiles <- list.files(pattern = "result")
# eine leere Datentabelle anlegen
data.r <- data.frame()
# Daten aller VPs einlesen
for(file in logfiles){
  vplogfile <- read_excel(file)
  data.r <- rbind(data.r,vplogfile)
}

# bio daten Reinforce, wie viel stress, control
bio.data.r <- data.r %>%
  group_by(Condition,VP) %>%
  summarise()
summarise(bio.data.r,n())

### Variablen recode

# Variablen "Dev" hinzufügen: ifelse
#  2,16,29 Blöcke DevH, 3,17,30 DevL, andere NoDev
data.r$Dev <- ifelse(data.r$Block==2|data.r$Block==16|data.r$Block==29,"DevH",
                    ifelse(data.r$Block==3|data.r$Block==17|data.r$Block==30,"DevL","NoDev"))

# Variablen "Dev_jn" 
data.r$Dev_jn <-ifelse(data.r$Block==2|data.r$Block==16|data.r$Block==29|
                         data.r$Block==3|data.r$Block==17|data.r$Block==30,
                      "Dev","NoDev")

## Stimuli
# recode "Stimuli"
data.r$Stimulus <-ifelse((data.r$S_high==1 & data.r$stimulusType=="fractal01")|
                        (data.r$S_high==2 & data.r$stimulusType=="fractal02"),"S_high",
                 ifelse((data.r$S_high==1 & data.r$stimulusType=="fractal02")|
                        (data.r$S_high==2 & data.r$stimulusType=="fractal01"),"S_low",data.r$stimulusType))

########################
####### Rechnen ########
########################

######### Proband exclude
### missed trials rate
ex.MissRate<- data.r %>%   # wie viel Daten sind ungültig
  group_by(Condition,VP) %>% 
  summarise(sum(RT==-1))  ## Vp 055, missed 94 trials

## ±3 SDs from the mean RT
ex.data.gültig<-subset(data.r,data.r$RT!=-1)
ex.uplim = mean(ex.data.gültig$RT) + 3*sd(ex.data.gültig$RT)
ex.uplim
ex.langsam<- ex.data.gültig %>% 
  group_by(VP,Condition,Dev ) %>%
  filter(RT > ex.uplim) %>% 
  summarise(N = n()) # Vp 10 ,15, 55 more than 10% trials zu langsam

ex.schnell<- ex.data.gültig %>% 
  group_by(VP,Condition,Dev) %>%
  filter(RT < 200) %>% 
  summarise(N = n())

### responded with one key more than 90% trials 
ex.oneKey<- data.r %>% 
  group_by(Condition,VP,Choice) %>% 
  summarise(N = n())  # no proband


## Vp 055 ausschlißen
data.r0<-subset(data.r, data.r$VP!=55)

bio.data.r0 <- data.r0 %>%
  group_by(Condition,VP) %>%
  summarise()
summarise(bio.data.r0,n())

######consumption trials ####
# Daten nur Consumption trials
data.r3<- subset(data.r0,stimulusType!="fractal01" & stimulusType!="fractal02" )
by(data.r3$Trial, data.r3$Stimulus,length)
by(data.r3$Trial, data.r3$Condition,length)## check wie viele trials

# ungültige Rate
con.unRate <- data.r3 %>%   
  group_by(Condition,Dev) %>% 
  summarise(unAnzhal=sum(RT==-1),allAnzhal=length(Trial)) 
con.unRate$unRate<-con.unRate$unAnzhal/con.unRate$allAnzhal
con.unRate # wie viel Daten sind ungültig, und die Rate pro Bedingung

sum(con.unRate$unAnzhal)/sum(con.unRate$allAnzhal) # 0.8% prozent ungültig
con.unRate %>%
  group_by(Condition) %>% 
  summarise(sum(unAnzhal)/sum(allAnzhal)) ## Stress Group verpasst weniger Trials

# ungültige Daten ausschlißen
data.r4<-subset(data.r3, RT>0)
# neue Variable"respond1": Anteil der Ohigh Auswahlen
data.r4$response1 <-ifelse(data.r4$Reward_t==100|(data.r4$Dev=="DevH"&data.r4$Reward_t==0),1,0) # NoDev 100 Punkte | DevH 0 Punkte

# Anteil der Ohigh Auswahlen: pro Proband pro Block 
con.meanBlo <- data.r4 %>%
  group_by(VP,Condition,Block,Dev) %>%
  summarise(accBlo=mean((sum(response1==1))/3))
# mean acc pro Proband Pro Bedingungen.
con.meanBedin <- con.meanBlo %>%
  group_by(VP,Condition,Dev) %>%
  summarise(acc=mean(accBlo))

# Dev Check: mean acc pro Proband pro Block only DevH: con.meanBloH<-filter(con.meanBlo,Dev=="DevH")
# write.table(con.meanBloH,file = "/Users/yichenzhong/Desktop/ConsumDevH.csv",sep = ",",row.names = T)
con.meanDevH<-con.meanBlo %>%
  filter(Dev=="DevH") %>%
  group_by(VP,Condition,Dev) %>%
  summarise(acc=mean(accBlo))

con.meanDevH %>%  # Ausreißer: 2 stress, 5 control aus
  subset(acc>0.6) %>%
  summarise()

## 7 Vp ausschlißen
data.r5<-filter(data.r4,VP!=8,VP!=17,VP!=30,VP!=39, 
                VP!=45,VP!=56,VP!=63)
# Anteil der Ohigh Auswahlen: pro Proband pro Block 
con.r5meanBlo <- data.r5 %>%
  group_by(VP,Condition,Block,Dev) %>%
  summarise(accBlo=mean((sum(response1==1))/3))
# Anteil der Ohigh Auswahlen: pro Proband pro Block 
con.r5meanBedin <- con.r5meanBlo %>%
  group_by(VP,Condition,Dev) %>%
  summarise(acc=mean(accBlo))

# ANOVA
# ANOVA zusammen
con.anova <- ezANOVA(data = con.r5meanBedin
                     , dv = acc
                     , wid = VP
                     , within = .(Dev)
                     , between = .(Condition)
)
con.anova

# t-test (Dev Ohigh vs No Dev)
con.r5meanBedin.DevH.NoDev <- filter(con.r5meanBedin,Dev!="DevL")
t.test(acc~Dev,data=con.r5meanBedin.DevH.NoDev,var.equal = TRUE,p.adj="bonferroni")
cohen.d(acc~Dev,data=con.r5meanBedin.DevH.NoDev)

# t-test Dev Ohigh vs Dev Olow
con.r5meanBedin.DevH.DevL <- filter(con.r5meanBedin,Dev!="NoDev")
t.test(acc~Dev,data=con.r5meanBedin.DevH.DevL,var.equal = TRUE)
cohen.d(acc~Dev,data=con.r5meanBedin.DevH.DevL)

# t-test NoDev vs DevL
con.r5meanBedin.NoDev.DevL <- filter(con.r5meanBedin,Dev!="DevH")
t.test(acc~Dev,data=con.r5meanBedin.NoDev.DevL,var.equal = TRUE)
cohen.d(acc~Dev,data=con.r5meanBedin.NoDev.DevL)

#Grafik
con.grafik <- con.r5meanBedin %>%
  group_by(Condition,Dev) %>%
  summarise(acc1=mean(acc),SE=std.error(acc))

ggplot(con.grafik, aes(x = con.grafik$Dev, y = con.grafik$acc1, fill = con.grafik$Condition))+ 
  geom_bar(position = "dodge", stat = "identity", colour = "white", width = 0.6)+
  ylab("Prop Ohigh choices") +
  xlab ("Devaluation") +
  coord_cartesian(ylim=c(0,1)) +
  scale_x_discrete(breaks=c("NoDev", "DevH","DevL"), 
                   labels=c("No Dev", "Dev Ohigh","Dev Olow")) +
  geom_errorbar(aes(ymin=acc1-SE, ymax=acc1+SE), width=.1, position=position_dodge(.6))+
  theme(text=element_text(size=20), axis.text.x=element_text(size=10, colour = "black"),    # Beschriftung x-Achse
        axis.text.y=element_text(size=20, colour = "black"))+
  theme_bw()+
  scale_fill_manual(values=c("#CCCCCC", "#000000"))

######################
### 1.Genauigkeit####
#####################

#0.1 Datenvorbereiten: Daten ohne consumption trials
data.r1<- subset(data.r0,stimulusType=="fractal01"|stimulusType=="fractal02" )
by(data.r1$Trial, data.r1$Stimulus,length) ## check wie viele trials
data.r6<-filter(data.r1,VP!=8,VP!=17,VP!=30,VP!=39,
                VP!=45,VP!=56,VP!=63)

# ungültige Rate
rei.unRate <- data.r6 %>%   
  group_by(Condition,Dev_jn) %>% 
  summarise(unAnzhal=sum(RT==-1),allAnzhal=length(Trial)) 
rei.unRate$unRate<-rei.unRate$unAnzhal/rei.unRate$allAnzhal
rei.unRate # wie viel Daten sind ungültig, und die Rate pro Bedingung

# ungültige Daten von NoDev Block ausschließen
data.r2<-filter(data.r6,!(Dev=="NoDev"& RT==-1)) 

by(data.r2$VP, data.r2$Condition,table)
ezDesign(data.r2,x=VP,y=Dev,row= Stimulus)


# 0.2 rechnen acc für ANOVA
# mean acc pro Proband pro Block x2(Stimulus high/low)
rei.meanAccBlo <- data.r2 %>%
  group_by(VP,Condition,Block,Stimulus,Dev) %>%
  summarise(accBlo=mean(Response))
# mean acc pro Proband Pro Bedingungen.
rei.meanAccBedin <- rei.meanAccBlo  %>%
  group_by(VP,Condition,Stimulus,Dev) %>%
  summarise(accBedin=mean(accBlo))

#write.table(rei.meanAccBlo,file = "/Users/yichenzhong/Desktop/AccMean.csv",sep = ",",row.names = T)
##1.1 ANOVA rechen
rei.anovaAcc <- ezANOVA(data = rei.meanAccBedin
                      , dv = accBedin
                      , wid = VP
                      , within = .(Stimulus,Dev)
                      , between = .(Condition)
)
rei.anovaAcc

## main effect of Dev
# No Dev vs Ohigh
rei.meanAccBedin.Noh <- rei.meanAccBlo  %>%
  filter(Dev!="DevL")%>%
  group_by(VP,Condition,Dev) %>%
  summarise(acc=mean(accBlo))
t.test(acc~Dev,data=rei.meanAccBedin.Noh,var.equal = TRUE)
cohen.d(acc~Dev,data=rei.meanAccBedin.Noh)

# No Dev vs Olow
rei.meanAccBedin.Nol <- rei.meanAccBlo  %>%
  filter(Dev!="DevH")%>%
  group_by(VP,Condition,Dev) %>%
  summarise(acc=mean(accBlo))
t.test(acc~Dev,data=rei.meanAccBedin.Nol,var.equal = TRUE)
cohen.d(acc~Dev,data=rei.meanAccBedin.Nol)

#  Dev Ohigh vs Olow
rei.meanAccBedin.hl <- rei.meanAccBlo  %>%
  filter(Dev!="NoDev")%>%
  group_by(VP,Condition,Dev) %>%
  summarise(acc=mean(accBlo))
t.test(acc~Dev,data=rei.meanAccBedin.hl,var.equal = TRUE)
cohen.d(acc~Dev,data=rei.meanAccBedin.hl)

## Stimulus Interaktion
#  NoDev, Shigh vs Slow
rei.meanAccBedin.No<-filter(rei.meanAccBedin,Dev=="NoDev")
t.test(accBedin~Stimulus,data=rei.meanAccBedin.No,var.equal = TRUE)
cohen.d(accBedin~Stimulus,data=rei.meanAccBedin.No)

#  Dev High, Shigh vs Slow*
rei.meanAccBedin.DevH<-filter(rei.meanAccBedin,Dev=="DevH")
t.test(accBedin~Stimulus,data=rei.meanAccBedin.DevH,var.equal = TRUE)
cohen.d(accBedin~Stimulus,data=rei.meanAccBedin.DevH)

#  Dev High, Shigh vs Slow*
rei.meanAccBedin.DevL<-filter(rei.meanAccBedin,Dev=="DevL")
t.test(accBedin~Stimulus,data=rei.meanAccBedin.DevL,var.equal = TRUE)
cohen.d(accBedin~Stimulus,data=rei.meanAccBedin.DevL)

##1.2 Grafik
rei.grafikAcc <- rei.meanAccBedin %>% 
  group_by(Condition,Dev,Stimulus) %>%
  summarise(acc1=mean(accBedin),SE=std.error(accBedin))
#1.2.1 Grafik für control gruppe
rei.grafikAccKontrol <- subset(rei.grafikAcc,Condition=="Control")
ggplot(rei.grafikAccKontrol, aes(x = rei.grafikAccKontrol$Dev, y = rei.grafikAccKontrol$acc1, fill = rei.grafikAccKontrol$Stimulus))+ 
  geom_bar(stat = "identity", position = position_dodge(), colour = "white", width = 0.6)+
  ylab("prop correct responses (control)") +
  xlab ("Devaluation") +
  coord_cartesian(ylim=c(0,1)) +
  scale_x_discrete(breaks=c("NoDev", "DevH", "DevL"), 
                   labels=c("No Dev", "Dev High","Dev Low")) +
  scale_fill_discrete(name="Stimulus",
                      breaks=c("S_high", "S_low"),
                      labels=c("S high", "S low")) + 
  geom_errorbar(aes(ymin=acc1-SE, ymax=acc1+SE), width=.1, position=position_dodge(.6))+
  theme(text=element_text(size=50), axis.text.x=element_text(size=30, colour = "black"),    # Beschriftung x-Achse
        axis.text.y=element_text(size=50, colour = "black"))+
  theme_bw()+ ## Background white
  scale_fill_manual(values=c("#006600","#CC3300")) ## Farbe ändern

# 1.2.2 Grafik für stress
rei.grafikAccStress <- subset(rei.grafikAcc,Condition=="Stress")
ggplot(rei.grafikAccStress, aes(x = rei.grafikAccStress$Dev, y = rei.grafikAccStress$acc1, fill = rei.grafikAccStress$Stimulus))+ 
  geom_bar(stat = "identity", position = position_dodge(), colour = "white", width = 0.6)+
  ylab("prop correct responses (stress)") +
  xlab ("Devaluation") +
  coord_cartesian(ylim=c(0,1)) +
  scale_x_discrete(breaks=c("NoDev", "DevH", "DevL"), 
                   labels=c("No Dev", "Dev High","Dev Low")) +
  scale_fill_discrete(name="Stimulus",
                      breaks=c("S_high", "S_low"),
                      labels=c("S high", "S low")) + 
  geom_errorbar(aes(ymin=acc1-SE, ymax=acc1+SE), width=.1, position=position_dodge(.6))+
  theme(text=element_text(size=20), axis.text.x=element_text(size=10, colour = "black"),    # Beschriftung x-Achse
        axis.text.y=element_text(size=20, colour = "black"))+
  theme_bw()+ ## Background white
  scale_fill_manual(values=c("#006600","#CC3300"))

#####################
# Zeit Effekt rechnen
zeit.Dev<-filter(rei.meanAccBlo,Dev != "NoDev"&
                   ((Dev=="DevH" & Stimulus=="S_high")|(Dev=="DevL"& Stimulus=="S_low")))
# "block" nominalskalieren
zeit.Dev$Block<-as.factor(zeit.Dev$Block) 
# time 1,2,3
zeit.Dev$time<-ifelse((zeit.Dev$Block=="2"|zeit.Dev$Block=="3"),"1",
                       ifelse((zeit.Dev$Block=="29"|zeit.Dev$Block=="30"),"3","2")) 

zeit.anova <- ezANOVA(data = zeit.Dev
                      , dv = accBlo
                      , wid = VP
                      , within = .(Block)
                      , between = .(Condition)
)
zeit.anova


# time main effect Follow Up Test
# Block 2,3  t = 1.2594, df = 88, p-value = 0.2112
zeit.2_3<-filter(zeit.Dev,Block=="2"|Block=="3")
t.test(accBlo~Block,data=zeit.2_3,var.equal=TRUE)
# Block 2,16 t = 1.7214, df = 88, p-value = 0.08869
zeit.2_16<-filter(zeit.Dev,Block=="2"|Block=="16")
t.test(accBlo~Block,data=zeit.2_16,var.equal=TRUE)
# Block 2,17    t = 2.0228, df = 88, p-value = 0.04613*
zeit.2_17<-filter(zeit.Dev,Block=="2"|Block=="17")
t.test(accBlo~Block,data=zeit.2_17,var.equal=TRUE)
cohen.d(accBlo~Block,data=zeit.2_17,na.rm=FALSE) #?
# Block 2,29     t = 2.8526, df = 88, p-value = 0.005403*
zeit.2_29<-filter(zeit.Dev,Block=="2"|Block=="29")
t.test(accBlo~Block,data=zeit.2_29,var.equal=TRUE)
# Block 2,30 t = 1.8258, df = 88, p-value = 0.07128
zeit.2_30<-filter(zeit.Dev,Block=="2"|Block=="30")
t.test(accBlo~Block,data=zeit.2_30,var.equal=TRUE)

# Block 3,16 t = 0.41523, df = 88, p-value = 0.679
zeit.3_16<-filter(zeit.Dev,Block=="3"|Block=="16")
t.test(accBlo~Block,data=zeit.3_16,var.equal=TRUE)
# Block 3,17 t = 0.66261, df = 88, p-value = 0.5093
zeit.3_17<-filter(zeit.Dev,Block=="3"|Block=="17")
t.test(accBlo~Block,data=zeit.3_17,var.equal=TRUE)
# Block 3,29 t = 1.5656, df = 88, p-value = 0.121
zeit.3_29<-filter(zeit.Dev,Block=="3"|Block=="29")
t.test(accBlo~Block,data=zeit.3_29,var.equal=TRUE)
# Block 3,30 t = 0.54499, df = 88, p-value = 0.5871
zeit.3_30<-filter(zeit.Dev,Block=="3"|Block=="30")
t.test(accBlo~Block,data=zeit.3_30,var.equal=TRUE)

# Block 16,17 t = 0.23978, df = 88, p-value = 0.8111
zeit.16_17<-filter(zeit.Dev,Block=="16"|Block=="17")
t.test(accBlo~Block,data=zeit.16_17,var.equal=TRUE)
# Block 16,29 t = 1.223, df = 88, p-value = 0.2246
zeit.16_29<-filter(zeit.Dev,Block=="16"|Block=="29")
t.test(accBlo~Block,data=zeit.16_29,var.equal=TRUE)
# Block 16,30 t = 0.14605, df = 88, p-value = 0.8842
zeit.16_30<-filter(zeit.Dev,Block=="16"|Block=="30")
t.test(accBlo~Block,data=zeit.16_30,var.equal=TRUE)

# Block 17,29 t = 1.0706, df = 88, p-value = 0.2873
zeit.17_29<-filter(zeit.Dev,Block=="17"|Block=="29")
t.test(accBlo~Block,data=zeit.17_29,var.equal=TRUE)
# Block 17,30 t = -0.078274, df = 88, p-value = 0.9378
zeit.17_30<-filter(zeit.Dev,Block=="17"|Block=="30")
t.test(accBlo~Block,data=zeit.17_30,var.equal=TRUE)

# Block 29,30 t = -1.0437, df = 88, p-value = 0.2995
zeit.29_30<-filter(zeit.Dev,Block=="29"|Block=="30")
t.test(accBlo~Block,data=zeit.29_30,var.equal=TRUE)


#interaction Follow Up test 
# block2
zeit.block2<-filter(zeit.Dev,Block=="2")
t.test(accBlo~Condition,data=zeit.block2,var.equal=TRUE)
p.adjust(0.03147, method = "bonferroni")
cohen.d(accBlo~Condition,data=zeit.block2)
# block3
zeit.block3<-filter(zeit.Dev,Block=="3")
t.test(accBlo~Condition,data=zeit.block3,var.equal=TRUE)
cohen.d(accBlo~Condition,data=zeit.block3)
# block16
zeit.block16<-filter(zeit.Dev,Block=="16")
t.test(accBlo~Condition,data=zeit.block16,var.equal=TRUE)
cohen.d(accBlo~Condition,data=zeit.block16)
# block17
zeit.block17<-filter(zeit.Dev,Block=="17")
t.test(accBlo~Condition,data=zeit.block17,var.equal=TRUE)
cohen.d(accBlo~Condition,data=zeit.block17)
# block29
zeit.block29<-filter(zeit.Dev,Block=="29")
t.test(accBlo~Condition,data=zeit.block29,var.equal=TRUE)
cohen.d(accBlo~Condition,data=zeit.block29)
# block30
zeit.block30<-filter(zeit.Dev,Block=="30")
t.test(accBlo~Condition,data=zeit.block30,var.equal=TRUE)
cohen.d(accBlo~Condition,data=zeit.block30)

# Grafik
zeit.grafik.6blo<-zeit.Dev %>%
  group_by(Block,Condition) %>%
  summarise(acc=mean(accBlo),SE=std.error(accBlo))

ggplot(zeit.grafik.6blo, aes(x=zeit.grafik.6blo$Block, y=zeit.grafik.6blo$acc, 
                             group=zeit.grafik.6blo$Condition, color=Condition)) +
  geom_line(aes(color=Condition))+
  geom_point(aes(color=Condition), size=2)+
  coord_cartesian(ylim=c(0,0.4)) +
  labs(title="",x="Block", y= "acc")+
  scale_fill_manual(name="Condition", values=c("Control", "Stress"), labels=c("Control", "Stress"))+
  theme_classic()+
  scale_colour_manual(values=c("#999999","#CC0000"))

######################
#### 2 reaaktions Zeit

# ungültige Daten Rate
rt.unRate <- data.r6 %>%   
  group_by(Condition) %>% 
  summarise(unAnzhal=sum(RT==-1),allAnzhal=length(Trial)) 
rt.unRate$unRate<-rt.unRate$unAnzhal/rt.unRate$allAnzhal
rt.unRate

# 0.1 Daten r2 vorbereiten: ungültige RT ausschließen
data.r7<-subset(data.r6,RT>0)

#check data
boxplot(data.r7$RT~data.r7$Dev,data=data.r7) 


# 0.2 rechnen mean rt für ANOVA
rt.meanBlo <- data.r7 %>% 
  group_by(VP,Condition,Block,Stimulus,Dev,Dev_jn) %>%
  summarise(rtBlo=mean(RT))

rt.meanBedin <- rt.meanBlo %>% 
  group_by(VP,Condition,Stimulus,Dev,Dev_jn) %>%
  summarise(rt=mean(rtBlo))

#2.1 ANOVA rechen
ezDesign(rt.meanBedin,x=VP,y=Dev,row= Stimulus)

rt.meanBedin1<-filter(rt.meanBedin,VP!= 15,VP!= 34)


rt.anova <- ezANOVA(data = rt.meanBedin1
                    , dv = rt
                    , wid = VP
                    , within = .(Stimulus,Dev)
                    , between = .(Condition)
)
rt.anova 

# Stimuli effect
rt.stimuli<-rt.meanBedin1 %>%
  group_by(VP,Condition,Stimulus) %>%
  summarise(meanRt=mean(rt))
t.test(meanRt~Stimulus,data=rt.stimuli,var.equal=TRUE)
cohen.d(meanRt~Stimulus,data=rt.stimuli)

t.test(rt~Stimulus,data=rt.meanBedin1 ,var.equal=TRUE)
cohen.d(rt~Stimulus,data=rt.meanBedin1)

# Dev Stimuli interaction
#  NoDev, Shigh vs Slow
rt.No<-filter(rt.meanBedin1,Dev=="NoDev")
t.test(rt.No$rt~rt.No$Stimulus,var.equal=TRUE)
#  Dev H, Shigh vs Slow
rt.Dh<-filter(rt.meanBedin1,Dev=="DevH")
t.test(rt.Dh$rt~rt.Dh$Stimulus,var.equal=TRUE)
# Dev L, Shigh vs Slow
rt.Dl<-filter(rt.meanBedin1,Dev=="DevL")
t.test(rt.Dl$rt~rt.Dh$Stimulus,var.equal=TRUE)


##2.2 Grafik für Rt
rt.grafik <- rt.meanBedin1 %>% 
  group_by(Condition,Dev,Stimulus) %>%
  summarise(rt1=mean(rt),SE=std.error(rt))
# kontrol
rt.kontrol <- subset(rt.grafik,Condition=="Control")
ggplot(rt.kontrol, aes(x = rt.kontrol$Dev, y = rt.kontrol$rt1, fill = rt.kontrol$Stimulus))+ 
  geom_bar(stat = "identity", position = position_dodge(), colour = "white", width = 0.6)+
  ylab("RT_Kontrolle") +
  xlab ("Devaluation") +
  coord_cartesian(ylim=c(0,700)) +
  scale_x_discrete(breaks=c("NoDev", "DevH", "DevL"), 
                   labels=c("No Dev", "Dev High","Dev Low")) +
  scale_fill_discrete(name="Stimulus",
                      breaks=c("S_high", "S_low"),
                      labels=c("S high", "S low")) + 
  geom_errorbar(aes(ymin=rt1-SE, ymax=rt1+SE), width=.1, position=position_dodge(.6))+
  theme(text=element_text(size=20), axis.text.x=element_text(size=10, colour = "black"),    # Beschriftung x-Achse
        axis.text.y=element_text(size=20, colour = "black"))+
  theme_bw()+ ## Background white
  scale_fill_manual(values=c("#006600","#CC3300"))

# stress
rt.stress <- subset(rt.grafik,Condition=="Stress")
ggplot(rt.stress, aes(x = rt.stress$Dev, y = rt.stress$rt1, fill = rt.stress$Stimulus))+ 
  geom_bar(stat = "identity", position = position_dodge(), colour = "white", width = 0.6)+
  ylab("RT_Stress") +
  xlab ("Devaluation") +
  coord_cartesian(ylim=c(0,700)) +
  scale_x_discrete(breaks=c("NoDev", "DevH", "DevL"), 
                   labels=c("No Dev", "Dev High","Dev Low")) +
  scale_fill_discrete(name="Stimulus",
                      breaks=c("S_high", "S_low"),
                      labels=c("S high", "S low")) + 
  geom_errorbar(aes(ymin=rt1-SE, ymax=rt1+SE), width=.1, position=position_dodge(.6))+
  theme(text=element_text(size=20), axis.text.x=element_text(size=10, colour = "black"),    # Beschriftung x-Achse
        axis.text.y=element_text(size=20, colour = "black"))+
  theme_bw()+ ## Background white
  scale_fill_manual(values=c("#006600","#CC3300"))


