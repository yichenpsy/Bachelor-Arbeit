library(tidyverse) ## Alternative: library(dplyr); library(ggplot2)
library(plotrix) # f??r std.error()
library(dplyr)
library(readxl)
library(ez)
library(xlsx)
library(reshape2)
library(effsize) 
# Tabelle bilden
# data.s : alle roh Werte aus Excel
# data.m: MDBF
# data.s1: relevant Variablen auswahl, TSST score umcodierung,mean TSST berechnen
# data.s2: TSST levels als eine Variable

## Arbeitsspeicher leeren
rm(list=ls())

## Vorbereitung 
# set working directory: Arbeitsverzeichnis setzen (wo liegen die Daten?)
setwd("/Users/yichenzhong/Desktop/")
# eine leere Datentabelle anlegen
data.s <- data.frame()
# daten read
### zuerst, excel Daten bearbeiten: falsch, unvollständige ,wiederholende Werte löschen!!
data.s <- read_excel("MBDF_BD.xlsx")
length(data.s$VP)
# daten filtern
data.s <- filter(data.s,data.s$gruppe == "k" | data.s$gruppe == "s"  )
data.s <- filter(data.s,data.s$Sys_M5_t2 > 1 )
length(data.s$VP)

data.s %>% 
  group_by(sex,gruppe) %>%
  summarise(N = n())

###################
#######MDBF#######
#################
# 1. data.m: daten select von data.s: MBDF, sex, gruppe
data.m <- data.frame()
data.m <- select( data.s, VP, sex,gruppe,MDBF_1:MDBF_24)

# 2. data.m1: neue Variablen: MBDF_item
data.m1<-melt(data.m,id=(c("VP","sex","gruppe")))
data.m1<- rename(data.m1,"item"= variable )

# 3. neue Variable revers: 3,4,5,7,9,11,13,16,18,19,22,23 =  abs(MDBF_value -6)
data.m1$revers<-ifelse(data.m1$item=="MDBF_3"|data.m1$item=="MDBF_4"|
                         data.m1$item=="MDBF_5"|data.m1$item=="MDBF_7"|
                         data.m1$item=="MDBF_9"|data.m1$item=="MDBF_11"|
                         data.m1$item=="MDBF_13"|data.m1$item=="MDBF_16"|
                         data.m1$item=="MDBF_18"|data.m1$item=="MDBF_19"|
                         data.m1$item=="MDBF_22"|data.m1$item=="MDBF_23",
                       abs(data.m1$value-6),data.m1$value)

# 4. neue Vriable level: GS: 1,4,8,11,14,16,18,21  WM: 2,5,7,10,13,17,20,23  RU:3,6,9,12,15,19,22,24
data.m1$level <-ifelse(data.m1$item=="MDBF_1"|data.m1$item=="MDBF_4"|data.m1$item=="MDBF_8"|
                         data.m1$item=="MDBF_11"|data.m1$item=="MDBF_14"|data.m1$item=="MDBF_16"|
                         data.m1$item=="MDBF_18"|data.m1$item=="MDBF_21","GS",
                       ifelse(data.m1$item=="MDBF_2"|data.m1$item=="MDBF_5"|data.m1$item=="MDBF_7"|
                                data.m1$item=="MDBF_10"|data.m1$item=="MDBF_13"|data.m1$item=="MDBF_17"|
                                data.m1$item=="MDBF_20"|data.m1$item=="MDBF_23","WM","RU"))

# 5. sum_level, sum
mean.MDBF <- data.m1 %>% 
  group_by(VP,sex,gruppe) %>%
  summarise(sum=sum(revers)) 

mean.MDBF_level <- data.m1 %>% 
  group_by(VP,sex,gruppe,level) %>%
  summarise(sum=sum(revers)) 

## ANOVA
# ANOVA 1.Gute-Schlecht Stimmung
GS<-subset(mean.MDBF_level,mean.MDBF_level$level=="GS")
GS.anova <- ezANOVA(data = GS
                      , dv = sum
                      , wid = VP
                      , between = .(gruppe, sex)
)
GS.anova

GS %>% 
  group_by(gruppe) %>%
  summarise(gs = mean(sum))

t.test(GS$sum~GS$gruppe,var.equal=TRUE)
cohen.d(GS$sum~GS$gruppe)

# ANOVA 2.Ruhe-Unruhe 
RU <-subset(mean.MDBF_level,mean.MDBF_level$level=="RU")
RU.anova <- ezANOVA(data = RU
                    , dv = sum
                    , wid = VP
                    , between = .(gruppe, sex)
)
RU.anova

RU %>% 
  group_by(gruppe) %>%
  summarise(ru = mean(sum))

t.test(RU$sum~RU$gruppe,var.equal=TRUE)
cohen.d(RU$sum~RU$gruppe)

# ANOVA 3.Wachheit-Müdigkeit
WM <-subset(mean.MDBF_level,mean.MDBF_level$level=="WM")
WM.anova <- ezANOVA(data = WM
                    , dv = sum
                    , wid = VP
                    , between = .(gruppe, sex)
)
WM.anova

WM %>% 
  group_by(sex) %>%
  summarise(wm = mean(sum))

t.test(WM$sum~WM$sex)
t.test(WM$sum~WM$gruppe,var.equal=TRUE)
cohen.d(WM$sum~WM$gruppe)


###### TSST Post Fragen
# Tabelle bilden
# data.s : alle roh Werte aus Excel
# data.s1: relevant Variablen auswahl, TSST score umcodierung,mean TSST berechnen
# data.s2: TSST levels als eine Variable
data.s1 <- data.frame()
data.s1 <- select( data.s, VP, sex,gruppe,TSST_Sch,TSST_Una,TSST_Str)
data.s1$TSST_Sch <-((data.s1$TSST_Sch-1)*10)
data.s1$TSST_Una <-((data.s1$TSST_Una-1)*10)
data.s1$TSST_Str <-((data.s1$TSST_Str-1)*10)
data.s1$TSST_mean <- (data.s1$TSST_Sch + data.s1$TSST_Una + data.s1$TSST_Str)/3
data.s1$TSST_mean <-round(data.s1$TSST_mean, 2)
data.s2<-melt(data.s1, id=(c("VP","sex","gruppe","TSST_mean")))
data.s2<- rename(data.s2,"TSST_levels"= variable, "TSST_value"= value )

# Proband Bio Daten
# Anzahl Geschlecht
anzahl.Geschl<-table(data.s1$sex)
anzahl.Geschl
# gruppe
anzahl.gruppe<-table(data.s1$gruppe)
anzahl.gruppe
# Anzahl gruppe x Geschlecht
data.s1 %>% 
  group_by(gruppe, sex) %>% 
  summarise(length(VP)) 

## ANOVA für 3 Fragen 
# ANOVA 1. Schwierigkeit
Sch<-subset(data.s2,data.s2$TSST_levels=="TSST_Sch")
Sch.anova <- ezANOVA(data = Sch
                     , dv = TSST_value
                     , wid = VP
                     , between = .(gruppe, sex)
)
Sch.anova

# ANOVA 2. Unangenehm
Una<-subset(data.s2,data.s2$TSST_levels=="TSST_Una")
Una.anova <- ezANOVA(data = Una
                     , dv = TSST_value
                     , wid = VP
                     , between = .(gruppe, sex)
)
Una.anova

# ANOVA 3. Stressig
Str<-subset(data.s2,data.s2$TSST_levels=="TSST_Str")
Str.anova <- ezANOVA(data = Str
                     , dv = TSST_value
                     , wid = VP
                     , between = .(gruppe, sex)
)
Str.anova


# gruppe signifikant unterschiede
# kein Geschlecht effekt, kein interaktion effect

# mean Werte Tabelle von subjective rating (3 levels)
mean.sr1 <- data.s2 %>% 
  group_by(gruppe,TSST_levels) %>%
  summarise(m_TSST=mean(TSST_value),SE=std.error(TSST_value))
mean.sr1$m_TSST <- round (mean.sr1$m_TSST, 2)

# Grafik für subjective rating (3 levels)
ggplot(mean.sr1, aes(x = mean.sr1$TSST_levels, y = mean.sr1$m_TSST, fill = mean.sr1$gruppe))+ 
  geom_bar(position = "dodge", stat = "identity", colour = "white", width = 0.6)+
  theme_bw()+
  scale_fill_brewer(palette="Pastel1")+
  ylab("Subjective rating") +
  xlab ("Drei TSST Post-Fragen") +
  coord_cartesian(ylim=c(0,100)) +
scale_x_discrete(breaks=c("TSST_Sch", "TSST_Una","TSST_Str"), 
       labels=c("schwer", "unangenehm","stress")) +
scale_fill_discrete(name="Group",
                      breaks=c("k", "s"),
                      labels=c("control", "stress")) + 
  geom_errorbar(aes(ymin=m_TSST-SE, ymax=m_TSST+SE), width=.1, position=position_dodge(.6))+
  theme(text=element_text(size=17), axis.text.x=element_text(size=17, colour = "black"),    # Beschriftung x-Achse
        axis.text.y=element_text(size=17, colour = "black"))+
scale_fill_manual(values=c( "#999999","#CC3333"))


#########################
##### Blood Pressure ###

data.bp <- data.frame()
data.bp <- select(data.s, VP, sex,gruppe,Sys_M1_t1:Pulse_M5_t2)

#data.bp mean werte Jeder Messung rechnen
data.bp1<-data.bp
data.bp1<- filter(data.bp1,VP!=042,VP!=005)

data.bp1 %>%
  group_by(sex,gruppe)%>%
  summarise(n=n())
  
data.bp1$Sys_mean1 <- (data.bp1$Sys_M1_t1 + data.bp1$Sys_M1_t2)/2
data.bp1$Dia_mean1 <- (data.bp1$Dia_M1_t1 + data.bp1$Dia_M1_t2)/2
data.bp1$Pulse_mean1 <- (data.bp1$Pulse_M1_t1 + data.bp1$Pulse_M1_t2)/2

data.bp1$Sys_mean2 <- (data.bp1$Sys_M2_t1 + data.bp1$Sys_M2_t2)/2
data.bp1$Dia_mean2 <- (data.bp1$Dia_M2_t1 + data.bp1$Dia_M2_t2)/2
data.bp1$Pulse_mean2 <- (data.bp1$Pulse_M2_t1 + data.bp1$Pulse_M2_t2)/2

data.bp1$Sys_mean3 <- (data.bp1$Sys_M3_t1 + data.bp1$Sys_M3_t2)/2
data.bp1$Dia_mean3 <- (data.bp1$Dia_M3_t1 + data.bp1$Dia_M3_t2)/2
data.bp1$Pulse_mean3 <- (data.bp1$Pulse_M3_t1 + data.bp1$Pulse_M3_t2)/2

data.bp1$Sys_mean4 <- (data.bp1$Sys_M4_t1 + data.bp1$Sys_M4_t2)/2
data.bp1$Dia_mean4 <- (data.bp1$Dia_M4_t1 + data.bp1$Dia_M4_t2)/2
data.bp1$Pulse_mean4 <- (data.bp1$Pulse_M4_t1 + data.bp1$Pulse_M4_t2)/2

data.bp1$Sys_mean5 <- (data.bp1$Sys_M5_t1 + data.bp1$Sys_M5_t2)/2
data.bp1$Dia_mean5 <- (data.bp1$Dia_M5_t1 + data.bp1$Dia_M5_t2)/2
data.bp1$Pulse_mean5 <- (data.bp1$Pulse_M5_t1 + data.bp1$Pulse_M5_t2)/2

# data.bp2 relevant Daten auswahl
data.bp2 <-  select(data.bp1, VP, sex,gruppe,Sys_mean1:Pulse_mean5)

# data.bp3 variable "Messung", blood pressure
data.bp3 <- melt(data.bp2, id=(c("VP","sex","gruppe")))
data.bp3<- rename(data.bp3,"Messungen"= variable, "bp"= value)

## Systolic
#Sys Daten auswahl
Sys <- subset(data.bp3, data.bp3$Messungen=="Sys_mean1"| 
                data.bp3$Messungen=="Sys_mean2"| 
                data.bp3$Messungen=="Sys_mean3"| 
                data.bp3$Messungen=="Sys_mean4"| 
                data.bp3$Messungen=="Sys_mean5")

# mean Werte Tabelle Sys
mean.sys <- Sys %>% 
  group_by(gruppe,Messungen) %>%
  summarise(M=mean(bp),SE=std.error(bp)) 
mean.sys$M <- round (mean.sys$M , 2)  #2 komma stelle
mean.sys$Messungen=factor( mean.sys$Messungen,
                           levels=c("Sys_mean1","Sys_mean2","Sys_mean3","Sys_mean4","Sys_mean5"),
                           labels=c("T1","T2","T3","T4","T5"))
mean.sys$gruppe=factor( mean.sys$gruppe, levels=c("k","s"), labels = c("control","stress"))

#### Grafik für Sys
ggplot(mean.sys, aes(x=mean.sys$Messungen, y=mean.sys$M,group=mean.sys$gruppe)) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.1,color="gray",
                position=position_dodge(0.05))+
  geom_line(aes(color=gruppe),size=1)+
  geom_point(aes(color=gruppe), size=2)+
  labs(title="Systolic Blood Pressure",x="Time", y= "Systolic blood pressure (mmHg)")+
  scale_fill_manual(name="Group", values=c( "s","k"), labels=c( "stress","control"))+
  theme(text=element_text(size=30), axis.text.x=element_text(size=30, colour = "black"),    # Beschriftung x-Achse
        axis.text.y=element_text(size=30, colour = "black"))+
  theme_classic()+
  scale_colour_manual(values=c( "#999999","#CC0000"))

#ANOVA: 5(Zeitpunke, with) x 2 (gruppe, betw) x 2 (Geschlecht, betw)
bp_Sys.anova <- ezANOVA(data = Sys
                    , dv = bp
                    , wid = VP
                    , within = .(Messungen)
                    , between = .(gruppe, sex)
)
bp_Sys.anova

# Post-Hoc T-test
# generall gruppe effect
t.test(Sys$bp~Sys$gruppe,var.equal=TRUE)
# jede Zeit Punkt gruppe effect
Sys_M1 <- subset(Sys, Sys$Messungen=="Sys_mean1")
t.test(Sys_M1$bp~Sys_M1$gruppe,var.equal=TRUE)

Sys_M2 <- subset(Sys, Sys$Messungen=="Sys_mean2")
t.test(Sys_M2$bp~Sys_M2$gruppe,var.equal=TRUE)
cohen.d(Sys_M2$bp~Sys_M2$gruppe)

Sys_M3 <- subset(Sys, Sys$Messungen=="Sys_mean3")
t.test(Sys_M3$bp~Sys_M3$gruppe,var.equal=TRUE)

Sys_M4 <- subset(Sys, Sys$Messungen=="Sys_mean4")
t.test(Sys_M4$bp~Sys_M4$gruppe,var.equal=TRUE)

Sys_M5 <- subset(Sys, Sys$Messungen=="Sys_mean5")
t.test(Sys_M5$bp~Sys_M5$gruppe,var.equal=TRUE)

#Diastolic
#Dia Daten auswahl
Dia <- subset(data.bp3, data.bp3$Messungen=="Dia_mean1"| 
                data.bp3$Messungen=="Dia_mean2"| 
                data.bp3$Messungen=="Dia_mean3"| 
                data.bp3$Messungen=="Dia_mean4"| 
                data.bp3$Messungen=="Dia_mean5")

# mean Werte Tabelle Sys
mean.dia <- Dia %>% 
  group_by(gruppe,Messungen) %>%
  summarise(M=mean(bp),SE=std.error(bp)) 
mean.dia$M <- round (mean.dia$M , 2)  #2 komma stelle
mean.dia$Messungen=factor( mean.dia$Messungen,
                           levels=c("Dia_mean1","Dia_mean2","Dia_mean3","Dia_mean4","Dia_mean5"),
                           labels=c("T1","T2","T3","T4","T5"))
mean.dia$gruppe=factor( mean.dia$gruppe, levels=c("k","s"), labels = c("control","stress"))

#### Grafik für Dia
ggplot(mean.dia, aes(x=mean.dia$Messungen, y=mean.dia$M,group=mean.dia$gruppe)) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.1,color="gray",
                position=position_dodge(0.05))+
  geom_line(aes(color=gruppe),size=1)+
  geom_point(aes(color=gruppe), size=2)+
  labs(title="Diatolic Blood Pressure",x="Time", y= "Diatolic blood pressure (mmHg)")+
  scale_fill_manual(name="Group", values=c( "s","k"), labels=c( "stress","control"))+
  theme_classic()+
  scale_colour_manual(values=c( "#999999","#CC0000"))

#ANOVA: 5(Zeitpunke, with) x 2 (gruppe, betw) x 2 (Geschlecht, betw)
bp_Dia.anova <- ezANOVA(data = Dia
                        , dv = bp
                        , wid = VP
                        , within = .(Messungen)
                        , between = .(gruppe, sex)
)
bp_Dia.anova

# Post-Hoc T-test
# generall gruppe effect
t.test(Sys$bp~Dia$gruppe,var.equal=TRUE)
# jede Zeit Punkt gruppe effect
Dia_M1 <- subset(Dia, Dia$Messungen=="Dia_mean1")
t.test(Dia_M1$bp~Dia_M1$gruppe,var.equal=TRUE)

Dia_M2 <- subset(Dia, Dia$Messungen=="Dia_mean2")
t.test(Dia_M2$bp~Dia_M2$gruppe,var.equal=TRUE)
cohen.d(Dia_M2$bp~Dia_M2$gruppe)

Dia_M3 <- subset(Dia, Dia$Messungen=="Dia_mean3")
t.test(Dia_M3$bp~Dia_M3$gruppe,var.equal=TRUE)

Dia_M4 <- subset(Dia, Dia$Messungen=="Dia_mean4")
t.test(Dia_M4$bp~Dia_M4$gruppe,var.equal=TRUE)

Dia_M5 <- subset(Dia, Dia$Messungen=="Dia_mean5")
t.test(Dia_M5$bp~Dia_M5$gruppe,var.equal=TRUE)

##....

#Pulse
#Pulse Daten auswahl
Pul <- subset(data.bp3, data.bp3$Messungen=="Pulse_mean1"| 
                data.bp3$Messungen=="Pulse_mean2"| 
                data.bp3$Messungen=="Pulse_mean3"| 
                data.bp3$Messungen=="Pulse_mean4"| 
                data.bp3$Messungen=="Pulse_mean5")

# mean Werte Tabelle Sys
mean.pul <- Pul %>% 
  group_by(gruppe,Messungen) %>%
  summarise(M=mean(bp),SE=std.error(bp)) 
mean.pul$M <- round (mean.pul$M , 2)  #2 komma stelle
mean.pul$Messungen=factor( mean.pul$Messungen,
                           levels=c("Pulse_mean1","Pulse_mean2","Pulse_mean3","Pulse_mean4","Pulse_mean5"),
                           labels=c("T1","T2","T3","T4","T5"))
mean.pul$gruppe=factor( mean.pul$gruppe, levels=c("k","s"), labels = c("control","stress"))

#### Grafik für Dia
ggplot(mean.pul, aes(x=mean.pul$Messungen, y=mean.pul$M,group=mean.pul$gruppe)) +
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), width=.1,color="gray",
                position=position_dodge(0.05))+
  geom_line(aes(color=gruppe),size=1)+
  geom_point(aes(color=gruppe), size=2)+
  labs(title="Pulse",x="Time", y= "Pulse")+
  scale_fill_manual(name="Group", values=c( "s","k"), labels=c( "stress","control"))+
  theme_classic()+
  scale_colour_manual(values=c( "#999999","#CC0000"))

#ANOVA: 5(Zeitpunke, with) x 2 (gruppe, betw) x 2 (Geschlecht, betw)
bp_Pul.anova <- ezANOVA(data = Pul
                        , dv = bp
                        , wid = VP
                        , within = .(Messungen)
                        , between = .(gruppe, sex)
)
bp_Pul.anova

# Post-Hoc T-test
# generall gruppe effect
t.test(Pul$bp~Pul$gruppe,var.equal=TRUE)
# jede Zeit Punkt gruppe effect
Pul_M1 <- subset(Pul, Pul$Messungen=="Pulse_mean1")
t.test(Pul_M1$bp~Pul_M1$gruppe,var.equal=TRUE)

Pul_M2 <- subset(Pul, Pul$Messungen=="Pulse_mean2")
t.test(Pul_M2$bp~Pul_M2$gruppe,var.equal=TRUE)
cohen.d(Pul_M2$bp~Pul_M2$gruppe)

Pul_M3 <- subset(Pul, Pul$Messungen=="Pulse_mean3")
t.test(Pul_M3$bp~Pul_M3$gruppe,var.equal=TRUE)
cohen.d(Pul_M3$bp~Pul_M3$gruppe)

Pul_M4 <- subset(Pul, Pul$Messungen=="Pulse_mean4")
t.test(Pul_M4$bp~Pul_M4$gruppe,var.equal=TRUE)

Pul_M5 <- subset(Pul, Pul$Messungen=="Pulse_mean5")
t.test(Pul_M5$bp~Pul_M5$gruppe,var.equal=TRUE)
