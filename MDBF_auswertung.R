library(tidyverse) ## Alternative: library(dplyr); library(ggplot2)
library(plotrix) # f??r std.error()
library(dplyr)
library(readxl)
library(ez)
library(xlsx)
library(reshape2)
## Arbeitsspeicher leeren
rm(list=ls())
setwd("/Users/yichenzhong/Desktop/BA/Daten/")
data.s <- data.frame()
# daten read
### zuerst, excel Daten bearbeiten: falsch, unvollständige ,wiederholende Werte löschen!!
data.s <- read_excel("S3.xlsx")
length(data.s$VP)
# daten filtern
data.s <- filter(data.s,data.s$gruppe == "k" | data.s$gruppe == "s" )
data.s <- filter(data.s,data.s$Sys_M5_t2 > 1 )
length(data.s$VP)
#######
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

# ANOVA, t-test
MBDF.anova <- ezANOVA(data = mean.MDBF_level
                      , dv = sum
                      , wid = VP
                      , within = .(level)
                      , between = .(gruppe, sex)
)
MBDF.anova
# levels effkt
level_sum <- data.m1 %>% 
  group_by(level) %>%
  summarise(sum=sum(revers)) 
# t-test für 3 levels?

# Kein Gruppe Unterschiede
t.test(mean.MDBF$sum~mean.MDBF$gruppe)

lev <- mean.MDBF_level %>% 
  group_by(level) %>% 
  summarise(mean(sum)) 

#Grafik
mean.grafik <- mean.MDBF %>% 
  group_by(gruppe) %>%
  summarise(mean=mean(sum),SE=std.error(sum))

ggplot(mean.grafik, aes(x = mean.grafik$gruppe, y=mean.grafik$mean,  fill = mean.grafik$gruppe))+ 
  geom_bar(position = "dodge", stat = "identity", colour = "white", width = 0.4)+
  theme_bw()+
  ylab("MDBF") +
  xlab ("Group") +
  coord_cartesian(ylim=c(0,120))+
  scale_x_discrete(breaks=c("k", "s"),
                   labels=c("control", "stress"))+ 
  geom_errorbar(aes(ymin=mean-SE, ymax=mean+SE), width=.1, position=position_dodge(.6))+
  theme(text=element_text(size=20), axis.text.x=element_text(size=20, colour = "black"),    # Beschriftung x-Achse
        axis.text.y=element_text(size=20, colour = "black"))+
scale_fill_manual(values=c( "#999999","#CC0000"))

## boxplot
boxplot(sum~gruppe,data=mean.MDBF, main="MDBF", 
        xlab="Groups", ylab="Sum MDBF")

by(mean.MDBF$sum,mean.MDBF$gruppe,summary)
by(mean.MDBF$sum,mean.MDBF$gruppe,sd)




