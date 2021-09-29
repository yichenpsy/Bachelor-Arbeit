######## Online Fragebögen
# set working directory: Arbeitsverzeichnis setzen (wo liegen die Daten?)
setwd("/Users/yichenzhong/Desktop/")
# eine leere Datentabelle anlegen
data.o <- data.frame()
# daten read
### zuerst, excel Daten bearbeiten: falsch, unvollständige ,wiederholende Werte löschen!!
data.o <- read_excel("Online1.xlsx")
length(data.o$VP)
data.o %>%
  group_by(SEX,Condition)%>%
  summarise(n())


######STAI
# state
stai.s<-select(data.o,VP,SEX,Condition,STAI_S_T2_1:STAI_S_T2_20)
stai.s1<-melt(stai.s,id=(c("VP","SEX","Condition")))
stai.s1<- rename(stai.s1,"item"= variable )

stai.s1$revers<-ifelse(stai.s1$item=="STAI_S_T2_1"|stai.s1$item=="STAI_S_T2_2"|
                       stai.s1$item=="STAI_S_T2_5"|stai.s1$item=="STAI_S_T2_8"|
                       stai.s1$item=="STAI_S_T2_10"|stai.s1$item=="STAI_S_T2_11"|
                       stai.s1$item=="STAI_S_T2_15"|stai.s1$item=="STAI_S_T2_16"|
                       stai.s1$item=="STAI_S_T2_19"|stai.s1$item=="STAI_S_T2_20",
                       -(stai.s1$value)+5,stai.s1$value)

stai.s.sum<-stai.s1 %>%
  group_by(VP,SEX,Condition) %>%
  summarise(sum=sum(value))


stai.s.anova<- ezANOVA(data = stai.s.sum
                        , dv = sum
                        , wid = VP
                        , between = .(Condition, SEX)
)
stai.s.anova

t.test(stai.s.sum$sum~stai.s.sum$Condition,var.equal=TRUE)
cohen.d(stai.s.sum$sum~stai.s.sum$Condition)

#Trait
stai.t<-select(data.o,VP,SEX,Condition,STAI_T_1:STAI_T_20)
stai.t1<-melt(stai.t,id=(c("VP","SEX","Condition")))
stai.t1<- rename(stai.t1,"item"= variable )

stai.t1$revers<-ifelse(stai.t1$item=="STAI_T_1"|stai.t1$item=="STAI_T_6"|
                          stai.t1$item=="STAI_T_7"|stai.t1$item=="STAI_T_10"|
                          stai.t1$item=="STAI_T_13"|stai.t1$item=="STAI_T_16"|
                          stai.t1$item=="STAI_T_19",
                       -(stai.t1$value)+5,stai.t1$value)

stai.t.sum<-stai.t1 %>%
  group_by(VP,SEX,Condition) %>%
  summarise(sum=sum(value))

stai.t.anova<- ezANOVA(data = stai.t.sum
                       , dv = sum
                       , wid = VP
                       , between = .(Condition, SEX)
)
stai.t.anova

t.test(stai.t.sum$sum~stai.t.sum$Condition,var.equal=TRUE)
cohen.d(stai.t.sum$sum~stai.t.sum$Condition)

##################
###### BDI
BDI<-select(data.o,VP,SEX,Condition,BDI_1:BDI_21)
BDI1<-melt(BDI,id=(c("VP","SEX","Condition")))
BDI1$value1<-BDI1$value-1

BDI.sum<-BDI1 %>%
  group_by(VP,SEX,Condition) %>%
  summarise(sum=sum(value1))

BDI.anova<- ezANOVA(data = BDI.sum
                       , dv = sum
                       , wid = VP
                       , between = .(Condition, SEX)
)
BDI.anova

t.test(BDI.sum$sum~BDI.sum$Condition,var.equal=TRUE)
cohen.d(BDI.sum$sum~BDI.sum$Condition)
#################
###### TICS
t<-select(data.o,VP,SEX,Condition,TICS_1:TICS_57)

t1<-melt(t,id=(c("VP","SEX","Condition")))
t1<- rename(t1,"item"= variable )


# 7 skala codieren
# Uebe: 1,4,17,27,38,44,50,54
# Soue: 7,19,28,39,49,57
# Erdr: 8,12,14,22,23,30,32,40,43
# Unzu: 5,10,13,21,37,41,48,53
# Uefo: 3,20,24,35,47,55
# Mang: 2,18,31,46
# Sozs: 6,15,26,33,45,52
# Sozi: 11,29,34,42,51,56
#Sorg: 9,16,25,36
#Sscs: 9,16,18,25,31,35,36,38,44,47,54,57

t1$skala <-ifelse(t1$item=="TICS_1"|t1$item=="TICS_4"|t1$item=="TICS_17"| t1$item=="TICS_27"|
                    t1$item=="TICS_38"|t1$item=="TICS_44"|t1$item=="TICS_50"|t1$item=="TICS_54",
                  "Uebe",
                  
ifelse(t1$item=="TICS_7"|t1$item=="TICS_19"|t1$item=="TICS_28"|t1$item=="TICS_39"|
         t1$item=="TICS_49"|t1$item=="TICS_57",
       "Soue",
       
ifelse(t1$item=="TICS_8"|t1$item=="TICS_12"|t1$item=="TICS_14"| t1$item=="TICS_22"|
         t1$item=="TICS_23"|t1$item=="TICS_30"|t1$item=="TICS_32"|t1$item=="TICS_40"|
         t1$item=="TICS_43",
       "Erdr",
       
ifelse(t1$item=="TICS_5"|t1$item=="TICS_10"|t1$item=="TICS_13"|t1$item=="TICS_21"|
         t1$item=="TICS_37"|t1$item=="TICS_41"|t1$item=="TICS_48"|t1$item=="TICS_53",
       "Unzu",
       
ifelse(t1$item=="TICS_3"|t1$item=="TICS_20"|t1$item=="TICS_24"|t1$item=="TICS_35"|
         t1$item=="TICS_47"|t1$item=="TICS_55",
        "Uefo",
       
ifelse(t1$item=="TICS_2"|t1$item=="TICS_18"|t1$item=="TICS_31"|t1$item=="TICS_46",
       "Mang",
       
 ifelse(t1$item=="TICS_6"|t1$item=="TICS_15"|t1$item=="TICS_26"|t1$item=="TICS_33"|
          t1$item=="TICS_45"|t1$item=="TICS_52",
        "Sozs", 

ifelse(t1$item=="TICS_11"|t1$item=="TICS_29"|t1$item=="TICS_34"|t1$item=="TICS_42"|
         t1$item=="TICS_51"|t1$item=="TICS_56",
       "Sozi", 
               
ifelse(t1$item=="TICS_9"|t1$item=="TICS_16"|t1$item=="TICS_25"|t1$item=="TICS_36",
       "Sorg", 
                      
ifelse(t1$item=="TICS_9"|t1$item=="TICS_16"|t1$item=="TICS_18"|t1$item=="TICS_25"|
         t1$item=="TICS_31"|t1$item=="TICS_35"|t1$item=="TICS_36"|t1$item=="TICS_38"|
         t1$item=="TICS_44"|t1$item=="TICS_47"|t1$item=="TICS_54"|t1$item=="TICS_57",
       "Sscs", "no"
 ))))))))))

t1$value<-t1$value-1
                          
x<-t1%>% #check
  group_by(skala,VP) %>%
  summarise(n())

#  Uebe * Arbeitsüberlastung  Work overload
t.uebe<- filter(t1,skala=="Uebe")
t.uebe1<- t.uebe%>%
  group_by(VP,Condition,SEX) %>%
  summarise(sum=sum(value))
t.test(t.uebe1$sum~t.uebe1$Condition,var.equal=TRUE)
cohen.d(t.uebe1$sum~t.uebe1$Condition)

#  Soue Soziale Überlastung  Social overload
t.soue<- filter(t1,skala=="Soue")
t.soue1<- t.soue%>%
  group_by(VP,Condition,SEX) %>%
  summarise(sum=sum(value))
t.test(t.soue1$sum~t.soue1$Condition,var.equal=TRUE)
cohen.d(t.soue1$sum~t.soue1$Condition)

#  Erdr Erfolgsdruck  Performance pressure
t.erdr<- filter(t1,skala=="Erdr")
t.erdr1<- t.erdr%>%
  group_by(VP,Condition,SEX) %>%
  summarise(sum=sum(value))
t.test(t.erdr1$sum~t.erdr1$Condition,var.equal=TRUE)
cohen.d(t.erdr1$sum~t.erdr1$Condition)

#  Unzu * Arbeitsunzufriedenheit   Work discontent
t.unzu<- filter(t1,skala=="Unzu")
t.unzu1<- t.unzu%>%
  group_by(VP,Condition,SEX) %>%
  summarise(sum=sum(value))
t.test(t.unzu1$sum~t.unzu1$Condition,var.equal=TRUE)
cohen.d(t.unzu1$sum~t.unzu1$Condition)

# Uefo Überforderung  Excessive workload
t.uefo<- filter(t1,skala=="Uefo")
t.uefo1<- t.uefo%>%
  group_by(VP,Condition,SEX) %>%
  summarise(sum=sum(value))
t.test(t.uefo1$sum~t.uefo1$Condition,var.equal=TRUE)
cohen.d(t.uefo1$sum~t.uefo1$Condition)

# Mang Mangel an sozialer Anerkennung  Lack of social recognition
t.mang<- filter(t1,skala=="Mang")
t.mang1<- t.mang%>%
  group_by(VP,Condition,SEX) %>%
  summarise(sum=sum(value))
t.test(t.mang1$sum~t.mang1$Condition,var.equal=TRUE)
cohen.d(t.mang1$sum~t.mang1$Condition)

# Sozs Soziale Spannungen   Social tension
t.sozs<- filter(t1,skala=="Sozs")
t.sozs1<- t.sozs%>%
  group_by(VP,Condition,SEX) %>%
  summarise(sum=sum(value))
t.test(t.sozs1$sum~t.sozs1$Condition,var.equal=TRUE)
cohen.d(t.sozs1$sum~t.sozs1$Condition)

# Sozi Soziale Isolation p=0.14  Social isolation
t.sozi<- filter(t1,skala=="Sozi")
t.sozi1<- t.sozi%>%
  group_by(VP,Condition,SEX) %>%
  summarise(sum=sum(value))
t.test(t.sozi1$sum~t.sozi1$Condition,var.equal=TRUE)
cohen.d(t.sozi1$sum~t.sozi1$Condition)

# Sorg Besorgnis Chronic worrying
t.sorg<- filter(t1,skala=="Sorg")
t.sorg1<- t.sorg%>%
  group_by(VP,Condition,SEX) %>%
  summarise(sum=sum(value))
t.test(t.sorg1$sum~t.sorg1$Condition,var.equal=TRUE)
cohen.d(t.sorg1$sum~t.sorg1$Condition)


# SSCS Screening-Skala  TICS screening scale
t.sscs<-filter(t1,t1$item=="TICS_9"|t1$item=="TICS_16"|t1$item=="TICS_18"|t1$item=="TICS_25"|
                  t1$item=="TICS_31"|t1$item=="TICS_35"|t1$item=="TICS_36"|t1$item=="TICS_38"|
                  t1$item=="TICS_44"|t1$item=="TICS_47"|t1$item=="TICS_54"|t1$item=="TICS_57")
t.sscs1<- t.sscs%>%
  group_by(VP,Condition,SEX) %>%
  summarise(sum=sum(value))
t.test(t.sscs1$sum~t.sscs1$Condition,var.equal=TRUE)
cohen.d(t.sscs1$sum~t.sscs1$Condition)
