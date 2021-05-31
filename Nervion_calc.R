library(readxl)
library(tidyverse)
library(lubridate)
library(patchwork)
library(hrbrthemes)
library(scales)



pal_class<-c("#007eff","#00d600","#ffff00","#ff8c2b","#ff0000")
ClassNames <- c("High","Good","Mod","Poor","Bad")


# load data from AZTI
xlfile<-"data/ETC ICM task 1.6.2.1 data contaminants Nervión estuary for temporal trends.xls"

dfStn <- read_xls(xlfile,sheet="Sampling locations coordinates")
dfW <- read_xls(xlfile,sheet="Water")
dfS <- read_xls(xlfile,sheet="Sediment")
dfB <- read_xls(xlfile,sheet="Biota")

dfW <- dfW %>%
  mutate(Date=as.Date(`Sampling date`,tryFormats = c("%d/%m/%Y")))

dfB <- dfB %>%
  mutate(Date=as.Date(`Sampling date`,tryFormats = c("%d/%m/%Y")))
dfS <- dfS %>%
  mutate(Date=as.Date(`Sampling date`,tryFormats = c("%d/%m/%Y")))

# load file matching parameter names from AZTI to CHASE parameters
xlfile<-"data/params_AZTI_CHASE.xlsx"
dfParams <- read_xlsx(xlfile,sheet="PARAMS") %>%
  filter(Param!=0)

# ------------- load CHASE threshold values ---------------------------------------
dfThresholds<-read.table("data/thresholds_v6.txt",sep="\t",header=T,fileEncoding = "UTF-16") %>%
  filter(is.na(Exclude)) %>% 
  filter(Biota.Type!="Fish") %>%
  filter(Category!="BioEffects") %>%
  arrange(Category,Biota.Type,PARAM,Threshold.Species,Threshold.Tissue) %>%
  filter(ID!=95) %>% # drop OSPAR thresholds where there is an EU EQS
  filter(!ID %in% c(222,223,224,225,226)) #  OSPAR proposed PCB water EAC thresholds


dfThresholds <- dfThresholds %>%
  select(ID,PARAM,GROUP,Substance.name,Category,Threshold.Value,Threshold.Unit,Threshold.BASIS,Biota.Type,Threshold.Species,Threshold.Tissue)


# ------------- join data ---------------------------------------

df <- bind_rows(dfW,dfB,dfS)
df$ObsID <- 1:nrow(df)


dfstn <- df %>%
  mutate(Date=as.Date(`Sampling date`,tryFormats = c("%d/%m/%Y"))) %>%
  mutate(Year=year(Date)) %>%
  distinct(Year,`Sampling point`,`Matrix type`) %>%
  mutate(Colour=ifelse(Year %in% c(2000,2010,2020),1,0))



df <- df %>%
  mutate(ValueCorr = ifelse(Operator=="<",0.5,1)*as.numeric(Value))

df <- df %>%
  left_join(dfParams,by="Variable") %>%
  filter(!is.na(Param))

df <- df %>%
  group_by(Category=`Matrix type`,Station=`Sampling point`,Date,Variable,Param,Unit,Species,Sum) %>%
  summarise(Value=mean(ValueCorr,na.rm=T),n=n(),ObsID=paste0(ObsID,collapse=",")) %>% # take average of measurements of same parameter
  ungroup()

df <- df %>%
  left_join(dfThresholds,by=c("Category","Param"="PARAM"))

# remove Cadmium obs where species = oyster and threshold species = mytilus (and the other way round)
dfDrop <- data.frame(Param=c("CD","CD"),
                     Species=c("Crassostrea gigas","Mytilus galloprovincialis"),
                     Threshold.Species=c("Mytilus","Oysters"),
                     Drop=c(T,T))

df <- df %>%
  left_join(dfDrop,by=c("Param","Species","Threshold.Species"))

df <- df %>% 
  filter(is.na(Drop)) %>%
  select(-Drop)

# check that only one threshold is applied to each observation

test <- df %>%
  group_by(ObsID) %>%
  summarise(n=n()) %>%
  filter(n>1) %>%
  ungroup() 

nrow(test) # should be zero
df2 <-df # for checking errors


# remove values without a threshold 
df <- df %>%
  filter(!is.na(Threshold.Value))

# unit conversion factors
dfUnits <- data.frame(Category=c("Water","Water","Biota","Biota","Sediment","Sediment"),
                      Unit=c("µg/l","mg/l","µg/kg FW","mg/kg FW","µg/kg DW","mg/kg DW"),
                      Threshold.Unit=c("µg/l","µg/l","µg/kg","µg/kg","µg/kg","µg/kg"),
                      UnitFactor=c(1,1000,1,1000,1,1000))

df <- df %>%
  left_join(dfUnits,by=c("Category","Unit","Threshold.Unit"))

df <- df %>%
  mutate(ValueCorr = Value * UnitFactor)

df <- df %>%
  mutate(Sum=ifelse(is.na(Sum),"",Sum)) %>%
  mutate(Param=ifelse(Sum=="PCB6","PCB6",Param)) %>%
  group_by(Category,Station,Date,Param,Substance.name,
           Threshold.Value,Threshold.Unit,Threshold.BASIS,
           Biota.Type,Threshold.Species,Threshold.Tissue) %>%
  summarise(Value=sum(ValueCorr),n=n()) %>%
  ungroup()


# take annual averages
df <- df %>%
  mutate(Year=year(Date),
         logValue=log(Value))


df <- df %>% 
  group_by(Category,Station,Year,Param,Substance.name,
           Threshold.Value,Threshold.Unit,Threshold.BASIS,
           Biota.Type,Threshold.Species,Threshold.Tissue) %>%
  summarise(n=n(),Mean=mean(Value,na.rm=T),meanOfLog=mean(logValue,na.rm=T),Max=max(Value,na.rm=T),Min=min(Value,na.rm=T)) %>%
  mutate(logMean=exp(meanOfLog)) %>%
  mutate(CR=Mean/Threshold.Value) %>% 
  mutate(CRlog=logMean/Threshold.Value) 

#save(df,file="AZTI/CR_values.Rda")
#load("AZTI/CR_values.Rda")


# --------------------- Aggregation  ----------------------------------------

dfCategory <- df %>%
  ungroup() %>%
  group_by(Category,Station,Year) %>%
  summarise(sumCR=sum(CR,na.rm=T),sumCRlog=sum(CRlog,na.rm=T),n=n()) %>%
  mutate(CS=sumCR/sqrt(n),CSlog=sumCRlog/sqrt(n)) %>%
  mutate(log10CS=log10(CS),log10CSlog=log10(CSlog))

dfCHASE <- dfCategory %>%
  ungroup() %>%
  pivot_wider(id_cols=c(Station,Year),names_from=Category,values_from=CS) %>%
  arrange(Station,Year)

dfCHASElog <- dfCategory %>%
  ungroup() %>%
  pivot_wider(id_cols=c(Station,Year),names_from=Category,values_from=CSlog) %>%
  arrange(Station,Year)

#dfCHASE %>% group_by(Station) %>% summarise(B=mean(Biota,na.rm=T),S=mean(Sediment,na.rm=T),W=mean(Water,na.rm=T))
  

# ---------------------  plotting ----------------------------------------


stationlevelsSW <- c("E-N10","E-N15","E-N17","E-N20","E-N30","L-N10","L-N20","L-RF30")
stationlevelsB <- c("I-N20","I-N10")

dfPlotB <- dfCategory %>% filter(Category=="Biota")
dfPlotSW <- dfCategory %>% filter(Category %in% c("Sediment","Water"))
                                  
dfPlotSW$Station <- factor(dfPlotSW$Station,levels=stationlevelsSW)                                  
dfPlotB$Station <- factor(dfPlotB$Station,levels=stationlevelsB)                                  

CHASEcat<-function(CS){
  if(!is.numeric(CS)) return(NA)
  cat <- ifelse(CS<0.5,1,2)
  cat <- ifelse(CS>1,3,cat)
  cat <- ifelse(CS>5,4,cat)
  cat <- ifelse(CS>10,5,cat)
  return(cat)
}


dfPlotSW <- dfPlotSW %>%
  mutate(Cat=CHASEcat(CS)) %>%
  mutate(Class=ClassNames[Cat])

dfPlotSW[nrow(dfPlotSW)+1,"Year"]<-2020
dfPlotSW[nrow(dfPlotSW),"Station"]<-dfPlotSW$Station[1]
dfPlotSW[nrow(dfPlotSW),"Category"]<-dfPlotSW$Category[1]
dfPlotSW[nrow(dfPlotSW),"Class"]<-"High"

dfPlotSW$Class <- factor(dfPlotSW$Class,levels=ClassNames)

alpha_bands<-0.4

p1 <-ggplot(dfPlotSW, aes(x=Year, y=CSlog)) +
  geom_hline(yintercept=0,linetype=2, color="#FF0000") +
  geom_smooth(method='lm', aes(x=Year,y=log10CSlog),se=FALSE, color='turquoise4') +
  geom_point(aes(x=Year,y=log10CSlog), colour="#000000") +
  facet_grid(Category~Station,scales="free_y") +
  scale_color_manual(values=pal_class) +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines")) +
  xlab("Year") + ylab("log10(CS)")

p1
ggsave("png/SedimentWater.png",p1,dpi=300,units="cm",width=24,height=10)

dfPlotB<-dfPlotBx

dfPlotB[nrow(dfPlotB)+1,"Year"]<-2020
dfPlotB[nrow(dfPlotB),"Station"]<-dfPlotB$Station[1]
dfPlotB[nrow(dfPlotB),"Category"]<-dfPlotB$Category[1]

p2 <-ggplot(dfPlotB, aes(x=Year, y=CSlog)) +
  geom_hline(yintercept=0,linetype=2, color="#FF0000") +
  geom_smooth(method='lm', aes(x=Year,y=log10CSlog),se=FALSE, color='turquoise4') +
  geom_point(aes(x=Year,y=log10CSlog), colour="#000000") +
  facet_grid(Category~Station,scales="free_y") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines")) +
  xlab("Year") + ylab("log10(CS)")
  

p2
ggsave("png/Biota.png",p2,dpi=300,units="cm",width=14,height=8)


dfSubsCount <- df %>%
  group_by(Category,Year,Param,Substance.name) %>%
  summarise(nobs=n())

dfSubsCount <- dfSubsCount %>%
  ungroup() %>%
  group_by(Category,Year) %>%
  summarise(nobs=sum(nobs,na.rm=T),nsubs=n()) %>%
  mutate(Colour=ifelse(Year %in% c(2000,2010,2020),1,0))

mypal <- c("#AAAAAA","#666666")

p3 <-ggplot(dfSubsCount, aes(x=Year,y=nsubs,fill=factor(Colour))) +
  geom_col(width=0.5,alpha=0.5) +
  facet_grid(Category~.,scales="free_y") +
  theme_ipsum() +
  theme(strip.text.y = element_text(angle=0,size=9),
        panel.spacing = unit(1, "lines")) +
  scale_y_continuous(breaks= pretty_breaks(n=2)) +
  xlab("Year") + ylab("Count of substances") +
  scale_fill_manual(guide=NULL,values=mypal)

p3



library(hrbrthemes)
library(patchwork)
library(scales)

mypal <- c("#AAAAAA","#666666")

pstn <-ggplot(dfstn, aes(x=Year,fill=factor(Colour))) +
  geom_histogram(stat="count",width=0.5,alpha=0.5) +
  facet_grid(`Matrix type`~.) +
  theme_ipsum() +
  theme(panel.spacing = unit(1, "lines"),
        strip.text.y = element_text(angle=0,size=9)) +
  scale_y_continuous(breaks= pretty_breaks(n=3)) +
  ylab("Count of stations") +
  scale_fill_manual(guide=NULL,values=mypal)
pstn


layout <- '
AAAAAAAA
AAAAAAAA
###BB###
'
pTS <- wrap_plots(A = p1, B = p2, design = layout)
pTS

ggsave("png/timeseries.png",pTS,dpi=100,units="cm",width=24,height=16)

pcount <- pstn + p3 + plot_layout(ncol=1)
pcount

ggsave("png/obs_station_count.png",pcount,dpi=300,units="cm",width=14,height=16)


# ----------------------------------------Plot Contamination ratios for individual susbtances  ----------------------------------------

stationlevelsSW <- c("E-N10","E-N15","E-N17","E-N20","E-N30","L-N10","L-N20","L-RF30")
stationlevelsB <- c("I-N20","I-N10")


dfSubstance <- df %>%
  mutate(log10CR=log10(CR)) %>%
  mutate(Substance.name = ifelse(nchar(Substance.name)>30,
                                 paste0(substr(Substance.name,1,30),"\n",substr(Substance.name,31,99)),
                                 Substance.name))


dfPlotSsub <-dfSubstance %>% filter(Category %in% c("Sediment"))
df_subs_S <- dfPlotSsub %>%
  ungroup() %>%
  distinct(Substance.name)%>%
  arrange(Substance.name)
df_subs_S <- df_subs_S$Substance.name
dfPlotSsub$Station <- factor(dfPlotSsub$Station,levels=stationlevelsSW)        
dfPlotSsub$Substance.name <- factor(dfPlotSsub$Substance.name,levels=df_subs_S)

dfPlotWsub <- dfSubstance %>% filter(Category %in% c("Water"))
df_subs_W <- dfPlotWsub %>%
  ungroup() %>%
  distinct(Substance.name)%>%
  arrange(Substance.name)
df_subs_W <- df_subs_W$Substance.name
dfPlotWsub$Station <- factor(dfPlotWsub$Station,levels=stationlevelsSW)        
dfPlotWsub$Substance.name <- factor(dfPlotWsub$Substance.name,levels=df_subs_W)  

dfPlotBsub <- dfSubstance %>% filter(Category=="Biota")
df_subs_B <- dfPlotBsub %>%
  ungroup() %>%
  distinct(Substance.name) %>%
  arrange(Substance.name)
df_subs_B <- df_subs_B$Substance.name
dfPlotBsub$Station <- factor(dfPlotBsub$Station,levels=stationlevelsB)        
dfPlotBsub$Substance.name <- factor(dfPlotBsub$Substance.name,levels=df_subs_B)

p4 <-ggplot(dfPlotSsub, aes(x=Year, y=log10CR)) +
  geom_hline(yintercept=0,linetype=2, color="#FF0000") +
  geom_smooth(method='lm', aes(x=Year,y=log10CR),se=FALSE, color='turquoise4') +
  geom_point(aes(x=Year,y=log10CR), colour="#000000") +
  facet_grid(Substance.name~Station,scales="free_y") +
  scale_color_manual(values=pal_class) +
  theme_ipsum() +
  labs(subtitle= "Substances in sediment") +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        strip.text.y = element_text(angle=0,size=7),
        panel.spacing = unit(1, "lines")) +
  xlab("Year") + ylab("log10(CR)")


ggsave("png/substances_Sediment.png",p4,dpi=100,units="cm",width=24,height=50)


p5 <-ggplot(dfPlotWsub, aes(x=Year, y=log10CR)) +
  geom_hline(yintercept=0,linetype=2, color="#FF0000") +
  geom_smooth(method='lm', aes(x=Year,y=log10CR),se=FALSE, color='turquoise4') +
  geom_point(aes(x=Year,y=log10CR), colour="#000000") +
  facet_grid(Substance.name~Station,scales="free_y") +
  scale_color_manual(values=pal_class) +
  theme_ipsum() +
  labs(subtitle= "Substances in water") +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        strip.text.y = element_text(angle=0,size=7),
        panel.spacing = unit(1, "lines")) +
  xlab("Year") + ylab("log10(CR)")

ggsave("png/substances_Water.png",p5,dpi=100,units="cm",width=24,height=100)


p6 <-ggplot(dfPlotBsub, aes(x=Year, y=log10CR)) +
  geom_hline(yintercept=0,linetype=2, color="#FF0000") +
  geom_smooth(method='lm', aes(x=Year,y=log10CR),se=FALSE, color='turquoise4') +
  geom_point(aes(x=Year,y=log10CR), colour="#000000") +
  facet_grid(Substance.name~Station,scales="free_y") +
  scale_color_manual(values=pal_class) +
  theme_ipsum() +
  labs(subtitle= "Substances in biota") +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        strip.text.y = element_text(angle=0,size=7),
        panel.spacing = unit(1, "lines")) +
  xlab("Year") + ylab("log10(CR)")

ggsave("png/substances_Biota.png",p6,dpi=100,units="cm",width=12,height=50)




