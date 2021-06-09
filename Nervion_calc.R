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


# ------------- load CHASE threshold values ---------------------------------------

xlfile<-"data/Nervion_CHASE_thresholds AZTI.xlsx"
dfThresholds <- read_xlsx(xlfile,sheet="Thresholds")  %>%
  filter(is.na(Exclude))


# -------------- normalisation ----------------------------------------------
dfS_Char  <- dfS %>% 
  filter(Variable %in% c("% Silt/Clay","% Organic matter")) %>%
  mutate(Variable=ifelse(Variable=="% Silt/Clay",
                         "FinePct",
                         ifelse(Variable=="% Organic matter","OrgPct",Variable))) %>%
  select(`Sampling point`,Date,Variable,Value)  %>%
  pivot_wider(names_from=Variable,values_from=Value)

dfS <- dfS %>% 
  left_join(dfS_Char,by=c("Sampling point", "Date"))
# normalize sediment to 1% TOC

# ------------- join data ---------------------------------------

df <- bind_rows(dfW,dfB,dfS)
df$ObsID <- 1:nrow(df)



dfstn <- df %>%
  mutate(Date=as.Date(`Sampling date`,tryFormats = c("%d/%m/%Y"))) %>%
  mutate(Year=year(Date)) %>%
  distinct(Year,`Sampling point`,`Matrix type`) %>%
  mutate(Colour=ifelse(Year %in% c(2000,2010,2020),1,0))


df <- df %>%
  left_join(dfThresholds,by=c("Variable"="SubstanceES","Matrix type"="Category","Species"="Species")) %>%
  filter(!is.na(ThresholdValue))
  
# correction factor to SQG* (we multiply value instead of dividing threshold)
df <- df %>%
  mutate(factor_SQG=ifelse(`Matrix type`=="Sediment" & ParamGroup=="I-MET", as.numeric(FinePct)*0.01,1)) 

# note - no normalisation of sediment conc. to 1% organic content

# correction factor for biota fresh vs dry weight
df <- df %>%
  mutate(factor_biota=ifelse(Species=="Mytilus galloprovincialis" & ThresholdBasis=="D",1/0.15,1)) %>%
  mutate(factor_biota=ifelse(Species=="Crassostrea gigas" & ThresholdBasis=="D",1/0.15,factor_biota)) %>%
  mutate(factor_biota=if_else(is.na(factor_biota),1,factor_biota))

# calculated corrected concentrations
df <- df %>%
  mutate(ValueCorr = ifelse(Operator=="<",0.5,1)*as.numeric(Value)*factor_SQG*factor_biota)


# check number of observations for each substance per station
df_count <- df %>%
  group_by(`Matrix type`,`Sampling point`,Variable) %>%
  summarise(n=n()) %>%
  ungroup() 

df_dropped <- df_count %>%
  filter(n<3)

write.table(df_dropped,"results/substances_with_few_observations.csv",sep=";",col.names=T,row.names=F)

# select only substance/station combinations where n > 3
df_count <- df_count %>%
  filter(n>=3)

df <- df_count %>%
  left_join(df,by=c("Matrix type","Sampling point","Variable"))


# take average of measurements of same parameter
df <-df %>%
  group_by(Category=`Matrix type`,Station=`Sampling point`,Date,Variable,Param,Substance,Sum=GROUP,Unit,Species,Sum,
           ThresholdValue,ThresholdUnit) %>%
  summarise(Value=mean(ValueCorr,na.rm=T),n=n(),ObsID=paste0(ObsID,collapse=",")) %>% 
  ungroup()

# now sum concentrations for thresholds with sums of different substances
df <-df %>%
  group_by(Category,Station,Date,Param,Substance,Unit,Species,
           ThresholdValue,ThresholdUnit) %>%
  summarise(Value=sum(Value,na.rm=T),n=sum(n,na.rm=T),ObsID=paste0(ObsID,collapse=",")) %>% 
  ungroup()


# unit conversion factors
dfUnits <- data.frame(Category=c("Water","Water","Biota","Biota","Sediment","Sediment"),
                      Unit=c("µg/l","mg/l","µg/kg FW","mg/kg FW","µg/kg DW","mg/kg DW"),
                      ThresholdUnit=c("µg/l","µg/l","µg/kg","µg/kg","µg/kg","µg/kg"),
                      UnitFactor=c(1,1000,1,1000,1,1000))

df <- df %>%
  left_join(dfUnits,by=c("Category","Unit","ThresholdUnit"))

df <- df %>%
  mutate(Value = Value * UnitFactor)


# take annual averages
df <- df %>%
  mutate(Year=year(Date),
         logValue=ifelse(Value<=0,-10,log(Value)))


df <- df %>% 
  group_by(Category,Station,Year,Param,Substance,
           ThresholdValue,ThresholdUnit,Species) %>%
  summarise(n=n(),Mean=mean(Value,na.rm=T),meanOfLog=mean(logValue,na.rm=T),Max=max(Value,na.rm=T),Min=min(Value,na.rm=T)) %>%
  mutate(logMean=exp(meanOfLog)) %>%
  mutate(CR=Mean/ThresholdValue) %>% 
  mutate(CRlog=logMean/ThresholdValue) 

#save(df,file="AZTI/CR_values.Rda")
#load("AZTI/CR_values.Rda")


# --------------------- Aggregation  ----------------------------------------

dfCategory <- df %>%
  ungroup() %>%
  group_by(Category,Station,Year) %>%
  summarise(sumCR=sum(CR,na.rm=T),sumCRlog=sum(CRlog,na.rm=T),n=n()) %>%
  mutate(CS=sumCR/sqrt(n),CSlog=sumCRlog/sqrt(n)) %>%
  mutate(log10CS=log10(CS),log10CSlog=log10(CSlog))


dfCategory <- dfCategory %>%
   mutate(Station=ifelse(Station %in% c("E-N30","I-N10"),"E-N30/I-N10",Station))%>%
   mutate(Station=ifelse(Station %in% c("E-N20","I-N20"),"E-N20/I-N20",Station))


dfCHASE <- dfCategory %>%
  ungroup() %>%
  pivot_wider(id_cols=c(Station,Year),names_from=Category,values_from=CS) %>%
  arrange(Station,Year)


  
dfCHASElog <- dfCategory %>%
  ungroup() %>%
  #filter(Category %in% c("Sediment","Water")) %>%
  group_by(Station,Year) %>%
  arrange(desc(CSlog)) %>%
  slice(1)


#dfCHASE %>% group_by(Station) %>% summarise(B=mean(Biota,na.rm=T),S=mean(Sediment,na.rm=T),W=mean(Water,na.rm=T))


# ---------------------  plotting ----------------------------------------


stationlevels <- c("E-N10","E-N15","E-N17","E-N20/I-N20","E-N30/I-N10","L-N10","L-N20","L-RF30")
catlevels <- c("Biota","Water","Sediment")

dfPlotCHASE <- dfCHASElog

dfPlotCHASE$Station <- factor(dfCHASElog$Station,levels=stationlevels)
dfPlotCHASE$Category <- factor(dfPlotCHASE$Category,levels=catlevels)

dfPlotCHASE <- dfPlotCHASE %>%
  mutate(Category="CHASE") %>%
  group_by(Station) %>%
  mutate(RibbonYear=ifelse(Year==min(Year,na.rm=T),1994,Year)) %>%
  mutate(RibbonYear=ifelse(Year==max(Year,na.rm=T),2020,RibbonYear))


CHASEcat<-function(CS){
  if(!is.numeric(CS)) return(NA)
  cat <- ifelse(CS<0.5,1,2)
  cat <- ifelse(CS>1,3,cat)
  cat <- ifelse(CS>5,4,cat)
  cat <- ifelse(CS>10,5,cat)
  return(cat)
}

alpha_bands<-0.4
ER0<- -2.5
ER05<-log10(0.5)
ER10<-log10(1)
ER15<-log10(5)
ER20<-log10(10)
ERmax <- 3

smooth_method <- "lm" # "loess" # 

p0 <-ggplot(dfPlotCHASE, aes(x=Year, y=CSlog)) +
  geom_hline(yintercept=0,linetype=2, color="#FF0000") +
  geom_ribbon(aes(ymin=ER0,ymax=ER05,x=RibbonYear),fill="#007eff",alpha=alpha_bands)+
  geom_ribbon(aes(ymin=ER05,ymax=ER10,x=RibbonYear),fill="#00d600",alpha=alpha_bands)+
  geom_ribbon(aes(ymin=ER10,ymax=ER15,x=RibbonYear),fill="#ffff00",alpha=alpha_bands)+
  geom_ribbon(aes(ymin=ER15,ymax=ER20,x=RibbonYear),fill="#ff8c2b",alpha=alpha_bands)+
  geom_ribbon(aes(ymin=ER20,ymax=ERmax,x=RibbonYear),fill="#ff0000",alpha=alpha_bands)+
  geom_smooth(data=,method=smooth_method, aes(x=Year,y=log10CSlog),se=TRUE, color='turquoise4') +
  geom_point(aes(x=Year,y=log10CSlog), colour="#000000") +
  facet_grid(Category~Station,scales="free_y") +
  scale_color_manual(values=pal_class) +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines")) +
  xlab("Year") + ylab("log10(CS)")

p0
ggsave("png/CHASE.png",p0,dpi=300,units="cm",width=24,height=7)

catlevels <- c("Sediment","Water","Biota")

dfPlotSWB <- dfCategory %>%
  mutate(Cat=CHASEcat(CS)) %>%
  mutate(Class=ClassNames[Cat])
dfPlotSWB$Category <- factor(dfPlotSWB$Category,levels=catlevels)
dfPlotSWB$Station <- factor(dfPlotSWB$Station,levels=stationlevels)

dfPlotSWB[nrow(dfPlotSWB)+1,"Year"]<-2020
dfPlotSWB[nrow(dfPlotSWB),"Station"]<-dfPlotSWB$Station[1]
dfPlotSWB[nrow(dfPlotSWB),"Category"]<-dfPlotSWB$Category[1]
dfPlotSWB[nrow(dfPlotSWB),"Class"]<-"High"

dfPlotSWB$Class <- factor(dfPlotSWB$Class,levels=ClassNames)


p1 <-ggplot(dfPlotSWB, aes(x=Year, y=CSlog)) +
  geom_hline(yintercept=0,linetype=2, color="#FF0000") +
  geom_smooth(method=smooth_method, aes(x=Year,y=log10CSlog),se=TRUE, color='turquoise4') +
  geom_point(aes(x=Year,y=log10CSlog), colour="#000000") +
  facet_grid(Category~Station,scales="free_y") +
  scale_color_manual(values=pal_class) +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        panel.spacing = unit(1, "lines")) +
  xlab("Year") + ylab("log10(CS)")

p1
ggsave("png/SedimentWaterBiota.png",p1,dpi=300,units="cm",width=24,height=14)



dfSubsCount <- df %>%
  group_by(Category,Year,Param,Substance) %>%
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


pcount <- pstn + p3 + plot_layout(ncol=1)
pcount

ggsave("png/obs_station_count.png",pcount,dpi=300,units="cm",width=14,height=16)


# ----------------------------------------Plot Contamination ratios for individual susbtances  ----------------------------------------

stationlevelsSW <- c("E-N10","E-N15","E-N17","E-N20","E-N30","L-N10","L-N20","L-RF30")
stationlevelsB <- c("I-N20","I-N10")


dfSubstance <- df %>%
  mutate(log10CR=log10(CR)) 


dfPlotSsub <-dfSubstance %>% filter(Category %in% c("Sediment"))
df_subs_S <- dfPlotSsub %>%
  ungroup() %>%
  distinct(Substance)%>%
  arrange(Substance)
df_subs_S <- df_subs_S$Substance
dfPlotSsub$Station <- factor(dfPlotSsub$Station,levels=stationlevelsSW)        
dfPlotSsub$Substance <- factor(dfPlotSsub$Substance,levels=df_subs_S)

dfPlotWsub <- dfSubstance %>% filter(Category %in% c("Water"))
df_subs_W <- dfPlotWsub %>%
  ungroup() %>%
  distinct(Substance)%>%
  arrange(Substance)
df_subs_W <- df_subs_W$Substance
dfPlotWsub$Station <- factor(dfPlotWsub$Station,levels=stationlevelsSW)        
dfPlotWsub$Substance <- factor(dfPlotWsub$Substance,levels=df_subs_W)  

dfPlotBsub <- dfSubstance %>% filter(Category=="Biota")
df_subs_B <- dfPlotBsub %>%
  ungroup() %>%
  distinct(Substance) %>%
  arrange(Substance)
df_subs_B <- df_subs_B$Substance
dfPlotBsub$Station <- factor(dfPlotBsub$Station,levels=stationlevelsB)        
dfPlotBsub$Substance <- factor(dfPlotBsub$Substance,levels=df_subs_B)

p4 <-ggplot(dfPlotSsub, aes(x=Year, y=log10CR)) +
  geom_hline(yintercept=0,linetype=2, color="#FF0000") +
  geom_smooth(method=smooth_method, aes(x=Year,y=log10CR),se=TRUE, color='turquoise4') +
  geom_point(aes(x=Year,y=log10CR), colour="#000000") +
  facet_grid(Substance~Station,scales="free_y") +
  scale_color_manual(values=pal_class) +
  theme_ipsum() +
  labs(subtitle= "Substances in sediment") +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        strip.text.y = element_text(angle=0,size=7),
        panel.spacing = unit(1, "lines")) +
  xlab("Year") + ylab("log10(CR)")


ggsave("png/substances_Sediment.png",p4,dpi=100,units="cm",width=24,height=56)


p5 <-ggplot(dfPlotWsub, aes(x=Year, y=log10CR)) +
  geom_hline(yintercept=0,linetype=2, color="#FF0000") +
  geom_smooth(method=smooth_method, aes(x=Year,y=log10CR),se=TRUE, color='turquoise4') +
  geom_point(aes(x=Year,y=log10CR), colour="#000000") +
  facet_grid(Substance~Station,scales="free_y",labeller=label_wrap_gen(width=20)) +
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
  geom_smooth(method=smooth_method, aes(x=Year,y=log10CR),se=TRUE, color='turquoise4') +
  geom_point(aes(x=Year,y=log10CR), colour="#000000") +
  facet_grid(Substance~Station,scales="free_y") +
  scale_color_manual(values=pal_class) +
  theme_ipsum() +
  labs(subtitle= "Substances in biota") +
  theme(axis.text.x=element_text(angle=90,vjust=0.5,hjust=1),
        strip.text.y = element_text(angle=0,size=7),
        panel.spacing = unit(1, "lines")) +
  xlab("Year") + ylab("log10(CR)")

ggsave("png/substances_Biota.png",p6,dpi=100,units="cm",width=10,height=40)




