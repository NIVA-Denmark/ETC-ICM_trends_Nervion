library(readxl)
library(tidyverse)
library(lubridate)

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

df <- bind_rows(dfW,dfB,dfS)

df <- df %>%
  mutate(Year=year(Date)) %>%
  distinct(Year,`Sampling point`,`Matrix type`) %>%
  mutate(Colour=ifelse(Year %in% c(2000,2010,2020),1,0))


library(hrbrthemes)
library(patchwork)
library(scales)

mypal <- c("#AAAAAA","#666666")

pstn <-ggplot(df, aes(x=Year,fill=factor(Colour))) +
  geom_histogram(stat="count",width=0.5,alpha=0.5) +
  facet_grid(`Matrix type`~.) +
  theme_ipsum() +
  theme(panel.spacing = unit(1, "lines"),
        strip.text.y = element_text(angle=0,size=9)) +
  scale_y_continuous(breaks= pretty_breaks(n=3)) +
  ylab("Count of stations") +
  scale_fill_manual(guide=NULL,values=mypal)
pstn


ggsave("./png/station_count.png",pstn,dpi=300,units="cm",width=12,height=10)
ggsave("./png/station_count.png",pstn,dpi=300,units="cm",width=12,height=10)
