## Script to look at Cres E vs. Cres W nutrient, ions, DOC, DO and CO2 data from 2021-2022 and 2022-2023 field seasons
## Created by ATW 5/2/23

## TO DO

####Set up workspace
rm(list=ls())
options(stringsAsFactors = FALSE)
require (magrittr)
library(tidyverse)
library(dplyr)
library(lubridate)
library (ggplot2)
library(readxl)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##input and output location for files
drive<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/MDV streams/'
outdrive<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/MDV streams/Crescent/'
DOdrive<-'/Users/annawright/Library/CloudStorage/OneDrive-UCB-O365/Stream Metabolism/2021_2022_data_ATW/'

##load 21-22 data in data
DOC21<-read_excel(paste0(drive, '2021-22 DOC streams.xlsx'))
DOC21$Date<-as.Date(DOC21$DateTime)
Nut21<-read_excel(paste0(drive, 'Streams NUTS 2021-22.xlsx'))
Ion21<-read_excel(paste0(drive, 'Streams IC 2021-22.xlsx'))

##Add together, filter out Cres E and W and normal Cres 
Nut21$Date[is.na(Nut21$Date)]<-'2022-01-18'
All21<-full_join(DOC21, Nut21, by=c("Stream", "Date"))
All21<-full_join(All21, Ion21, by=c("Stream", "Date"))
All21$Season<-'2021'

Cres21<-All21%>%
  dplyr::filter(., Stream=='Crescent'|Stream=='Crescent West'|Stream=='Crescent East')
Cres21$Date<-as.Date(Cres21$Date)

##load 22-23 data in data
DOC22<-read_excel(paste0(drive, 'Stream NPOC 2022-23.xlsx'))
DOC22$Date<-lubridate::ymd(DOC22$`Date Collected`)
DOC22$Season<-'2022'
Nut22<-read_excel(paste0(drive, 'Fryxell stream nutrients 2022-23.xlsx'))
Nut22$Date<-lubridate::mdy(Nut22$Date)
Nut22$Season<-'2022'
Ion22<-read_excel(paste0(drive, 'Streams IC 2022-23_wSi.xlsx'))
Ion22$Date<-lubridate::ymd(Ion22$date)
Ion22$Season<-'2022'

All22<-full_join(DOC22, Nut22, by=c("Stream", "Season", "Date"))
All22<-full_join(All22, Ion22, by=c("Stream", "Season", "Date"))

Cres22<-All22%>%
  dplyr::filter(., Stream=='Crescent'|Stream=='Crescent West'|Stream=='Crescent East')
Cres22$`NH4 mg/L N`<-(Cres22$`NH4 µg N/L`/1000)
Cres22$`SRP mg/L`<-(Cres22$`SRP µg P/L`/1000)
### need to update this to NO3 when we get correct data from Kathy 
Cres22$`N+N mg/L of N`<-(Cres22$`Nitrate + nitrite - Results [µg N/liter]`/1000)

### select only a few for the sake of the poster 
Cres21<-Cres21%>%
  dplyr::select(., Stream, Date, Season, `SRP mg/L`, `NH4 mg/L N`, `N+N mg/L of N`, `NPOC mg/L`, `F mg/L`, `Cl mg/L`,`Li mg/L`,  `SO4 mg/L`, `Na mg/L`, `K mg/L`, `Mg mg/L`, `Ca mg/L`, `Si mg/L`)
# Cres21<-Cres21%>%
#   dplyr::select(., Stream, Date, Season, `SRP mg/L`, `N+N mg/L of N`, `Cl mg/L`,`Na mg/L`, `K mg/L`, `Mg mg/L`, `Ca mg/L`, `Si mg/L`)
Cres22<-Cres22%>%
  dplyr::rename(., `NPOC mg/L`=`[DOC]mg/L (data pasted as values)`)%>%
  dplyr::select(., Stream, Date, Season, `SRP mg/L`, `N+N mg/L of N`, `NH4 mg/L N`, `NPOC mg/L`, `F mg/L`, `Cl mg/L`, `Na mg/L`,  `Li mg/L`, `K mg/L`,`SO4 mg/L`, `Mg mg/L`, `Ca mg/L`, `Si mg/L`)
Cres<-full_join(Cres21,Cres22)

Cres2<-Cres%>% gather("variable", "concentration mg/L", `SRP mg/L`, `N+N mg/L of N`, `Cl mg/L`,  `Na mg/L`, `K mg/L`, `Mg mg/L`, `Ca mg/L`, `Si mg/L`, `Li mg/L`, `SO4 mg/L`)

Cres2$`concentration mg/L`<-as.numeric(Cres2$`concentration mg/L`)

library(ggbreak)
##initial plot to just look at them all vs. date
AllPlot<-ggplot(Cres2, aes(y=`concentration mg/L`, x=Date, color=group, fill=group)) + 
  geom_point(size=6)+geom_line()+
  facet_grid(`variable`~ ., scales = "free_y")+
  ggtitle('Stream Chemistry and DOC')+
  # theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 40))+
  theme(legend.position="right")+
  scale_x_break(c(as.Date('2022-02-01'), as.Date('2022-12-01')))+
  scale_x_date(limits = as.Date(c('2021-12-01','2023-01-30')))
ggsave(AllPlot, filename="AllCresChem.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 32, height = 25)

##Take averages over both whole seasons #################################################################################
Cres3<-Cres%>%
  group_by(Stream)%>%
  summarise(F_mgL=mean(`F mg/L`, na.rm=TRUE), Cl_mgL=mean(`Cl mg/L`, na.rm=TRUE), SO4_mgL=mean(`SO4 mg/L`, na.rm=TRUE), Na_mgL=mean(`Na mg/L`, na.rm=TRUE),
          K_mgL=mean(`K mg/L`, na.rm=TRUE), Mg_mgL=mean(`Mg mg/L`, na.rm=TRUE), Ca_mgL=mean(`Ca mg/L`, na.rm=TRUE), Li_mgL=mean(`Li mg/L`, na.rm=TRUE), SRP_mgL=mean(`SRP mg/L`, na.rm=TRUE),
          N_mgL=mean(`N+N mg/L of N`, na.rm=TRUE), NH4_mgL=mean(`NH4 mg/L N`,  na.rm=TRUE), DOC_mgL=mean(`NPOC mg/L`, na.rm=TRUE), Si_mgL=mean(`Si mg/L`, na.rm=TRUE))

Cres4<-Cres3%>% gather("variable", "concentration mg/L", `F_mgL`, `Cl_mgL`,  `SO4_mgL`, `Li_mgL`,
                      `Na_mgL`, `K_mgL`, `Mg_mgL`, `Ca_mgL`, `SRP_mgL`, `N_mgL`, NH4_mgL, `Si_mgL`)


##Package with colorblind friendly colors
library(ggpubfigs)
# using the "ito_seven" color palette and theme_big_simple()
ggplot(mtcars, aes(factor(carb), fill=factor(cyl))) + geom_bar() + scale_fill_manual(values = friendly_pal("ito_seven")) + theme_big_simple()

AvgPlot<-ggplot(Cres4, aes(y=`concentration mg/L`, x=Stream, color=Stream, fill=Stream)) + 
  geom_bar(stat="identity", position="dodge2")+
  ##geom_errorbar(aes(color=season))+
  facet_wrap(`variable`~ ., scales = "free_y")+
  scale_fill_manual(values = friendly_pal("ito_seven"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
  ggtitle('')+
  xlab('')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 50))+
  theme(legend.position="")
ggsave(AvgPlot, filename="Avg_CresChem.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 25, height = 20)

## do a t-test to see if the chemistry between the 3 streams are actually different
library(tatest)
library(rempsyc)

# ## to do them all together
Cres4<-Cres%>%
  dplyr::filter(., Stream!='Crescent')%>%
  dplyr::select(., Stream, "Ca mg/L", "Na mg/L","K mg/L","Mg mg/L","Cl mg/L",
                "SRP mg/L","N+N mg/L of N", "NH4 mg/L N", "Si mg/L")%>%
  dplyr::rename(., Ca_mgL="Ca mg/L",Na_mgL="Na mg/L", K_mgL="K mg/L",Mg_mgL="Mg mg/L",Cl_mgL="Cl mg/L",
                SRP_mgL="SRP mg/L",N_mgL="N+N mg/L of N", NH4_mgL="NH4 mg/L N", Si_mgL="Si mg/L")
test<-nice_t_test(
  data = Cres4,
  response = names(Cres4)[2:10],
  group = "Stream",
  warning = FALSE
)
##put into a table
table1<-nice_table(test)

# Save in Word
flextable::save_as_docx(table1, path = paste0(outdrive, "21_23_EandW_t-tests.docx"))

### Find the Si:K ratio for each branch 
Cres$`K_mglw`=(Cres$`K mg/L`*0.98)
Cres$K_mol<-(Cres$K_mglw/39.0983)
Cres$Si_mol<-(Cres$`Si mg/L`/28.06)
Cres$SiK_ratio<-(Cres$Si_mol/Cres$K_mol)

Ratio<-Cres%>%
  group_by(Stream)%>%
  summarise(., Avg_SiK_ratio=mean(SiK_ratio, na.rm=TRUE))

RaPlot<-ggplot(Ratio, aes(y=`Avg_SiK_ratio`, x=Stream, color=Stream, fill=Stream)) + 
  geom_bar(stat="identity", position="dodge2")+
  ##geom_errorbar(aes(color=season))+
  # facet_wrap(`variable`~ ., scales = "free_y", nrow=3)+
  ggtitle('')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 50))+
  theme(legend.position="")
ggsave(RaPlot, filename="Avg_EW_Ratio.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 30, height = 25)


### Compare with F8 Long-term averages of stream chemistry ##################################################################
##### average for crescent before and after 2012
ion<-read.csv(paste0(drive, 'mcmlter-strm-ions-20220201.csv'))
nut<-read.csv(paste0(drive, 'mcmlter-strm-nutrients-20220201.csv'))
# doc<-read.csv(paste0(drive, 'mcmlter-strm-doc-20220201.csv'))

ion$Stream[ion$location=='Crescent Stream at F8']<-'Crescent'
ion<-ion%>%
  dplyr::filter(., Stream=='Crescent')
ion$DateTime<-lubridate::mdy_hm(ion$date_time)
nut$Stream[nut$location=='Crescent Stream at F8']<-'Crescent'
nut<-nut%>%
  dplyr::filter(., Stream=='Crescent')
nut$DateTime<-lubridate::mdy_hm(nut$date_time)
# doc$Stream[doc$location=='Crescent Stream at F8']<-'Crescent'
# doc<-doc%>%
#   dplyr::filter(., Stream=='Crescent')
# doc$DateTime<-lubridate::mdy_hm(doc$date_time)
all1<-full_join(ion, nut, by=c('Stream', 'DateTime'))
# all<-full_join(all, doc, by=c('Stream', 'DateTime'))
all1$Date<-as.Date(all1$DateTime)

all1$year<-lubridate::year(all1$Date)
all1$month<-lubridate::month(all1$Date)
all1$year2<-(all1$year -1)
all1$season<-ifelse(all1$month>=10, all1$year, all1$year2)
all1$n_no2_ugl[is.na(all1$n_no2_ugl)]<-0
all1$`n+n_ugl`=(all1$n_no2_ugl+all1$n_no3_ugl)

##add in the newest ion, nut and doc data
Cres5<-Cres%>%
  dplyr::rename(., na_mgl="Na mg/L", k_mgl="K mg/L",mg_mgl="Mg mg/L",cl_mgl="Cl mg/L",ca_mgl="Ca mg/L", li_mgl="Li mg/L", so4_mgl="SO4 mg/L",
                srp_mgl="SRP mg/L",`n+n_mgl`="N+N mg/L of N", si_mgl="Si mg/L", season="Season")
Cres5$season<-as.numeric(Cres5$season)
Cres5<-Cres5%>%
  dplyr::filter(., Stream=='Crescent')
Cres5$Date[is.na(Cres5$Date)]<-'2022-01-18'

all1$srp_mgl=(all1$srp_ugl/1000)
all1$`n+n_mgl`=(all1$`n+n_ugl`/1000)
# Cres5$n_no2_ugl=(Cres5$n_no2_mgl/1000)
all1$n_nh4_mgl=(all1$n_nh4_ugl/1000)
all1<-full_join(all1, Cres5)

all1$year<-lubridate::year(all1$Date)
## add in groups but make another copy of it before the groups
all2<-all1
all2$group<-'all'

all1$group[all1$year<2012]<-'a pre 2012'
all1$group[all1$year>=2012]<-'b post 2012'

all<-all1%>%
  dplyr::select(., 'Stream', 'DateTime', 'Date', 'year', 'season', 'group', "na_mgl","k_mgl","mg_mgl", "cl_mgl","si_mgl", "ca_mgl", "li_mgl", "so4_mgl",
                "srp_mgl","n+n_mgl")%>%
  dplyr::filter(., !is.na(group))
## remove no3 outliers 
all<-all%>%
  dplyr::filter(., `n+n_mgl`<0.3)
all$`n+n_mgl`[all$`n+n_mgl`<0]=0

all3<-all%>%gather(., variable, `concentration (mg/l)`, "na_mgl","k_mgl","mg_mgl","cl_mgl","si_mgl", "ca_mgl", "li_mgl",
                    "srp_mgl","n+n_mgl", "so4_mgl")

###Box plot
AllPlot<-ggplot(all3, aes(y=`concentration (mg/l)`, x=group, fill=group)) + 
  geom_boxplot(lwd=3)+
  facet_wrap(`variable`~ ., scales = "free_y")+
  scale_fill_manual(values = friendly_pal("ito_seven"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
  ggtitle('')+
  # theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 50))+
  theme(legend.position="bottom")
ggsave(AllPlot, filename="2012_CresChem_Boxplot.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 25, height = 15)

## do a t-test to see if they are actually different pre and post 2012
## modified t-test: "Performs a modified version of the t test to assess the correlation between two spatial processes."
## unpaired t-test -- test whether each population has the same mean 
## Welch's t-test: when the variances and sample size are unequal
library(tatest)
library(rempsyc)
## to do each variable individually
# SRP<-all2%>%dplyr::filter(., variable=='srp_ugl')
# t.test(concentration ~ group, data = SRP)
# sjPlot:: tab_model(K_t)

##want to compare the pre-2012, the post 2012, and the all time, so put them all together here
alldata<-full_join(all1, all2)

## to do them together
alldata<-alldata%>%
  rename(., n_mgl=`n+n_mgl`)
alldata<-alldata%>%
  dplyr::select(., group, "na_mgl","k_mgl","mg_mgl","cl_mgl","f_mgl", "si_mgl", "ca_mgl", "li_mgl", "so4_mgl",
                "srp_mgl","n_mgl","n_nh4_mgl")
## need to do multiple t-test between each one
## Pre 2012 and all
alldata1<-alldata%>%
  filter(., group!="b post 2012")

test1<-nice_t_test(
  data = alldata1,
  response = names(alldata1)[2:13],
  group = "group",
  warning = FALSE
)
##put into a table
Alltable1<-nice_table(test1)
# Save in Word
flextable::save_as_docx(Alltable1, path = paste0(outdrive, "pre2012_all_t-tests.docx"))

### Post 2012 and all 
alldata2<-alldata%>%
  filter(., group!="a pre 2012")

test2<-nice_t_test(
  data = alldata2,
  response = names(alldata2)[2:13],
  group = "group",
  warning = FALSE
)
##put into a table
Alltable2<-nice_table(test2)
# Save in Word
flextable::save_as_docx(Alltable2, path = paste0(outdrive, "post2012_all_t-tests.docx"))

### Pre and post 2012
alldata3<-alldata%>%
  filter(., group!="all")

test3<-nice_t_test(
  data = alldata3,
  response = names(alldata3)[2:13],
  group = "group",
  warning = FALSE
)
##put into a table
Alltable3<-nice_table(test3)
# Save in Word
flextable::save_as_docx(Alltable3, path = paste0(outdrive, "2012pre_post_t-tests.docx"))

alldata<-alldata%>% gather("variable", "concentration mg/L", "na_mgl", "k_mgl","mg_mgl", "f_mgl", "cl_mgl","si_mgl","ca_mgl","li_mgl","so4_mgl",
                           "srp_mgl","n_mgl","n_nh4_mgl")

## Plot a bar chart with the averages of all 3 populations together
AllPlot<-ggplot(alldata, aes(y=`concentration mg/L`, x=group, fill=group)) + 
  # geom_bar(stat="identity", position="dodge2")+
  geom_boxplot(lwd=3)+
  facet_wrap(`variable`~ ., scales = "free_y")+
  ##geom_errorbar(aes(color=season))+
  facet_wrap(`variable`~ ., scales = "free_y")+
  scale_fill_manual(values = friendly_pal("ito_seven"))+
  theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
  ggtitle('')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 50))+
  theme(legend.position="")
ggsave(AllPlot, filename="AllPrePost_Avgs.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 30, height = 25)


### look at the N:P ratio change between pre 2012 and after
# all$NP<-((all$n_no3_ugl + all$n_no2_ugl + all$n_nh4_ugl) /all$srp_ugl)
# 
# AllNP<-ggplot(all, aes(y=NP, x=group, color=group, fill=group)) +
#   geom_boxplot()+
#   # geom_errorbar( aes(x=group, ymin=min, ymax=max), color='black')+
#   # facet_wrap(`variable`~ ., scales = "free_y")+
#   ggtitle('Avg NP ratio before and after 2012')+
#   theme(axis.text.x = element_text(angle = 90))+
#   theme(legend.position="bottom")
# ggsave(AllNP, filename="2012_CresNP_boxplot.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 10, height = 10)
# 
# t.test(NP ~ group, data = all)
# sjPlot:: tab_model(K_t)

### plot the concentrations over time  --- all the concentrations by date
## colog by the events
all2$color<-'long-term'
all2$color[all2$Date>='2021-12-17'&all2$Date<='2022-01-01']<-'2021 event'
all2$color[all2$Date>='2012-01-18'&all2$Date<='2012-01-24']<-'2012 event'

##color it by season instead
all2$color<-'other season'
all2$color[all2$Date>='2012-01-17'&all2$Date<='2012-12-31']<-'2012/2013'
all2$color[all2$Date>='2021-12-17'&all2$Date<='2022-01-30']<-'2021'

##filter out only a few, one of each category
all2<-all2%>%
  filter(., variable=='a na_mgl'|variable=='b si_mgl'|variable=='c n+n_mgl')

ConPlot1<-ggplot(all2, aes(y=`concentration (mg/l)`, x=Date)) + 
  geom_point(aes(color=color, size=3))+
  # geom_line(aes())+
  # geom_smooth()+
  stat_poly_line(method="lm") +
  stat_poly_eq(use_label(c("P", "R2")), size=10)+
  # scale_y_continuous(trans='log10')+
  facet_wrap(`variable`~ ., scales = "free_y", nrow=3)+
  ggtitle('')+
  # theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 50))+
  theme(legend.position="bottom")
ggsave(ConPlot1, filename="All_concentration.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 20, height = 15)

## avg concentration by season
all3<-all%>%
  group_by(season)%>%
  summarise(., Ca_mgl=mean(`ca_mgl`, na.rm=TRUE), Na_mgl=mean(`na_mgl`, na.rm=TRUE), K_mgl=mean(`k_mgl`, na.rm=TRUE), Mg_mgl=mean(`mg_mgl`, na.rm=TRUE),
            Cl_mgl=mean(`cl_mgl`, na.rm=TRUE), Si_mgl=mean(`si_mgl`, na.rm=TRUE),
            `N+N_mgNl`=mean(`n+n_mgl`, na.rm=TRUE), SRP_mgl=mean(`srp_mgl`, na.rm=TRUE))
##color it by season instead
all3$color<-'other season'
all3$color[all3$season=='2012'|all3$season=='2013']<-'2012/2013'
all3$color[all3$season=='2021']<-'2021'
all3<-all3%>%gather(., variable, `concentration mg/L`, "Ca_mgl", "Na_mgl","K_mgl","Mg_mgl","Cl_mgl","Si_mgl",
                     "SRP_mgl","N+N_mgNl","NH4_mgNl")
all3$group[all3$variable=='SRP_mgl'|all3$variable=='NH4_mgNl'|all3$variable=='N+N mgNL']<-'Nutrients'
all3$group[all3$variable=='Na_mgl'|all3$variable=='Cl_mgl'|all3$variable=='Mg_mgl']<-'Sea salts'
all3$group[all3$variable=='Si_mgl'|all3$variable=='Ca_mgl'|all3$variable=='K_mgl']<-'Weathering ions'

ConPlot2<-ggplot(all3, aes(y=`concentration mg/L`, x=season)) + 
  geom_point(aes(color=color, size=4))+
  # geom_line()+
  geom_smooth(method='lm')+
  stat_poly_line(method="lm") +
  stat_poly_eq(use_label(c("P", "R2")), size=10)+
  facet_wrap(`variable`~ ., scales = "free_y", nrow=3)+
  ggtitle('')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 35))+
  theme(legend.position="bottom")
ggsave(ConPlot2, filename="Avg_concentration.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 25, height = 20)


## Find the average Si:K ratio to look at weathering vs. biological uptake of Si
## mass of Si 28.06
## mass of K 39.0983

##estimated from Gooseff et al., 2002 that 98% of K comes from weathering of Si while 2% comes from marine aerosols
all2$k_mglw=(all2$`k_mgl`*0.98)
all2$K_mol<-(all2$k_mglw/39.0983)
all2$Si_mol<-(all2$`si_mgl`/28.06)
all2$SiK_ratio<-(all2$Si_mol/all2$K_mol)

##color it by season instead
all2$color<-'other season'
all2$color[all2$Date>='2012-01-17'&all2$Date<='2012-12-31']<-'2012/2013'
all2$color[all2$Date>='2021-12-17'&all2$Date<='2022-01-30']<-'2021'

RatioPlot<-ggplot(all2, aes(y=`SiK_ratio`, x=Date)) + 
  geom_point(aes(size=4, color=color))+
  # geom_line()+
  geom_smooth(method='lm')+
  stat_poly_line(method="lm") +
  stat_poly_eq(use_label(c("P", "R2")), size=10)+
  # facet_wrap(`variable`~ ., scales = "free_y", nrow=3)+
  ggtitle('')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 35))+
  theme(legend.position="bottom")
ggsave(RatioPlot, filename="SiK_ratio.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 30, height = 15)

all3$K_mglw=(all3$K_mgl*0.98)
all3$K_mol<-(all3$K_mglw/39.0983)
all3$Si_mol<-(all3$Si_mgl/28.06)
all3$Avg_SiK_ratio<-(all3$Si_mol/all3$K_mol)

all3$group[all3$season<2012]<-'a pre 2012'
all3$group[all3$season>=2012]<-'b post 2012'

RatioAvg<-ggplot(all3, aes(y=`Avg_SiK_ratio`, x=season, group=group)) + 
  geom_point(aes(size=5))+
  # geom_line()+
  geom_smooth(method='lm')+
  stat_poly_line(method="lm") +
  stat_poly_eq(use_label(c("P", "R2")), size=12)+
  # facet_wrap(`variable`~ ., scales = "free_y", nrow=3)+
  ggtitle('')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 45))+
  theme(legend.position="bottom")
ggsave(RatioAvg, filename="SiK_ratio_Avg.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 30, height = 15)

### Do the same thing for N to Si ratio
all2<-all2%>%
  dplyr::filter(., `n+n_mgl`<0.3)
all2$`n+n_mgl`[all2$`n+n_mgl`<0]=0
all2$Tot_N=(all2$`n_nh4_mgl` + all2$`n+n_mgl`)
all2$N_mol<-(all2$Tot_N/14.01)
all2$NSi_ratio<-(all2$N_mol/all2$Si_mol)
all2$SiN_ratio<-(all2$Si_mol/all2$N_mol)

# Ratio<-Cres%>%
#   group_by(Stream)%>%
#   summarise(., Avg_SiK_ratio=mean(SiK_ratio, na.rm=TRUE))

RaPlot2<-ggplot(all2, aes(y=`NSi_ratio`, x=Date, fill=Stream)) + 
  geom_point(size=7)+
  geom_smooth(method="lm")+
  # facet_wrap(`variable`~ ., scales = "free_y", nrow=3)+
  ggtitle('')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 50))+
  theme(legend.position="")
ggsave(RaPlot2, filename="NSi_Ratio.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 30, height = 25)

RaPlot3<-ggplot(all2, aes(y=`SiN_ratio`, x=Date, fill=Stream)) + 
  geom_point(size=7)+
  geom_smooth(method="lm")+
  # facet_wrap(`variable`~ ., scales = "free_y", nrow=3)+
  ggtitle('')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 50))+
  theme(legend.position="")
ggsave(RaPlot3, filename="SiN_Ratio.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 30, height = 25)


### loook at long-term Crescent EC data
EC<-read.csv(paste0(drive, 'Gage Data/mcmlter-strm-f8_crescent-15min-20210105.csv'))

##add in 2022 and 2023 data
EC2<-read.csv(paste0(drive, 'Gage Data/F8_2122A_SUBM.csv'))
EC3<-read.csv(paste0(drive, 'Gage Data/F8_2122B_SUBM.csv'))
EC4<-read.csv(paste0(drive, 'Gage Data/F8_2223A_SUBM.csv'))
EC5<-read.csv(paste0(drive, 'Gage Data/F8_2223B_SUBM.csv'))
EC_2<-full_join(EC2, EC3)
EC_2<-full_join(EC_2, EC4)
EC_2<-full_join(EC_2, EC5)

EC<-full_join(EC, EC_2)
EC$DATE_TIME<-lubridate::mdy_hm(EC$DATE_TIME)
EC$Date<-as.Date(EC$DATE_TIME)

EC$year<-lubridate::year(EC$Date)
EC$group[EC$year<2012]<-'pre 2012'
EC$group[EC$year>=2012]<-'post 2012'

EC<-EC%>%dplyr::filter(., !is.na(CONDUCTIVITY))%>%
  dplyr::filter(., CONDUCTIVITY<500&CONDUCTIVITY>0)

ECplot1<-ggplot(EC, aes(x=group, y=`CONDUCTIVITY`))+
  geom_boxplot()+
  # geom_errorbar( aes(x=group, ymin=min, ymax=max), color='black')+
  # facet_wrap(`MAT.TYPE`~ ., scales = "free_y")+
  ggtitle('Avg EC before and after 2012')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 50))+
  theme(legend.position="bottom")
ggsave(ECplot1, filename="2012_CresEC_boxplot.jpeg", device="jpeg", path=paste0(outdrive), width = 25, height = 25)


EC2<-EC%>%
  dplyr::group_by(group)%>%
  dplyr::summarise(., EC_med=median(CONDUCTIVITY, na.rm=TRUE))
 
ECplot<-ggplot(EC2, aes(x=group, y=`EC_med`))+
  geom_bar(stat="identity", position="dodge2")+
  # geom_errorbar( aes(x=group, ymin=min, ymax=max), color='black')+
  # facet_wrap(`MAT.TYPE`~ ., scales = "free_y")+
  ggtitle('Median EC before and after 2012')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 35))+
  theme(legend.position="bottom")
ggsave(ECplot, filename="2012_CresEC.jpeg", device="jpeg", path=paste0(outdrive, 'Plots/'), width = 25, height = 25)

### Plot the probabily density funciton and highlight where these events lie
### add in coloring for the days around these events
EC$color<-'long-term'
EC$color[EC$Date>='2021-12-17'&EC$Date<='2021-12-22']<-'2021 event'
EC$color[EC$Date>='2012-01-18'&EC$Date<='2012-01-21']<-'2012 event'
library(ggbreak) 
ECdens<-ggplot()+geom_density(data=EC, aes(CONDUCTIVITY, color=color, fill=color), alpha=0.3)+
  # geom_density(data=Avg, aes(GPPavg_gC), color='red', fill='red', alpha=0.3)+
  # scale_x_break(c(5, 30))+
  # xlim(0, 7.5)+
  theme(text = element_text(size=50))
ggsave(ECdens, filename="EC_density plot.jpeg", device="jpeg", path=paste0(outdrive, 'Plots/'), width = 18, height = 15)

###Plot all EC over time
ECtime<-ggplot(EC, aes(x=DATE_TIME, y=CONDUCTIVITY))+geom_point(aes(color=color, size=3))+
  scale_y_continuous(trans='log10')+
  # facet_wrap(`variable`~ ., scales = "free_y", nrow=3)+
  # ggtitle('Avg Chem before and after 2012')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 35))+
  theme(legend.position="bottom")
ggsave(ECtime, filename="All_ECovertime.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 25, height = 20)

  

## t-test on EC
ECttest<-t.test(CONDUCTIVITY ~ group, data = EC)
sjPlot:: tab_model(ECttest)
ECtable<-nice_table(ECttest)
flextable::save_as_docx(ECttest, path = paste0(outdrive, "EC_2012_t-tests.docx"))

##put into a table
table2<-nice_table(ECttest)
# Save in Word
flextable::save_as_docx(table2, path = paste0(outdrive, "Flux_t-tests.docx"))

###PLOT EC during the 21-22 season before and after the other thermokarst degredation
EC2<-read.csv(paste0(drive, 'Gage Data/F8_2122A_SUBM.csv'))
EC3<-read.csv(paste0(drive, 'Gage Data/F8_2122B_SUBM.csv'))
EC_2<-full_join(EC2, EC3)
EC_2$DateTime<-lubridate::mdy_hm(EC_2$DATE_TIME)
EC_2<-EC_2%>%dplyr::filter(., CONDUCTIVITY>0)

##plot EC over time for 21-22 season
EC21<-ggplot(EC_2, aes(x=DateTime, y=CONDUCTIVITY))+geom_point()+geom_line()+
  ggtitle('Conductivity during 21-22 flow season')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 40))
ggsave(EC21, filename="2021_Crescent_EC.jpeg", device="jpeg", path=paste0(outdrive), width = 25, height = 20)


######################################################################################################################
#### ION CHEMOSTASIS -- BEFORE AND AFTER 2012???
#### READ IN LONG-TERM Q DATA 
Q1<-read.csv(paste0(drive, 'Gage Data/mcmlter-strm-f8_crescent-15min-20210105.csv'))
Q1$DATE_TIME<-lubridate::mdy_hm(Q1$DATE_TIME)
Q1<-Q1%>%rename(., DISCHARGE_RATE=`DISCHARGE.RATE`)
Q2<-read.csv(paste0(drive, 'Gage Data/F8_2122A_SUBM.csv'))
Q2$DATE_TIME<-lubridate::mdy_hm(Q2$DATE_TIME)
Q3<-read.csv(paste0(drive, 'Gage Data/F8_2223A_SUBM.csv'))
Q3$DATE_TIME<-lubridate::mdy_hm(Q3$DATE_TIME)

AllQ<-full_join(Q1, Q2)
AllQ<-full_join(AllQ, Q3)
AllQ<-AllQ[order(AllQ$DATE_TIME),]
## From Torrens and Wlostowski, convert to Darcy's velocity q by: q=Q/A
## A = (wc * 2whz) *zthaw
## where Ax = the cross-sectional stream area (m2), wc = the stream channel width (m), whz = the width of the hyporheic zone (m), 
## and zthaw = the thawed active layer depth (m; Wlostowski et al., 2018). 
##We assume that wc = 4m, whz = 5 m, and zthaw = 1m based on prior observations of channel and hyporheic extent (Northcott et al., 2009) 
##and maximum annual thaw depth below streams (Conovitz et al., 2006).
## SO A = 14
## Find total flow per day (m3/day) then divide by 14? Use trapezoidal method to get total for the day
AllQ$Date<-as.Date(AllQ$DATE_TIME)
AllQ$year<-lubridate::year(AllQ$Date)
AllQ$month<-lubridate::month(AllQ$Date)
AllQ$year2<-(AllQ$year -1)
AllQ$season<-ifelse(AllQ$month>=10, AllQ$year, AllQ$year2)
AllQ$DateTime_2<-lag(AllQ$DATE_TIME)
AllQ<-AllQ%>%
  mutate(difftime = difftime(DATE_TIME, DateTime_2, unit = 'secs'))
AllQ<-AllQ%>%
  dplyr::filter(., difftime<86400)
AllQ$Q_2=lag(AllQ$DISCHARGE_RATE)
AllQ<-AllQ%>%
  group_by(season)%>%
  mutate(Q_tot=(DISCHARGE_RATE + Q_2)/2 * (as.numeric(difftime)))
###Sumarrize by day, month, and season
AllQday<-AllQ%>%
  group_by(Date) %>% 
  summarise(Qtot = sum(Q_tot, na.rm=TRUE), season=season)
AllQday<-distinct(AllQday)
### convert to m3/day
AllQday$Qtot_m3<-(AllQday$Qtot/1000)
AllQday$q_md<-(AllQday$Qtot_m3/14)

##Plot Q over time just to see
Qplot<-ggplot(AllQday, aes(x=Date, y=Qtot))+
  geom_point()+geom_line()
AllQseason<-AllQday%>%
  group_by(season)%>%
  summarise(Qtot_seas=sum(Qtot, na.rm=TRUE))
### conver to m3
AllQseason$Qtot_m3=AllQseason$Qtot_seas/1000
Qplot<-ggplot(AllQseason, aes(x=season, y=Qtot_m3))+
  geom_bar(stat="identity", position="dodge2", color="blue", fill="lightblue")+
  # stat_poly_line(method="lm") +
  # stat_poly_eq(use_label(c("p")), size=10)+
  # geom_errorbar( aes(x=group, ymin=min, ymax=max), color='black')+
  # facet_wrap(`MAT.TYPE`~ ., scales = "free_y")+
  ggtitle('')+
  ylab(expression(Total~Discharge~(m^3)))+
  xlab('Flow Season')+
  # theme(axis.text.x = element_text(angle = 90))+
  # theme(text = element_text(size = 45))+
  # theme(legend.position="bottom")+
  theme_minimal()+
  theme(plot.title = element_blank(),
        axis.text = element_text(size = 55),
        axis.title = element_text(size = 55),
        axis.text.x = element_text(angle = 90),
        # axis.title.x = element_blank(),
        strip.background = element_blank(),
        # strip.text = element_text(size = 20),
        # legend.text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 55))
ggsave(Qplot, filename="Crescent_seasonalQ.jpeg", device="jpeg", path=paste0(outdrive, 'Plots/'), width = 25, height = 25)

### Look at difference between average flows pre and post 2012
AllQseason$group[AllQseason$season<2012]<-'a pre 2012'
AllQseason$group[AllQseason$season>=2012]<-'b post 2012'

Qplot2<-ggplot(AllQseason, aes(x=group, y=Qtot_m3, color=group))+
  geom_boxplot(linewidth=3)+
  ggtitle('')+
  ylab(~Total~Season~Discharge~(m^3))+
  xlab('')+
  theme_minimal()+
  theme(plot.title = element_blank(),
        axis.text = element_text(size = 45),
        axis.title = element_text(size = 45),
        axis.text.x = element_text(angle = 90),
        # axis.title.x = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 20),
        lengend.position="none",
        # legend.text = element_text(size = 16),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 45))
ggsave(Qplot2, filename="Crescent_2012Q_boxplot.jpeg", device="jpeg", path=paste0(outdrive, 'Plots/'), width = 25, height = 25)


##Find the average seasonal discharge before and after 2012
QAvg<-AllQseason%>%
  group_by(group)%>%
  summarise(., Avg_Qtot=mean(Qtot_m3, na.rm=TRUE))

Qplot2<-ggplot(QAvg, aes(x=group, y=Avg_Qtot))+
  geom_bar(stat="identity", position="dodge2", color="blue", fill="lightblue")+
  # stat_poly_line(method="lm") +
  # stat_poly_eq(use_label(c("p")), size=10)+
  # geom_errorbar( aes(x=group, ymin=min, ymax=max), color='black')+
  # facet_wrap(`MAT.TYPE`~ ., scales = "free_y")+
  ggtitle('Pre- and Post-2012 Average Seasonal Discharge')+
  ylab('Avg Total Discharge (l)')+
  xlab('')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 45))+
  theme(legend.position="bottom")
ggsave(Qplot2, filename="Crescent_2012Q.jpeg", device="jpeg", path=paste0(outdrive, 'Plots/'), width = 25, height = 25)

## t-test on Avg Seasonal Discharge
Qttest<-t.test(Qtot_seas ~ group, data =AllQseason )
sjPlot:: tab_model(ttest)
Qtable<-nice_table(Qttest)
flextable::save_as_docx(Qttest, path = paste0(outdrive, "Q_2012_t-tests.docx"))

##put into a table
table2<-nice_table(ECttest)
# Save in Word
flextable::save_as_docx(table2, path = paste0(outdrive, "Flux_t-tests.docx"))

###Join that together with the ion data
all2<-all1%>%
  dplyr::select(., 'Stream', 'DateTime', 'Date', 'year', 'season', 'group', "na_mgl","k_mgl","mg_mgl", "cl_mgl","si_mgl", "ca_mgl",
                "f_mgl", "li_mgl", "so4_mgl", "srp_mgl","n+n_mgl", "n_nh4_mgl")%>%
  dplyr::filter(., Stream=='Crescent East'|!Stream=="Crescent West")
all2$DATE_TIME<-lubridate::ymd_hms(all2$DateTime)
Alldata<-full_join(all2, AllQday, by="Date")

All_data<-Alldata%>%gather(., variable, `concentration (mg/l)`, "na_mgl","k_mgl","mg_mgl","cl_mgl","si_mgl", "ca_mgl", "f_mgl", "li_mgl", "so4_mgl",
                    "srp_mgl","n+n_mgl","n_nh4_mgl")

library(scales)
##plot all concentrations over time vs. discharge on log-log scale
Chemo<-ggplot(All_data, aes(y=`concentration (mg/l)`, x=`q_md`, color=group, group=group)) + 
                geom_point(size=3)+
                facet_wrap(`variable`~ .)+
                # geom_smooth(method="lm")+
                # geom_line(aes())+
                # geom_smooth()+
                stat_poly_line(method="lm") +
                geom_abline(slope=-1, linetype=3, linewidth=2)+
                scale_fill_manual(values = friendly_pal("nickel_five"))+
                theme(panel.background = element_rect(fill = 'white', colour = 'white'))+
                # stat_poly_eq(use_label(c("eq")), size=10)+
                # scale_x_continuous(trans='pseudo_log', breaks = c(1, 10, 100, 1000))+
                # scale_y_continuous(trans='pseudo_log', breaks = c(0.1, 1, 10, 100))+
                scale_y_continuous(trans='log10', breaks = c(0.1, 1, 10, 100))+
                scale_x_continuous(trans='log10', labels = comma)+
                # xlim(0, 1000)+
                # ylim(0, 100)+
                ggtitle('')+
                # theme(axis.text.x = element_text(angle = 90))+
                theme(text = element_text(size = 50))+
                theme(legend.position="bottom")
ggsave(Chemo, filename="Chemostasis_2012_noslope.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 25, height = 20)

### find breakpoints of chemostasis?
library(breakpoint)

### Plot the CVC/CVQ
### calculate the CV for each 
### cv <- sd(data) / mean(data) * 100

All_data2<-All_data%>%
  group_by(group, variable)%>%
  summarise(C_sd=sd(`concentration (mg/l)`, na.rm=TRUE), C_mean=mean(`concentration (mg/l)`, na.rm=TRUE), 
            Q_sd=sd(Qtot_m3, na.rm=TRUE), Q_mean=mean(Qtot_m3, na.rm=TRUE))
All_data2$CVC=((All_data2$C_sd / All_data2$C_mean) *100)
All_data2$CVQ=((All_data2$Q_sd / All_data2$Q_mean) *100)
All_data2$`CVC:CVQ`=(All_data2$CVC/All_data2$CVQ)

###plot the CVC/CVQ
CVplot<-ggplot(All_data2, aes(y=`CVC:CVQ`, x=variable, color=group, group=group)) + 
  geom_point(size=10)+
  # facet_wrap(`variable`~ .)+
  geom_hline(yintercept=0.5)+
  # xlim(0, 1000)+
  # ylim(0, 100)+
  ggtitle('')+
  theme(axis.text.x = element_text(angle = 90))+
  theme(text = element_text(size = 50))+
  theme(legend.position="bottom")
ggsave(CVplot, filename="CVCQplot.jpeg", device="jpeg", path=paste0(outdrive, 'Plots'), width = 25, height = 20)






