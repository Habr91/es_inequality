rm(list=ls())

this.dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(this.dir)

### LOAD LIBRARIES
library(reshape2)
library(ggplot2)
library(dplyr)
library(stringr)
library(gapminder)
library(magrittr)
library(grid)
library(gridExtra)

### LOAD DATASETS
load("SWIIDv5_1.RData")
EF <- read.csv("EcologicalFootrpint.csv",header=TRUE, sep=",", stringsAsFactors=FALSE)
ES <- read.csv("ES_countries.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
GINI_new <- read.csv("allginis_Oct2014.csv",header=TRUE, sep=",", stringsAsFactors=FALSE)
palma <- read.csv("Palma.csv", header = TRUE, sep = ",", stringsAsFactors=FALSE)
EF_temp <- read.csv("EF_temporal.csv",header=TRUE, sep=",", stringsAsFactors=FALSE)
EF_temp_total <- subset(EF_temp,EF_temp$Record == 'EFExportsPerCap'|EF_temp$Record == 'EFImportsPerCap')


swiid_summary <- swiid %>%
  bind_rows() %>%
  group_by(country, year) %>%
  summarize_all(funs(mean, sd)) %>%
  ungroup() %>%
  rename_(.dots=setNames(names(.),
                         str_replace(names(.), "_mean", ""))) %>%
  rename_(.dots=setNames(names(.),
                         str_replace(names(.), "_sd", "_se")))

#creat a dataframe of most up to date GINI values for all countries
GINI_2003 <- subset(as.data.frame(swiid_summary), year == 2003)
GINI_2004 <- subset(as.data.frame(swiid_summary), year == 2004)
GINI_2005 <- subset(as.data.frame(swiid_summary), year == 2005)
GINI_2006 <- subset(as.data.frame(swiid_summary), year == 2006)
GINI_2007 <- subset(as.data.frame(swiid_summary), year == 2007)
GINI_2008 <- subset(as.data.frame(swiid_summary), year == 2008)
GINI_2009 <- subset(as.data.frame(swiid_summary), year == 2009)
GINI_2010 <- subset(as.data.frame(swiid_summary), year == 2010)
GINI_2011 <- subset(as.data.frame(swiid_summary), year == 2011)
GINI_upto2012 <- subset(as.data.frame(swiid_summary), year == 2012)
GINI.list <- list (GINI_2011,GINI_2010,GINI_2009,GINI_2008,GINI_2007,GINI_2006,GINI_2005,GINI_2004,GINI_2003)

#Decadal Ginis
GINI_0515 <- as.data.frame(unique(subset(as.data.frame(swiid_summary), year > 2005 & year <= 2015)))
GINI_9505 <- as.data.frame(unique(subset(as.data.frame(swiid_summary), year > 1995 & year <= 2005)))
GINI_8595 <- as.data.frame(unique(subset(as.data.frame(swiid_summary), year > 1985 & year <= 1995)))
GINI_7585 <- as.data.frame(unique(subset(as.data.frame(swiid_summary), year > 1975 & year <= 1985)))
GINI_6575 <- as.data.frame(unique(subset(as.data.frame(swiid_summary), year > 1965 & year <= 1975)))
GINI_5565 <- as.data.frame(unique(subset(as.data.frame(swiid_summary), year > 1955 & year <= 1965)))
GINI_8505 <- as.data.frame(unique(subset(as.data.frame(swiid_summary), year > 1985 & year <= 2005)))

GINI_0515$match <- (GINI_0515$country %in% GINI_9505$country)
GINI_9505$match <- (GINI_9505$country %in% GINI_8595$country)
GINI_8595$match <- (GINI_8595$country %in% GINI_7585$country)
GINI_7585$match <- (GINI_7585$country %in% GINI_6575$country)
GINI_6575$match <- (GINI_6575$country %in% GINI_5565$country)

GINI_0515$match2 <- (subset(GINI_0515, match == TRUE)[,c(1)] %in% subset(GINI_9505, match == TRUE)[,c(1)])
GINI_to1985match <- subset(GINI_0515, match == TRUE)
GINI_to1985match$match2 <- (subset(GINI_0515, match == TRUE)[,c(1)] %in% subset(GINI_9505, match == TRUE)[,c(1)])
countries1985 <- as.data.frame(unique(subset(GINI_to1985match,match2 == TRUE)[,c(1)]))
names(countries1985) <- c("country")
GINI0515<- as.data.frame(unique(GINI_0515[,c(1)]))
merge(countries1985,,by.x=c("country"),by.y=c("country"))
#GINI_0515$matchto1985 <- GINI_0515[,c(1)] %in% countries1985



#GINI_9505 <- subset(as.data.frame(swiid_summary), year > 1995 & year <= 2005)
#GINI_8595 <- subset(as.data.frame(swiid_summary), year > 1985 & year <= 1995)
#GINI_7585 <- subset(as.data.frame(swiid_summary), year > 1975 & year <= 1985)
#GINI_6575 <- subset(as.data.frame(swiid_summary), year > 1965 & year <= 1975)
#GINI_5565 <- subset(as.data.frame(swiid_summary), year > 1955 & year <= 1965)

for (i in GINI.list){
  missing.entries <- anti_join(i,GINI_upto2012,by = c("country"))
  GINI_upto2012 <- rbind(GINI_upto2012,missing.entries)
}

#merge with EF data
GINI_EF <- merge(GINI_upto2012,EF,by.x=c("country"),by.y=c("Country.region"))
GINI_EF$EFcountry <- GINI_EF$Total.Ecological.Footprint*(GINI_EF$Population..millions.*1000000)
GINI_EF$HDI_quart <- with(GINI_EF, cut(GINI_EF$HDI, 
                                       breaks=quantile(GINI_EF$HDI, probs=seq(0,1, by=0.25), na.rm=TRUE), 
                                       include.lowest=TRUE))


ggplot(GINI_EF,aes(x=gini_net, y=Total.Ecological.Footprint)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +
  labs(x = "Gini", y = "Ecological Footprint (2012)", title = "GINI vs. Ecological Footprint per  capita")

ggplot(GINI_EF,aes(x=gini_net, y=EFcountry)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +
  labs(x = "Gini", y = "Ecological Footprint", title = "GINI vs. Total Ecological Footprint")

ggplot(GINI_EF,aes(x=gini_net, y=Total.Ecological.Footprint)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() + facet_grid(Income.Group~.)
labs(x = "Gini", y = "Ecological Footprint (2012)", title = "GINI vs. Ecological Footprint per  capita")

ggplot(GINI_EF,aes(x=gini_net, y=Total.Ecological.Footprint)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() + facet_grid(HDI_quart~.)
labs(x = "Gini", y = "Ecological Footprint (2012)", title = "GINI vs. Ecological Footprint per  capita")


GINI_2008 <- subset(GINI, EF$Reference_Period =='year' && EF$Year >= 2008, select=c(Country.region,Gini))
#-------
#comparing ES with decadal GINI change
countries_1985_05 <- unique(subset(GINI_9505,GINI_9505$match == TRUE)$country)
ES_1985_05 <- cbind((ES$Country %in% countries_1985_05),ES)
ES_1985_05 <- subset(ES_1985_05,ES_1985_05[,c(1)]==TRUE)

#Get avg GINI data for 2 previous decades 
GINI_9505ES <- cbind((GINI_9505$country %in% countries_1985_05),GINI_9505)
GINI_9505ES <- subset(GINI_9505ES,GINI_9505ES[,c(1)]==TRUE)
GINI_dec1 <- GINI_9505ES %>%
  group_by(country) %>%
  summarize(avg_gini = mean(gini_net))


GINI_8595ES <- cbind((GINI_8595$country %in% countries_1985_05),GINI_8595)
GINI_8595ES <- subset(GINI_8595ES,GINI_8595ES[,c(1)]==TRUE)
GINI_dec2 <- GINI_8595ES %>%
  group_by(country) %>%
  summarize(avg_gini2 = mean(gini_net))

#bind back to ES data
decadal_ES_Gini <- merge(ES_1985_05, GINI_dec1, by.x=c("Country"),by.y=c("country"))
decadal_ES_Gini <- merge(decadal_ES_Gini, GINI_dec2, by.x=c("Country"),by.y=c("country"))

if(!file.exists("DecadalGini_ES_harmonized.csv")){
  write.csv(decadal_ES_Gini, file = "DecadalGini_ES_harmonized.csv")
} else{
  decadal_ES_Gini <- read.csv("DecadalGini_ES_harmonized.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
}
decadal_ES_Gini <- merge(decadal_ES_Gini, EF, by.x=c("Country"),by.y=c("Country.region"))
write.csv(decadal_ES_Gini, file = "DecadalGini_ES_EF_harmonized.csv")
if(!file.exists("DecadalGini_ES_EF_harmonized.csv")){
  write.csv(decadal_ES_Gini, file = "DecadalGini_ES_EF_harmonized.csv")
} else{
  decadal_ES_Gini <- read.csv("DecadalGini_ES_EF_harmonized.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
}
decadal_ES_Gini <- mutate(decadal_ES_Gini,gini_change = avg_gini-avg_gini2)

#plots for both decades
p1 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=AQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1995-2005)", y = "Air Quality (~2000)")

p2 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=CSEQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1995-2005)", y = "Carbon Seq (~2000)")

p3 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=LIVE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1995-2005)", y = "Livestock (~2000)")

p4 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=CROP)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1995-2005)", y = "Crop (~2000)")

p5 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=CSTORE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1995-2005)", y = "Carbon Storage (~2000)")

p6 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=WQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1995-2005)", y = "Water Quality (~2000)")

p7 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=WA)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1995-2005)", y = "Water Povisioning (~2000)")

p8 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=REC)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() + theme(text = element_text(size=6)) +
  labs(x = "average GINI (1995-2005)", y = "Nature Recreation (~2000)")

grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,ncol=2)

#-- second plot

p1 <- ggplot(decadal_ES_Gini,aes(x=avg_gini2, y=AQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-1995)", y = "Air Quality (~2000)")

p2 <- ggplot(decadal_ES_Gini,aes(x=avg_gini2, y=CSEQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-1995)", y = "Carbon Seq (~2000)")

p3 <- ggplot(decadal_ES_Gini,aes(x=avg_gini2, y=LIVE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-1995)", y = "Livestock (~2000)")

p4 <- ggplot(decadal_ES_Gini,aes(x=avg_gini2, y=CROP)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-1995)", y = "Crop (~2000)")

p5 <- ggplot(decadal_ES_Gini,aes(x=avg_gini2, y=CSTORE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-1995)", y = "Carbon Storage (~2000)")

p6 <- ggplot(decadal_ES_Gini,aes(x=avg_gini2, y=WQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-1995)", y = "Water Quality (~2000)")

p7 <- ggplot(decadal_ES_Gini,aes(x=avg_gini2, y=WA)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-1995)", y = "Water Povisioning (~2000)")

p8 <- ggplot(decadal_ES_Gini,aes(x=avg_gini2, y=REC)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() + theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-1995)", y = "Nature Recreation (~2000)")

grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,ncol=2)

#-- plot 3

p1 <- ggplot(decadal_ES_Gini,aes(x=gini_change, y=AQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "decadal GINI change (1985-1995;1995-2005)", y = "Air Quality (~2000)")

p2 <- ggplot(decadal_ES_Gini,aes(x=gini_change, y=CSEQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "decadal GINI change (1985-1995;1995-2005)", y = "Carbon Seq (~2000)")

p3 <- ggplot(decadal_ES_Gini,aes(x=gini_change, y=LIVE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "decadal GINI change (1985-1995;1995-2005)", y = "Livestock (~2000)")

p4 <- ggplot(decadal_ES_Gini,aes(x=gini_change, y=CROP)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "decadal GINI change (1985-1995;1995-2005)", y = "Crop (~2000)")

p5 <- ggplot(decadal_ES_Gini,aes(x=gini_change, y=CSTORE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "decadal GINI change (1985-1995;1995-2005)", y = "Carbon Storage (~2000)")

p6 <- ggplot(decadal_ES_Gini,aes(x=gini_change, y=WQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "decadal GINI change (1985-1995;1995-2005)", y = "Water Quality (~2000)")

p7 <- ggplot(decadal_ES_Gini,aes(x=gini_change, y=WA)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "decadal GINI change (1985-1995;1995-2005)", y = "Water Povisioning (~2000)")

p8 <- ggplot(decadal_ES_Gini,aes(x=gini_change, y=REC)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() + theme(text = element_text(size=6)) +
  labs(x = "decadal GINI change (1985-1995;1995-2005)", y = "Nature Recreation (~2000)")

grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,ncol=2)

# fourth plot

p1 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=AQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) + facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1995-2005)", y = "Air Quality (~2000)")

p2 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=CSEQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1995-2005)", y = "Carbon Seq (~2000)")

p3 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=LIVE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1995-2005)", y = "Livestock (~2000)")

p4 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=CROP)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1995-2005)", y = "Crop (~2000)")

p5 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=CSTORE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1995-2005)", y = "Carbon Storage (~2000)")

p6 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=WQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1995-2005)", y = "Water Quality (~2000)")

p7 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=WA)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1995-2005)", y = "Water Povisioning (~2000)")

p8 <- ggplot(decadal_ES_Gini,aes(x=avg_gini, y=REC)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() + theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1995-2005)", y = "Nature Recreation (~2000)")

grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,ncol=2)

#plots for double decadal avg (1985-05)
GINI_8505ES<-cbind((GINI_8505$country %in% countries_1985_05),GINI_8505)
GINI_8505ES <- subset(GINI_8505ES,GINI_8505ES[,c(1)]==TRUE)

GINI_dec8505 <- GINI_8505ES %>%
  group_by(country) %>%
  summarize(avg_gini8505 = mean(gini_net))

#bind back to ES data
decadal_ES_Gini <- merge(decadal_ES_Gini, GINI_dec8505, by.x=c("Country"),by.y=c("country"))
write.csv(decadal_ES_Gini, file = "DecadalGini_ES_harmonized.csv")

#plot figure 5
p1 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=AQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-2005)", y = "Air Quality (~2000)")

p2 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=CSEQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-2005)", y = "Carbon Seq (~2000)")

p3 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=LIVE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-2005)", y = "Livestock (~2000)")

p4 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=CROP)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-2005)", y = "Crop (~2000)")

p5 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=CSTORE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-2005)", y = "Carbon Storage (~2000)")

p6 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=WQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-2005)", y = "Water Quality (~2000)")

p7 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=WA)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-2005)", y = "Water Povisioning (~2000)")

p8 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=REC)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() + theme(text = element_text(size=6)) +
  labs(x = "average GINI (1985-2005)", y = "Nature Recreation (~2000)")

grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,ncol=2)

#plot figure 6
p1 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=AQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1985-2005)", y = "Air Quality (~2000)")

p2 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=CSEQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1985-2005)", y = "Carbon Seq (~2000)")

p3 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=LIVE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1985-2005)", y = "Livestock (~2000)")

p4 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=CROP)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1985-2005)", y = "Crop (~2000)")

p5 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=CSTORE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1985-2005)", y = "Carbon Storage (~2000)")

p6 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=WQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1985-2005)", y = "Water Quality (~2000)")

p7 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=WA)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=12)) +facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1985-2005)", y = "Water Povisioning (~2000)")

p8 <- ggplot(decadal_ES_Gini,aes(x=avg_gini8505, y=REC)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() + theme(text = element_text(size=12)) + facet_wrap(~ Income.Group) +
  labs(x = "average GINI (1985-2005)", y = "Nature Recreation (~2000)")

grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,ncol=2)

#TRY ANOTHER Inequality measure
palma_ES <- cbind((palma$Country %in% countries_1985_05),palma)
palma_ES <- subset(palma_ES,palma_ES[,c(1)]==TRUE)
palma_ES1990 <- subset(palma_ES,palma_ES$Approximate.year == 1990)
names(palma_ES1990) <- paste(names(palma_ES1990),"90", sep = "_")
palma_ES2012 <- subset(palma_ES,palma_ES$Approximate.year == 2012)
names(palma_ES2012) <- paste(names(palma_ES2012),"2012", sep = "_")

#bind back to ES data
decadal_ES_Gini <- merge(decadal_ES_Gini, palma_ES1990, by.x=c("Country"),by.y=c("Country_90"))
decadal_ES_Gini <- merge(decadal_ES_Gini, palma_ES2012, by.x=c("Country"),by.y=c("Country_2012"))
decadal_ES_Gini <- mutate(decadal_ES_Gini,palma_change = Palma.ratio_2012-Palma.ratio_90)
write.csv(decadal_ES_Gini, file = "DecadalGini_ES_EF_Palma_harmonized.csv")

#plot Palma figures
p1 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=AQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~1990", y = "Air Quality (~2000)")

p2 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=CSEQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~1990", y = "Carbon Seq (~2000)")

p3 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=LIVE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~1990", y = "Livestock (~2000)")

p4 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=CROP)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~1990", y = "Crop (~2000)")

p5 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=CSTORE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~1990", y = "Carbon Storage (~2000)")

p6 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=WQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~1990", y = "Water Quality (~2000)")

p7 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=WA)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~1990", y = "Water Povisioning (~2000)")

p8 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=REC)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() + theme(text = element_text(size=6)) +
  labs(x = "Palma ~1990", y = "Nature Recreation (~2000)")

grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,ncol=2)

#-- second plot

p1 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_2012, y=AQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~2012", y = "Air Quality (~2000)")

p2 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_2012, y=CSEQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~2012", y = "Carbon Seq (~2000)")

p3 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_2012, y=LIVE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~2012", y = "Livestock (~2000)")

p4 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_2012, y=CROP)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~2012", y = "Crop (~2000)")

p5 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_2012, y=CSTORE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~2012", y = "Carbon Storage (~2000)")

p6 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_2012, y=WQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~2012", y = "Water Quality (~2000)")

p7 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_2012, y=WA)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma ~2012", y = "Water Povisioning (~2000)")

p8 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_2012, y=REC)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() + theme(text = element_text(size=6)) +
  labs(x = "Palma ~2012", y = "Nature Recreation (~2000)")

grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,ncol=2)

#-- plot 3

p1 <- ggplot(decadal_ES_Gini,aes(x=palma_change, y=AQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma Change (1990-2012)", y = "Air Quality (~2000)")

p2 <- ggplot(decadal_ES_Gini,aes(x=palma_change, y=CSEQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma Change (1990-2012)", y = "Carbon Seq (~2000)")

p3 <- ggplot(decadal_ES_Gini,aes(x=palma_change, y=LIVE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma Change (1990-2012)", y = "Livestock (~2000)")

p4 <- ggplot(decadal_ES_Gini,aes(x=palma_change, y=CROP)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma Change (1990-2012)", y = "Crop (~2000)")

p5 <- ggplot(decadal_ES_Gini,aes(x=palma_change, y=CSTORE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma Change (1990-2012)", y = "Carbon Storage (~2000)")

p6 <- ggplot(decadal_ES_Gini,aes(x=palma_change, y=WQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma Change (1990-2012)", y = "Water Quality (~2000)")

p7 <- ggplot(decadal_ES_Gini,aes(x=palma_change, y=WA)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +
  labs(x = "Palma Change (1990-2012)", y = "Water Povisioning (~2000)")

p8 <- ggplot(decadal_ES_Gini,aes(x=palma_change, y=REC)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() + theme(text = element_text(size=6)) +
  labs(x = "Palma Change (1990-2012)", y = "Nature Recreation (~2000)")

grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,ncol=2)

#plot 4 _ facet by income

p1 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=AQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +facet_wrap(~ Income.Group) +
  labs(x = "Palma ~1990", y = "Air Quality (~2000)")

p2 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=CSEQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +facet_wrap(~ Income.Group) +
  labs(x = "Palma ~1990", y = "Carbon Seq (~2000)")

p3 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=LIVE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +facet_wrap(~ Income.Group) +
  labs(x = "Palma ~1990", y = "Livestock (~2000)")

p4 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=CROP)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +facet_wrap(~ Income.Group) +
  labs(x = "Palma ~1990", y = "Crop (~2000)")

p5 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=CSTORE)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +facet_wrap(~ Income.Group) +
  labs(x = "Palma ~1990", y = "Carbon Storage (~2000)")

p6 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=WQ)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +facet_wrap(~ Income.Group) +
  labs(x = "Palma ~1990", y = "Water Quality (~2000)")

p7 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=WA)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() +theme(text = element_text(size=6)) +facet_wrap(~ Income.Group) +
  labs(x = "Palma ~1990", y = "Water Povisioning (~2000)")

p8 <- ggplot(decadal_ES_Gini,aes(x=Palma.ratio_90, y=REC)) +
  geom_point() + geom_smooth(method = "lm", se = T) +
  theme_bw() + theme(text = element_text(size=6)) +facet_wrap(~ Income.Group) +
  labs(x = "Palma ~1990", y = "Nature Recreation (~2000)")


grid.arrange(p1, p2, p3, p4,p5,p6,p7,p8,ncol=2)

### EF Temporal DATA
EF_9505 <- as.data.frame(unique(subset(as.data.frame(EF_temp_total), Year > 1995 & Year <= 2005)))
EF_8595 <- as.data.frame(unique(subset(as.data.frame(EF_temp_total), Year > 1985 & Year <= 1995)))
EF_0515 <- as.data.frame(unique(subset(as.data.frame(EF_temp_total), Year > 2005 & Year <= 2015)))

EF_9505_avg <-na.omit(EF_9505) %>%
  group_by(Country) %>%
  summarize(total_EF9505 = mean(as.numeric(Total)),crop_EF9505 = mean(as.numeric(Crop_Land)),
            grazing_EF9505 = mean(as.numeric(Grazing_Land)),forest_EF9505 = mean(as.numeric(Forest_Land)),
            fishing_EF9505 = mean(as.numeric(Fishing_Ground)),carbon_EF9505 = mean(as.numeric(Carbon)))
EF_9505_impexp <-na.omit(EF_9505) %>%
  group_by(Country,Record) %>%
  summarize(total_EF9505 = mean(as.numeric(Total)),crop_EF9505 = mean(as.numeric(Crop_Land)),
            grazing_EF9505 = mean(as.numeric(Grazing_Land)),forest_EF9505 = mean(as.numeric(Forest_Land)),
            fishing_EF9505 = mean(as.numeric(Fishing_Ground)),carbon_EF9505 = mean(as.numeric(Carbon)))%>%
  data.frame()
testmelt<- melt(EF_9505_impexp, id = c("Country","Record"), na.rm = T)
EF_9505_avg_cast <- dcast(testmelt, testmelt$Country ~ testmelt$Record + testmelt$variable )

EF_8595_avg <-EF_8595 %>%
  group_by(Country) %>%
  summarize(total_EF8595 = mean(as.numeric(Total)),crop_EF8595 = mean(as.numeric(Crop_Land)),
            grazing_EF8595 = mean(as.numeric(Grazing_Land)),forest_EF8595 = mean(as.numeric(Forest_Land)),
            fishing_EF8595 = mean(as.numeric(Fishing_Ground)),carbon_EF8595 = mean(as.numeric(Carbon)))
EF_8595_impexp <-na.omit(EF_8595) %>%
  group_by(Country,Record) %>%
  summarize(total_EF8595 = mean(as.numeric(Total)),crop_EF8595 = mean(as.numeric(Crop_Land)),
            grazing_EF8595 = mean(as.numeric(Grazing_Land)),forest_EF8595 = mean(as.numeric(Forest_Land)),
            fishing_EF8595 = mean(as.numeric(Fishing_Ground)),carbon_EF8595 = mean(as.numeric(Carbon)))%>%
  data.frame()
testmelt<- melt(EF_8595_impexp, id = c("Country","Record"), na.rm = T)
EF_8595_avg_cast <- dcast(testmelt, testmelt$Country ~ testmelt$Record + testmelt$variable )

EF_0515_avg <-na.omit(EF_0515) %>%
  group_by(Country) %>%
  summarize(total_EF0515 = mean(as.numeric(Total)),crop_EF0515 = mean(as.numeric(Crop_Land)),
            grazing_EF0515 = mean(as.numeric(Grazing_Land)),forest_EF0515 = mean(as.numeric(Forest_Land)),
            fishing_EF0515 = mean(as.numeric(Fishing_Ground)),carbon_EF0515 = mean(as.numeric(Carbon)))
EF_0515_impexp <-na.omit(EF_0515) %>%
  group_by(Country,Record) %>%
  summarize(total_EF0515 = mean(as.numeric(Total)),crop_EF0515 = mean(as.numeric(Crop_Land)),
            grazing_EF0515 = mean(as.numeric(Grazing_Land)),forest_EF0515 = mean(as.numeric(Forest_Land)),
            fishing_EF0515 = mean(as.numeric(Fishing_Ground)),carbon_EF0515 = mean(as.numeric(Carbon)))%>%
  data.frame()
testmelt<- melt(EF_0515_impexp, id = c("Country","Record"), na.rm = T)
EF_0515_avg_cast <- dcast(testmelt, testmelt$Country ~ testmelt$Record + testmelt$variable )

EF.merged <- merge(EF_9505_avg,EF_8595_avg,by=c("Country"))
EF.merged <- merge(EF.merged,EF_0515_avg,by=c("Country"))
EF.merged <- merge(EF.merged,EF_9505_avg_cast,by.x=c("Country"),by.y = c("testmelt$Country"))
EF.merged <- merge(EF.merged,EF_8595_avg_cast,by.x=c("Country"),by.y = c("testmelt$Country"))
EF.merged <- merge(EF.merged,EF_0515_avg_cast,by.x=c("Country"),by.y = c("testmelt$Country"))

decadal_ES_Gini <- merge(decadal_ES_Gini, EF.merged, by.x=c("Country"),by.y=c("Country"))
decadal_ES_Gini <- mutate(decadal_ES_Gini,EF_change05.95 = total_EF0515-total_EF9505)
decadal_ES_Gini <- mutate(decadal_ES_Gini,EF_change95.85 = total_EF9505-total_EF8595)
decadal_ES_Gini <- mutate(decadal_ES_Gini,EF_cropchange05.95 = crop_EF0515-crop_EF9505)
decadal_ES_Gini <- mutate(decadal_ES_Gini,EF_cropchange95.85 = crop_EF9505-crop_EF8595)
decadal_ES_Gini <- mutate(decadal_ES_Gini,EF_grazingchange05.95 = grazing_EF0515-grazing_EF9505)
decadal_ES_Gini <- mutate(decadal_ES_Gini,EF_grazingchange95.85 = grazing_EF9505-grazing_EF8595)
decadal_ES_Gini <- mutate(decadal_ES_Gini,EF_forestchange05.95 = forest_EF0515-forest_EF9505)
decadal_ES_Gini <- mutate(decadal_ES_Gini,EF_forestchange95.85 = forest_EF9505-forest_EF8595)
decadal_ES_Gini <- mutate(decadal_ES_Gini,EF_fishingchange05.95 = fishing_EF0515-fishing_EF9505)
decadal_ES_Gini <- mutate(decadal_ES_Gini,EF_fishingchange95.85 = fishing_EF9505-fishing_EF8595)
decadal_ES_Gini <- mutate(decadal_ES_Gini,EF_carbonchange05.95 = carbon_EF0515-carbon_EF9505)
decadal_ES_Gini <- mutate(decadal_ES_Gini,EF_carbonchange95.85 = carbon_EF9505-carbon_EF8595)

decadal_ES_Gini<- mutate(decadal_ES_Gini,ImpExp_EFratio0515 = EFImportsPerCap_total_EF0515/EFExportsPerCap_total_EF0515)
decadal_ES_Gini<- mutate(decadal_ES_Gini,ImpExp_EFratio8595 = EFImportsPerCap_total_EF8595/EFExportsPerCap_total_EF8595)
decadal_ES_Gini<- mutate(decadal_ES_Gini,ImpExp_EFratio9505 = EFImportsPerCap_total_EF9505/EFExportsPerCap_total_EF9505)

write.csv(decadal_ES_Gini,"harmonized_temporal_EF.csv")

#extract EF data for plotting
EF_temporal_plot <- decadal_ES_Gini[,c(1,13,14,28,29,34,43,48:117)]

#decadal change palma vs EF


p1<- ggplot(decadal_ES_Gini,aes(x=EF_change, y=palma_change, color=Record.y)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')+labs(x="EF Change (1985-2015)",y="Palma Change ~(1990-2012)")
p2<-ggplot(decadal_ES_Gini,aes(x=avg_EF8595.y, y=palma_change, color=Record.y)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p3<-ggplot(decadal_ES_Gini,aes(x=avg_EF9505.y, y=palma_change, color=Record.y)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p4<-ggplot(decadal_ES_Gini,aes(x=avg_EF0515, y=palma_change, color=Record.y)) + geom_smooth() + geom_point()+ theme_bw()


p5<-ggplot(decadal_ES_Gini,aes(x=avg_EF8595.y, y=Palma.ratio_90, color=Record.y)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p6<-ggplot(decadal_ES_Gini,aes(x=avg_EF8595.y, y=Palma.ratio_2012, color=Record.y)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p7<-ggplot(decadal_ES_Gini,aes(x=avg_EF9505.y, y=Palma.ratio_90, color=Record.y)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p8<-ggplot(decadal_ES_Gini,aes(x=avg_EF9505.y, y=Palma.ratio_2012, color=Record.y)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p9<-ggplot(decadal_ES_Gini,aes(x=avg_EF0515, y=Palma.ratio_90, color=Record.y)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p10<-ggplot(decadal_ES_Gini,aes(x=avg_EF0515, y=Palma.ratio_2012, color=Record.y)) + geom_smooth() + geom_point()+ theme_bw()



grid.arrange(p1, p2, p3, p4,ncol=2)
grid.arrange(p5, p6, p7,p8,p9,p10,ncol=2)

p5<-ggplot(decadal_ES_Gini,aes(x=ImpExp_EFratio9505, y=Palma.ratio_90)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none') + xlim(0,5)
p6<-ggplot(decadal_ES_Gini,aes(x=ImpExp_EFratio9505, y=Palma.ratio_2012)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')+ xlim(0,5)
p7<-ggplot(decadal_ES_Gini,aes(x=ImpExp_EFratio8595, y=Palma.ratio_90)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')+ xlim(0,5)
p8<-ggplot(decadal_ES_Gini,aes(x=ImpExp_EFratio8595, y=Palma.ratio_2012)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')+ xlim(0,5)
p9<-ggplot(decadal_ES_Gini,aes(x=ImpExp_EFratio0515, y=Palma.ratio_90)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')+ xlim(0,5)
p10<-ggplot(decadal_ES_Gini,aes(x=ImpExp_EFratio0515, y=Palma.ratio_2012)) + geom_smooth() + geom_point()+ theme_bw()+ xlim(0,5)
grid.arrange(p5, p6, p7,p8,p9,p10,ncol=2)

EF_types95 <- EF_temporal_plot[,c(6:14)]
EF_types85 <- EF_temporal_plot[,c(6:8,15:20)]
EF_types05 <- EF_temporal_plot[,c(6:8,21:26)]

p5<-ggplot(EF_types95,aes(x=Palma.ratio_90, y=total_EF9505)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p6<-ggplot(EF_types95,aes(x=Palma.ratio_90, y=crop_EF9505)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p7<-ggplot(EF_types95,aes(x=Palma.ratio_90, y=grazing_EF9505)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p8<-ggplot(EF_types95,aes(x=Palma.ratio_90, y=forest_EF9505)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p9<-ggplot(EF_types95,aes(x=Palma.ratio_90, y=fishing_EF9505)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p10<-ggplot(EF_types95,aes(x=Palma.ratio_90, y=carbon_EF9505)) + geom_smooth() + geom_point()+ theme_bw()
grid.arrange(p5, p6, p7,p8,p9,p10,ncol=2)

p5<-ggplot(EF_types85,aes(x=Palma.ratio_90, y=total_EF8595)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p6<-ggplot(EF_types85,aes(x=Palma.ratio_90, y=crop_EF8595)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p7<-ggplot(EF_types85,aes(x=Palma.ratio_90, y=grazing_EF8595)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p8<-ggplot(EF_types85,aes(x=Palma.ratio_90, y=forest_EF8595)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p9<-ggplot(EF_types85,aes(x=Palma.ratio_90, y=fishing_EF8595)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p10<-ggplot(EF_types85,aes(x=Palma.ratio_90, y=carbon_EF8595)) + geom_smooth() + geom_point()+ theme_bw()
grid.arrange(p5, p6, p7,p8,p9,p10,ncol=2)

p5<-ggplot(EF_types05,aes(x=Palma.ratio_2012, y=total_EF0515)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p6<-ggplot(EF_types05,aes(x=Palma.ratio_2012, y=crop_EF0515)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p7<-ggplot(EF_types05,aes(x=Palma.ratio_2012, y=grazing_EF0515)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p8<-ggplot(EF_types05,aes(x=Palma.ratio_2012, y=forest_EF0515)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p9<-ggplot(EF_types05,aes(x=Palma.ratio_2012, y=fishing_EF0515)) + geom_smooth() + geom_point()+ theme_bw()+ theme(legend.position='none')
p10<-ggplot(EF_types05,aes(x=Palma.ratio_2012, y=carbon_EF0515)) + geom_smooth() + geom_point()+ theme_bw()
grid.arrange(p5, p6, p7,p8,p9,p10,ncol=2)