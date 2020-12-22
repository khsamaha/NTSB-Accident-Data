# global option set
options(digits = 2, scipen=5 )

# load the library
library(dplyr)
library(plyr)
library(tidyr)
library(lubridate)
library(forcats)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(extrafontdb)    # done once
library(extrafont)      # registering fonts with R
library(knitr)
library(DT)


# Load theNTSB data ../input/AviationDataUP.csv
AviationDataUP <- read.csv("AviationData.csv", 
                           na.strings="", 
                           stringsAsFactors = FALSE
)

# Examine the properties of the data.
dimension        <- dim(AviationDataUP)
fatality         <- sum(na.omit(AviationDataUP$Total.Fatal.Injuries))        # 45543 
injuries         <- sum(na.omit((AviationDataUP$Total.Serious.Injurie) + 
                                  (AviationDataUP$Total.Serious.Injuries)))    # 34102
accident.count   <- length(unique(AviationDataUP$Accident.Number))           # 79141
num.of.countries <- length(unique(AviationDataUP$Country))                   # 178

us.count         <- table(AviationDataUP$Country) %>% transform() %>%        # 95%
  arrange(desc(Freq)) %>% 
  mutate( prcnt = Freq/ sum(Freq)) %>% 
  filter(Var1 == "United States")

count    <- c( fatality, injuries, accident.count, num.of.countries, round((us.count$prcnt * 100), 2))
features <- c( "Fatality", "Total Injuries", "Total# of Accidents", "Number of Countries", "USA.Count in %" )

t2 <- as.data.frame(cbind(features,count))

# only display the table,
datatable(head(t2), options = list(dom = 't'))

summary(as.Date(AviationDataUP$Event.Date))
summary(na.omit(AviationDataUP$Number.of.Engines))
summary(na.omit(AviationDataUP$Total.Fatal.Injuries))
mean(na.omit(AviationDataUP$Total.Fatal.Injuries))

library(knitr)

#+ Check for duplication
# AviationDataUP[!duplicated(AviationDataUP),]

# +Check for missing data
# count number of msng value in each column
msng <- function(x) { sum(is.na(x))
}
misng <- as.data.frame(colwise (msng) (AviationDataUP))


#+ General Tidy lower case for all
#lower <- function (x) {tolower(names(x))}
#names(AviationDataUP) <- lower(AviationDataUP)

##++ verify
#names(AviationDataUP) 

# convert column chracters to lower letter
#for(i in 1:31){
  #AviationDataUP[,i] = tolower(AviationDataUP[,i])
#}


names(AviationDataUP)[1:31] <- tolower(names(AviationDataUP)[1:31])
# split date month day
AviationDataUP <- separate(AviationDataUP, event.date, into=c('year', 'month', 'day'))

# split city and state from location
AviationDataUP <- AviationDataUP %>% separate(location, sep = ",",c("city", "state"))

### convert class to facotr and numeric
# convert to factor calss - treat month on its own
for(i in c(1:3,7:9,12:20,22:26,31:33)){
  AviationDataUP[,i] = as.factor( AviationDataUP[,i])
}

# convert to numeric class                                    
for(i in c(4,5,6,10,11,21,27:30)){
  AviationDataUP[,i] = as.numeric( AviationDataUP[,i])
}

############################
## Date change 
# convert numbers for month to abbriviation month name
AviationDataUP$month <- month.abb[as.numeric(AviationDataUP$month)]

# convert month to factor
AviationDataUP$month <- factor(AviationDataUP$month)

# change factor level based on frequency count - use forcats pkg
AviationDataUP$month <- AviationDataUP$month %>% fct_infreq()  

#kable(head(AviationDataUP[,4:15]))

#library(knitr)
#library(DT)

AviationDataUP <- AviationDataUP %>% 
  filter(country              == "United States" & 
           purpose.of.flight    != "Unknown"       &
           investigation.type   != "Incident"      &
           total.fatal.injuries  > 1
  ) %>% droplevels() %>%
  select(year, month, day, broad.phase.of.flight, city, state, latitude, longitude,  make,  model,
         number.of.engines, purpose.of.flight, total.fatal.injuries, weather.condition, 
         airport.code, airport.name, far.description, country ) 

#kable(head(AviationDataUP[,1:11]))
#DT::datatable(AviationDataUP)
datatable(head(AviationDataUP[,1:10], 8), options = list(dom = 't'))

##+++++++++++++++++++++++++

gg <- AviationDataUP %>% filter( year > 1981) %>% group_by(state) %>% dplyr::summarise(count = n()) %>% arrange(desc(count))


#trend
gg$state <- fct_inorder(gg$state)  #arrange level with the most frequent upfront
#gg$state <- fct_shift(gg$state)    #reverse order for the cord flip

# cal percent for labling
gg <- gg %>% mutate (prct  = (count / 5932) * 100,
                     prcnt = paste0(as.character(round(prct,2)), "%")
) 

# bar plot by state and accident count
ggg <- ggplot(gg[1:8,], aes(x = state, y = count, fill = state,  label= prcnt)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.6)) +
  scale_fill_brewer(palette="Set2") + 
  geom_text(position = position_dodge(0.6), vjust = 2.0, colour = "white", fontface = "bold")  +  # for horizontal spacing hjust = 2.0
  labs( x = "States", y = "Fatality Count", colour = "gray")

ggg <- ggg + theme(legend.position="none")
ggg <- ggg + labs(title = "US aircaft accident count 1980-2020", 
                  subtitle = "The top eight States with aircraft accident causing at least one fatality.",
                  caption = "Data Source: NTSB") 
ggg<- ggg + theme_light()
    
ggg1 <- ggg + theme(axis.text = element_text(angle = 45, hjust = 1, size=13, colour= "gray"))  +
  theme(axis.title.y = element_text(colour = "gray", size = rel(1.5), angle = 90)) + 
  theme(axis.title.x = element_text(colour = "gray", size = rel(1.5), angle = 0))

##+++++++++++++++++++++++++

# line grabph for over all years vs accident count
tt <- AviationDataUP %>% filter( year > 1981) %>% group_by(year) %>% dplyr::summarise(count = n()) %>% arrange(desc(count))

ggg <- ggplot(data = tt, aes(x=year, y = count)) + 
  geom_line(colour = "red", size = 2) 

ggg <- ggg + labs( x = "Years", y = "Fatality Count", colour = "gray") 
ggg <- ggg + theme_light()
ggg <- ggg + labs(title    = "US Fatal Airplane Accident 1980-2020", 
                  subtitle = "US airplane accident count trend that involed at least one fatality.",
                  caption  = "Data Source: NTSB")

ggg2 <- ggg + theme(axis.text = element_text(angle = 45, hjust = 1, size=13, colour= "gray"))  +
  theme(axis.title.y = element_text(colour = "gray", size = rel(1.5), angle = 90)) + 
  theme(axis.title.x = element_text(colour = "gray", size = rel(1.5), angle = 0)) 

##+++++++++++++++++++++++++

tt5 <- AviationDataUP %>% filter(year > 1981) %>% # & broad.phase.of.flight != "cruise") %>%
  select(year, broad.phase.of.flight, total.fatal.injuries) %>% 
  group_by(year, broad.phase.of.flight) %>%
  summarise_each(funs(sum), total.fatal.injuries) %>% arrange(desc(total.fatal.injuries))

phase <- c("CRUISE", "TAKEOFF", "APPROACH", "CLIMb", "DESCENT", "LANDING")

tt5 <- tt5 %>% filter(broad.phase.of.flight %in% phase) %>% droplevels()
# nlevels(tt5$broad.phase.of.flight)
# tt5$broad.phase.of.flight %>% levels()
# change fcator order - most frequent first
tt5$broad.phase.of.flight <- fct_inorder(tt5$broad.phase.of.flight) 

ggg3 <- ggplot(tt5, aes(x= year, y = total.fatal.injuries, colour = broad.phase.of.flight)) + 
  geom_line(size=1.5) + 
  labs( x = "Years", y = "Fatality Count", colour = "gray") +
  facet_wrap(~ broad.phase.of.flight)
ggg3 <- ggg3 + coord_cartesian(ylim = c(0, 375))
ggg3 <- ggg3 + theme_light()
ggg3 <- ggg3 + theme(legend.position="none")
ggg3 <- ggg3 + labs(title = "US aircaft accident Phase of flight 1980-2020", 
                    subtitle = "Top six Phases of flight with the most fatal accidents.",
                    caption = "Data Source: NTSB")

ggg3 <- ggg3 + theme(axis.text = element_text( hjust = 1, size=12, colour= "gray"))  +
  theme(axis.title.y = element_text(colour = "gray", size = rel(1.5), angle = 90)) + 
  theme(axis.title.x = element_text(colour = "gray", size = rel(1.5), angle = 0))    

#+++++++++++++++++++++++++++++

tt7 <- AviationDataUP %>%  filter(year > 1981) %>%
  select(purpose.of.flight, total.fatal.injuries) %>% 
  group_by(purpose.of.flight) %>% 
  summarise_each(funs(sum), total.fatal.injuries) %>% 
  arrange(desc(total.fatal.injuries)) %>% droplevels()

tt7$purpose.of.flight <- fct_inorder(tt7$purpose.of.flight)    # change levels  

#levels(tt7$purpose.of.flight)

ggg4 <- ggplot(tt7[2:9,], aes(x = purpose.of.flight, y = total.fatal.injuries,  fill = purpose.of.flight, label= total.fatal.injuries)) +
  geom_bar(stat="identity", position = position_dodge(width = 0.6)) +
  scale_fill_brewer(palette="Set2") + 
  geom_text(position = position_dodge(0.6), vjust = 2.0, colour = "white", fontface = "bold")  +  # for horizontal spacing hjust = 2.0
  labs( x = "purpose.of.flight", y = "Fatality Count", colour = "gray")

ggg4 <- ggg4 + coord_cartesian(ylim = c(0, 1600))
ggg4 <- ggg4 + theme_light()
ggg4 <- ggg4 + theme(legend.position="none")

ggg4 <- ggg4 + labs(title = "US aircaft accident purpose of flight 1980-2020", 
                    subtitle = "Purpose of the flight when the fatal accidents occur.\nPersonal flights, where the majority fatal flights took place, is excluded on this plot. ",
                    caption = "Data Source: NTSB")


ggg4 <- ggg4 + theme(axis.text = element_text( angle = 45, hjust = 1, size=12, colour= "gray"))  +
  theme(axis.title.y = element_text(colour = "gray", size = rel(1.5), angle = 90)) + 
  theme(axis.title.x = element_text(colour = "gray", size = rel(1.5), angle = 0)) 

#++++++  

grid.arrange(ggg1, 
             ggg2, 
             ggg3,
             ggg4,
             ncol=2 
)
