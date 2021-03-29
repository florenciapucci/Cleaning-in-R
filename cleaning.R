
# Libraries
library(stringr)
library(readr)



# Path
path<-""
setwd(path)
output<-""


# Applied ads
group1<-read.csv("applicationlistgroup1.csv")
group2<-read.csv("applicationlistgroup2.csv")
group3<-read.csv("applicationlistgroup3.csv")
group4<-read.csv("applicationlistgroup4.csv")
group5<-read.csv("applicationlistgroup5.csv")
group6<-read.csv("applicationlistgroup6.csv")
group7<-read.csv("applicationlistgroup7.csv")
group8<-read.csv("applicationlistgroup8.csv")

group1$group<-1
group2$group<-2
group3$group<-3
group4$group<-4
group5$group<-5
group6$group<-6
group7$group<-7
group8$group<-8

# Information about wages
wages1<-read.csv("wagesandlinksgroup1.csv")
wages2<-read.csv("wagesandlinksgroup2.csv")
wages3<-read.csv("wagesandlinksgroup3.csv")
wages4<-read.csv("wagesandlinksgroup4.csv")
wages5<-read.csv("wagesandlinksgroup5.csv")
wages6<-read.csv("wagesandlinksgroup6.csv")
wages7<-read.csv("wagesandlinksgroup7.csv")
wages8<-read.csv("wagesandlinksgroup8.csv")

group1<-merge(group1, wages1, by.x = "Link", by.y = "Link")
group2<-merge(group2, wages2, by.x = "Link", by.y = "Link")
group3<-merge(group3, wages3, by.x = "Link", by.y = "Link")
group4<-merge(group4, wages4, by.x = "Link", by.y = "Link")
group5<-merge(group5, wages5, by.x = "Link", by.y = "Link")
group6<-merge(group6, wages6, by.x = "Link", by.y = "Link")
group7<-merge(group7, wages7, by.x = "Link", by.y = "Link")
group8<-merge(group8, wages8, by.x = "Link", by.y = "Link")


# Gender 1=man, 0=woman
group1$gender<-1
group2$gender<-1
group3$gender<-1
group4$gender<-1
group5$gender<-0
group6$gender<-0
group7$gender<-0
group8$gender<-0

# Bacherlo, 1= engineer, 0=economist
group1$gender<-0
group2$gender<-1
group3$gender<-0
group4$gender<-1
group5$gender<-0
group6$gender<-1
group7$gender<-0
group8$gender<-1

# Religion, 1= judaism, 0=none
group1$religion<-1
group2$religion<-1
group3$religion<-0
group4$religion<-0
group5$religion<-1
group6$religion<-1
group7$religion<-0
group8$religion<-0



# Job information
jobads <- read_csv("ZonaJobsB (3).csv")
jobads$nuevo2<-str_sub(jobads$link,-2,-1)
jobads$nuevo3<-as.numeric(jobads$nuevo2)
jobads$nuevo<-ifelse(is.na(jobads$nuevo3), substr(jobads$link,1,nchar(as.character(jobads$link))-19),
                     substr(jobads$link,1,nchar(as.character(jobads$link))-20))


importantcols<-c("nuevo","Link","group","wage","gender","religion",
                 "Trabajo","Empresa","Ubicación","Fecha","Salario",
                 "Jornada laboral","Área","Descripción")

# Now we are going to add information for groups dataset
# First we need to write links again to match with dataset with all the information
# Then merge. Do this for all groups
hola<-group1
hola$nuevo2<-str_sub(hola$Link,-2,-1)
hola$nuevo3<-as.numeric(hola$nuevo2)
hola$nuevo<-ifelse(is.na(hola$nuevo3), substr(hola$Link,1,nchar(as.character(hola$Link))-19),
                   substr(hola$Link,1,nchar(as.character(hola$Link))-20))
hola2<-merge(hola,jobads,by.x="nuevo",by.y="nuevo",all.x=TRUE,all.y=FALSE)
hola2 <- hola2[!duplicated(hola2$nuevo),]

# Testing
sum(sort(hola$Link)==sort(group1$Link))==nrow(group1)
group1<-hola2[,importantcols]

# Group 2

hola<-group2
hola$nuevo2<-str_sub(hola$Link,-2,-1)
hola$nuevo3<-as.numeric(hola$nuevo2)
hola$nuevo<-ifelse(is.na(hola$nuevo3), substr(hola$Link,1,nchar(as.character(hola$Link))-19),
                   substr(hola$Link,1,nchar(as.character(hola$Link))-20))
hola2<-merge(hola,jobads,by.x="nuevo",by.y="nuevo",all.x=TRUE,all.y=FALSE)
hola2 <- hola2[!duplicated(hola2$nuevo),]
# Test
sum(sort(hola$Link)==sort(group2$Link)) ==nrow(group2)
group2<-hola2[,importantcols]

# Group 3

hola<-group3
hola$nuevo2<-str_sub(hola$Link,-2,-1)
hola$nuevo3<-as.numeric(hola$nuevo2)
hola$nuevo<-ifelse(is.na(hola$nuevo3), substr(hola$Link,1,nchar(as.character(hola$Link))-19),
                   substr(hola$Link,1,nchar(as.character(hola$Link))-20))
hola2<-merge(hola,jobads,by.x="nuevo",by.y="nuevo",all.x=TRUE,all.y=FALSE)
hola2 <- hola2[!duplicated(hola2$nuevo),]
# Test
sum(sort(hola$Link)==sort(group3$Link)) ==nrow(group3)
group3<-hola2[,importantcols]

# Group 4

hola<-group4
hola$nuevo2<-str_sub(hola$Link,-2,-1)
hola$nuevo3<-as.numeric(hola$nuevo2)
hola$nuevo<-ifelse(is.na(hola$nuevo3), substr(hola$Link,1,nchar(as.character(hola$Link))-19),
                   substr(hola$Link,1,nchar(as.character(hola$Link))-20))
hola2<-merge(hola,jobads,by.x="nuevo",by.y="nuevo",all.x=TRUE,all.y=FALSE)
hola2 <- hola2[!duplicated(hola2$nuevo),]
# Test
sum(sort(hola$Link)==sort(group4$Link)) ==nrow(group4)
group4<-hola2[,importantcols]

# Group 5

hola<-group5
hola$nuevo2<-str_sub(hola$Link,-2,-1)
hola$nuevo3<-as.numeric(hola$nuevo2)
hola$nuevo<-ifelse(is.na(hola$nuevo3), substr(hola$Link,1,nchar(as.character(hola$Link))-19),
                   substr(hola$Link,1,nchar(as.character(hola$Link))-20))
hola2<-merge(hola,jobads,by.x="nuevo",by.y="nuevo",all.x=TRUE,all.y=FALSE)
hola2 <- hola2[!duplicated(hola2$nuevo),]
# Test
sum(sort(hola$Link)==sort(group5$Link)) ==nrow(group5)
group5<-hola2[,importantcols]

# Group 6

hola<-group6
hola$nuevo2<-str_sub(hola$Link,-2,-1)
hola$nuevo3<-as.numeric(hola$nuevo2)
hola$nuevo<-ifelse(is.na(hola$nuevo3), substr(hola$Link,1,nchar(as.character(hola$Link))-19),
                   substr(hola$Link,1,nchar(as.character(hola$Link))-20))
hola2<-merge(hola,jobads,by.x="nuevo",by.y="nuevo",all.x=TRUE,all.y=FALSE)
hola2 <- hola2[!duplicated(hola2$nuevo),]
# Test
sum(sort(hola$Link)==sort(group6$Link)) ==nrow(group6)
group6<-hola2[,importantcols]

# Group 7

hola<-group7
hola$nuevo2<-str_sub(hola$Link,-2,-1)
hola$nuevo3<-as.numeric(hola$nuevo2)
hola$nuevo<-ifelse(is.na(hola$nuevo3), substr(hola$Link,1,nchar(as.character(hola$Link))-19),
                   substr(hola$Link,1,nchar(as.character(hola$Link))-20))
hola2<-merge(hola,jobads,by.x="nuevo",by.y="nuevo",all.x=TRUE,all.y=FALSE)
hola2 <- hola2[!duplicated(hola2$nuevo),]
# Test
sum(sort(hola$Link)==sort(group7$Link)) ==nrow(group7)
group7<-hola2[,importantcols]

# Group 8

hola<-group8
hola$nuevo2<-str_sub(hola$Link,-2,-1)
hola$nuevo3<-as.numeric(hola$nuevo2)
hola$nuevo<-ifelse(is.na(hola$nuevo3), substr(hola$Link,1,nchar(as.character(hola$Link))-19),
                   substr(hola$Link,1,nchar(as.character(hola$Link))-20))
hola2<-merge(hola,jobads,by.x="nuevo",by.y="nuevo",all.x=TRUE,all.y=FALSE)
hola2 <- hola2[!duplicated(hola2$nuevo),]
# Test
sum(sort(hola$Link)==sort(group8$Link)) ==nrow(group8)
group8<-hola2[,importantcols]


# Combine datasets and save
data<-rbind(group1,group2,group3,group4,group5,group6,group7,group8)

# Examining data
library(plyr)
counting<-count(data, "Área")
counting<-counting[order(counting$freq),]
counting

unique(data$`Jornada laboral`)
data$fulltime<-ifelse(data$`Jornada laboral`=="Full-time",1,0)

counting<-count(data, "Ubicación")
counting<-counting[order(counting$freq),]
counting

data$caba<-ifelse(data$Ubicación=="Capital Federal, Buenos Aires",1,0)

# Modify wage
data$wage<-substr(data$wage,2,999)
data$wage<-substr(data$wage,1,5)
data$wage<-as.numeric(data$wage)

# Save data
write.csv(data,paste0(output,"/","totaldata.csv"))





###########################################################################
###########################################################################
###                                                                     ###
###                          WORKING WITH DATA                          ###
###                                                                     ###
###########################################################################
###########################################################################

library(stargazer)
library(bannerCommenter)

# Path
path<-""
setwd(path)


output<-""


# Load data
data<-read.csv(paste0(output,"/","totaldata.csv"))


#################################################################
##            Checking for successful randomization            ##
#################################################################



# Checking for randomization
cor(data$religion,data$caba,use="complete.obs")
cor(data$religion,data$gender,use="complete.obs")
cor(data$religion,data$fulltime,use="complete.obs")
cor(data$religion,data$wage,use="complete.obs")


x <- c("Caba dummy","Gender","Full time dummy","Wage")
randomtesting <- data.frame(matrix(ncol = length(x), nrow = 0))
colnames(randomtesting) <- x
randomtesting[1,]$`Caba dummy`<-cor(data$religion,data$caba,use="complete.obs")
randomtesting[1,]$Gender<- cor(data$religion,data$gender,use="complete.obs")
randomtesting[1,]$`Full time dummy`<-cor(data$religion,data$fulltime,use="complete.obs")
randomtesting[1,]$Wage<-cor(data$religion,data$wage,use="complete.obs")
rownames(randomtesting)<-"Correlation with Treatment dummy"

stargazer(randomtesting,summary=FALSE,title="Correlation between treatment variable and characteristics",
          label="fig:randomtesting")

# For microsoft word
stargazer(randomtesting,summary=FALSE,title="Correlation between treatment variable and characteristics",
          label="fig:randomtesting",type="html")

# Treatment variable
data$treatment<-0

data$treatment<-ifelse( data$group==1,1,
                        ifelse(data$group==2,1,
                               ifelse(data$group==3,0,
                                      ifelse(data$group==4,0,
                                             ifelse(data$group==5,1,
                                                    ifelse(data$group==6,1,
                                                           ifelse(data$group==7,0,ifelse(data$group==8,0,NA))))))))
sum(is.na(data$treatment))

# Economist ==1, engineer==0
data$bachelor<-NA

data[data$group==1,]$bachelor<-1
data[data$group==2,]$bachelor<-0
data[data$group==3,]$bachelor<-1
data[data$group==4,]$bachelor<-0
data[data$group==5,]$bachelor<-1
data[data$group==6,]$bachelor<-0
data[data$group==7,]$bachelor<-1
data[data$group==8,]$bachelor<-0





###########################################################################
###########################################################################
###                                                                     ###
###                   MODIFY DATASET TO ADD CALLBACKS                   ###
###                                                                     ###
###########################################################################
###########################################################################

data$callbacks<-0
# Groups callbacks
group1<-2
group2<-3
group3<-4
group4<-2
group5<-3
group6<-4
group7<-2
group8<-4

# Modify callbakcs
data[data$group==1,][0:group1,]$callbacks<-1
data[data$group==2,][0:group2,]$callbacks<-1
data[data$group==3,][0:group3,]$callbacks<-1
data[data$group==4,][0:group4,]$callbacks<-1
data[data$group==5,][0:group5,]$callbacks<-1
data[data$group==6,][0:group6,]$callbacks<-1
data[data$group==7,][0:group7,]$callbacks<-1
data[data$group==8,][0:group8,]$callbacks<-1


sum((data[data$group %in% c(1,2,5,6),]$callbacks))
sum((data[data$group %in% c(3,4,7,8),]$callbacks))


#################################################################
##                         Regressions                         ##
#################################################################

# For latex
model1 <- lm(data$callbacks~data$treatment)
summary(model1)

stargazer(model1,title = "Main results",
          covariate.labels =c("Jewish","Constant"),
          dep.var.labels = "Callbacks",
          omit.stat = c("adj.rsq","rsq","ser","f"))



model2 <- lm(data$callbacks~data$treatment+data$gender)
summary(model2)

model4 <- lm(data$callbacks~data$treatment+data$gender+data$treatment*data$gender)
summary(model4)

stargazer(model2,model4,title = "Gender effects",
          covariate.labels =c("Jewish","Gender","Interaction"),
          dep.var.labels = "Callbacks",
          omit.stat = c("adj.rsq","rsq","ser","f"))

# For microsoft word

stargazer(model1,title = "Main results",
          covariate.labels =c("Jewish","Constant"),
          dep.var.labels = "Callbacks",
          omit.stat = c("adj.rsq","rsq","ser","f"),type="html")

stargazer(model2,model4,title = "Gender effects",
          covariate.labels =c("Jewish","Gender","Interaction"),
          dep.var.labels = "Callbacks",
          omit.stat = c("adj.rsq","rsq","ser","f"),type="html")

# Check for linear dependence
library(plm)
newdataframe<-data
newdataframe<-subset(newdataframe,select=c("gender","treatment","bachelor"))
newdataframe$constan<-1
matrix<-as.matrix(newdataframe)
detect.lindep(matrix)





###########################################################################
###########################################################################
###                                                                     ###
###     MODIFY DATASET TO ADD CALLBACKS (with ads information)          ###
###                                                                     ###
###########################################################################
###########################################################################

library(stringr)
library(dplyr)

# Lowercase
data$Descripción<-tolower(data$Descripción)
data$Trabajo<-tolower(data$Trabajo)
data$Empresa<-tolower(data$Empresa)

# outcome variable
data$callbacks<-0
# Group 1
sort(unique(data[data$group==1,]$Empresa))

data[data$Empresa=="Certius Technologies"& !(is.na(data$Empresa)),]$callbacks<-1

data[data$nuevo=="https://www.zonajobs.com.ar/empleos/power-bi-full-stack-1916770"&
       !(is.na(data$Empresa)),]$callbacks<-1

data[data$nuevo=="https://www.zonajobs.com.ar/empleos/backend-engineer-capital-humano-1920656"&
       !(is.na(data$Empresa)),]$callbacks<-1

data %>%
  filter(str_detect(Empresa, "capital humano"))

data[data$nuevo=="https://www.zonajobs.com.ar/empleos/backend-engineer-capital-humano-1920656",]

#Group 2
sort(unique(data[data$group==2,]$Empresa))


# Search for orub srl
data %>%
  filter(str_detect(Descripción, "orub"))
data %>%
  filter(str_detect(Descripción, "ORUB"))
data %>%
  filter(str_detect(Descripción, "amoresano"))
data %>%
  filter(str_detect(Trabajo, "amoresano"))

data2<-data[data$group==2,] %>%
  filter(str_detect(Empresa, "amoresano"))
link<-data2$Link
data[data$Link==link &!(is.na(data$Empresa)),]$callbacks<-1

# Carta sur
# data[data$Empresa== &!(is.na(data$Empresa))]   Change this
data %>%
  filter(str_detect(Trabajo, "cartasur"))
data %>%
  filter(str_detect(Trabajo, "carta sur"))
data %>%
  filter(str_detect(Descripción, "cartasur"))
data %>%
  filter(str_detect(Descripción, "carta sur"))
data %>%
  filter(str_detect(Descripción, "castello"))
data %>%
  filter(str_detect(Empresa, "castello"))
data %>%
  filter(str_detect(Empresa, "carta sur"))
data %>%
  filter(str_detect(Empresa, "cartasur"))


# Group 3

# UNE
data %>%
  filter(str_detect(Empresa, "une"))
data[data$Link=="https://www.zonajobs.com.ar/empleos/analista-de-empleos-capacitacion-y-desarrollo-une-consulting-srl-1919276.html?indiceAviso=25",]$callbacks<-1

# Group 4

# Remax
data[data$group==4,] %>%
  filter(str_detect(Empresa, "remax"))
data[data$group==4&data$Link=="https://www.zonajobs.com.ar/empleos/agente-inmobiliario-re-max-genesis-remax-genesis-1916962.html?indiceAviso=1",]$callbacks<-1

# RGA
data[data$group==4,] %>%
  filter(str_detect(Empresa, "rga"))
data[data$group==4,] %>%
  filter(str_detect(Descripción, "rga"))

