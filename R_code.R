# Initialization
rm(list=ls())

# Setting the working space so objects can be saved in it without refering to it again in saving functions
#you can change the path below to a location you prefer
#Try / or \\ or \ if you are using Mac
setwd("C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data")

#Checking working directory
getwd()

#loading libraries
{
library(SASxport)
library(haven)
library(tidyverse)
library(dplyr)
library(epitools)
library(janitor)
library(skimr)
library(expss)
library(rstatix)
library(gtsummary)
library(vtable)
library(naniar)
}

{
#Reading in NHANES 2009-2010 SAS xport datasets using SASxport package
demo_F <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/DEMO_F.XPT')
OHXDEN_F <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/OHXDEN_F.XPT')
OHXPER_F <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/OHXPER_F.XPT')

#Or just load the file directly without that path since the working
#directory is set to the path
demo_F <- read.xport(file='DEMO_F.XPT')
OHXDEN_F <- read.xport(file='OHXDEN_F.XPT')
OHXPER_F <- read.xport(file='OHXPER_F.XPT')

#Merging NHANES 2009-2010 datasets
#Wrong merging
NHANES_09_10 <- demo_F %>% full_join(OHXDEN_F,OHXPER_F, by='SEQN')
#Correct merging
NHANES_09_10 <- list(demo_F, OHXDEN_F, OHXPER_F) %>% reduce(left_join, by = "SEQN")

#Reading in NHANES 2011-2012 SAS xport datasets using SASxport package
demo_G <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/DEMO_G.XPT')
OHXDEN_G <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/OHXDEN_G.XPT')
OHXPER_G <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/OHXPER_G.XPT')

#Or just load the file directly without that path since the working
#directory is set to the path
demo_G <- read.xport(file='DEMO_G.XPT')
OHXDEN_G <- read.xport(file='OHXDEN_G.XPT')
OHXPER_G <- read.xport(file='OHXPER_G.XPT')

#Merging NHANES 2011-2012 datasets
#Wrong merging
NHANES_11_12 <- demo_G %>% full_join(OHXDEN_G,OHXPER_G, by='SEQN')
#Correct merging
NHANES_11_12 <- list(demo_G, OHXDEN_G, OHXPER_G) %>% reduce(left_join, by = "SEQN")


#Reading in NHANES 2013-2014 SAS xport datasets using SASxport package
demo_H <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/DEMO_H.XPT')
OHXDEN_H <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/OHXDEN_H.XPT')
OHXPER_H <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/OHXPER_H.XPT')

#Or just load the file directly without that path since the working
#directory is set to the path
demo_H <- read.xport(file='DEMO_H.XPT')
OHXDEN_H <- read.xport(file='OHXDEN_H.XPT')
OHXPER_H <- read.xport(file='OHXPER_H.XPT')

#Merging NHANES 2013-2014 datasets
#Wrong merging
NHANES_13_14 <- demo_H %>% full_join(OHXDEN_H,OHXPER_H, by='SEQN')
#Correct merging
NHANES_13_14 <- list(demo_H, OHXDEN_H, OHXPER_H) %>% reduce(left_join, by = "SEQN")

#Stacking all NHANES datasets
NHANES_09_14 <- bind_rows(NHANES_09_10,NHANES_11_12,NHANES_13_14,id=NULL)
}

View(NHANES_09_14)

#Create a tibble from data frame
tibble(NHANES_09_14)

# Cleaning names of variables (e.g., making them all with small letters)
# or seperating words by underscores
NHANES_09_14 <- clean_names(NHANES_09_14)

#Changing values of clinical perio variables with 99 to missing 
#Determining column number of perio variables
grep("ohx02cjd",colnames(NHANES_09_14))
grep("ohx31laa",colnames(NHANES_09_14))

#This worked
NHANES_09_14 <- NHANES_09_14 %>% mutate(across(c(85:588), ~na_if(.,99)))
#Or this
NHANES_09_14 <- NHANES_09_14 %>% mutate(across(c("ohx02cjd":"ohx31laa"), ~na_if(.,99)))

#Below did not work
variables <- NHANES_09_14[,85:588]
NHANES_09_14 <- NHANES_09_14 %>% mutate(variables=na_if(variables,99))

#Checking missing data for clinical perio variables
is.na(NHANES_09_14$ohx02cja)

#Below code did not work
NHANES_09_14_2 <- NHANES_09_14 %>% select(158:1161)%>% mutate(age=na_if(age,36))
NHANES_09_14_2 <- NHANES_09_14 %>% mutate(across(select(85:588)), na_if(36))
NHANES_09_14_2 <- NHANES_09_14 %>% mutate(across(.col=85:588), na_if(85:588,36))
NHANES_09_14_2 <- NHANES_09_14 %>% mutate(across(.col=85:588), .col=na_if(.col,36))
str(NHANES_09_14[,85:588])
NHANES_sub <- NHANES_09_14 %>% select(85:588)
NHANES_sub2 <- NHANES_sub %>% select(.col=where(is.numeric))
NHANES_sub3 <- NHANES_sub %>% select(.col=where(is.integer))
View(NHANES_09_14[,85:588])
NHANES_09_14 %>% class(across(select(58:588)))
class(NHANES_09_14[,58:588])
NHANES_09_14%>% 
  replace_with_na_at(.vars = c(NHANES_09_14,58:588),
                     condition = ~.vars == 99)
NHANES_09_14_2 <- NHANES_09_14 %>% mutate_at(vars(c(58:588)), na_if(vars,99))
NHANES_09_14[variables]<- lapply(mutate[,variables], na_if(99))
head(NHANES_09_14_2)
View(NHANES_09_14[,85:588])
colnames(NHANES_09_14)


#Counting number of teeth
#Creating binary variable for permanent tooth of each tooth
{
NHANES_09_14$permtooth_2 <- NHANES_09_14$ohx02tc==2
NHANES_09_14$permtooth_3 <-NHANES_09_14$ohx03tc==2
NHANES_09_14$permtooth_4 <-NHANES_09_14$ohx04tc==2
NHANES_09_14$permtooth_5 <-NHANES_09_14$ohx05tc==2
NHANES_09_14$permtooth_6 <-NHANES_09_14$ohx06tc==2
NHANES_09_14$permtooth_7 <-NHANES_09_14$ohx07tc==2
NHANES_09_14$permtooth_8 <-NHANES_09_14$ohx08tc==2
NHANES_09_14$permtooth_9 <-NHANES_09_14$ohx09tc==2
NHANES_09_14$permtooth_10 <-NHANES_09_14$ohx10tc==2
NHANES_09_14$permtooth_11 <-NHANES_09_14$ohx11tc==2
NHANES_09_14$permtooth_12 <-NHANES_09_14$ohx12tc==2
NHANES_09_14$permtooth_13 <-NHANES_09_14$ohx13tc==2
NHANES_09_14$permtooth_14 <-NHANES_09_14$ohx14tc==2
NHANES_09_14$permtooth_15 <-NHANES_09_14$ohx15tc==2
NHANES_09_14$permtooth_18 <-NHANES_09_14$ohx18tc==2
NHANES_09_14$permtooth_19 <-NHANES_09_14$ohx19tc==2
NHANES_09_14$permtooth_20 <-NHANES_09_14$ohx20tc==2
NHANES_09_14$permtooth_21 <-NHANES_09_14$ohx21tc==2
NHANES_09_14$permtooth_22 <-NHANES_09_14$ohx22tc==2
NHANES_09_14$permtooth_23 <-NHANES_09_14$ohx23tc==2
NHANES_09_14$permtooth_24 <-NHANES_09_14$ohx24tc==2
NHANES_09_14$permtooth_25 <-NHANES_09_14$ohx25tc==2
NHANES_09_14$permtooth_26 <-NHANES_09_14$ohx26tc==2
NHANES_09_14$permtooth_27 <-NHANES_09_14$ohx27tc==2
NHANES_09_14$permtooth_28 <-NHANES_09_14$ohx28tc==2
NHANES_09_14$permtooth_29 <-NHANES_09_14$ohx29tc==2
NHANES_09_14$permtooth_30 <-NHANES_09_14$ohx30tc==2
NHANES_09_14$permtooth_31 <-NHANES_09_14$ohx31tc==2
}

#Making sure permtooth variables were correctly coded
View(NHANES_09_14 %>% select(seqn, ohx02tc, permtooth_2) %>% slice(1:10))
NHANES_09_14 %>% filter(ohx02tc==2 & permtooth_2==F||NA) %>% select(seqn, ohx02tc, permtooth_2) %>% slice(1:10)
NHANES_09_14 %>% filter(ohx02tc!=2 & permtooth_2==T||NA) %>% select(seqn, ohx02tc, permtooth_2) %>% slice(1:10)

#Getting the column number of binary perm tooth variables
grep("permtooth_2",colnames(NHANES_09_14))
grep("permtooth_31",colnames(NHANES_09_14))

#Counting the number of permenent teeth for each person
NHANES_09_14$toothcount <- apply(NHANES_09_14[,674:701],1,sum, na.rm=T)

#Making sure they were correctly coded
View(NHANES_09_14 %>% select(seqn, permtooth_2:permtooth_31, toothcount) %>% slice(1:10))

#Restricting to those >=30, had complete perio exam, and had at least
#one permanent tooth
NHANES_09_14 <- NHANES_09_14 %>% filter(toothcount>=1 & ohdpdsts==1 & ridageyr>=30)

#Comparing toothcount here with SAS
View(NHANES_09_14 %>% select(seqn ,toothcount) %>% arrange(seqn)) %>% slice(1:10)


#Getting the maximum interproximal CAL
NHANES_09_14[,c("ohx02lad","ohx02las","ohx02lap","ohx02laa")]

{
NHANES_09_14$max_cal_2 <- max_row(NHANES_09_14[,c("ohx02lad","ohx02las","ohx02lap","ohx02laa")])
NHANES_09_14$max_cal_3 <- max_row(NHANES_09_14[,c("ohx03lad","ohx03las","ohx03lap","ohx03laa")])
NHANES_09_14$max_cal_4 <- max_row(NHANES_09_14[,c("ohx04lad","ohx04las","ohx04lap","ohx04laa")])
NHANES_09_14$max_cal_5 <- max_row(NHANES_09_14[,c("ohx05lad","ohx05las","ohx05lap","ohx05laa")])
NHANES_09_14$max_cal_6 <- max_row(NHANES_09_14[,c("ohx06lad","ohx06las","ohx06lap","ohx06laa")])
NHANES_09_14$max_cal_7 <- max_row(NHANES_09_14[,c("ohx07lad","ohx07las","ohx07lap","ohx07laa")])
NHANES_09_14$max_cal_8 <- max_row(NHANES_09_14[,c("ohx08lad","ohx08las","ohx08lap","ohx08laa")])
NHANES_09_14$max_cal_9 <- max_row(NHANES_09_14[,c("ohx09lad","ohx09las","ohx09lap","ohx09laa")])
NHANES_09_14$max_cal_10 <- max_row(NHANES_09_14[,c("ohx10lad","ohx10las","ohx10lap","ohx10laa")])
NHANES_09_14$max_cal_11 <- max_row(NHANES_09_14[,c("ohx11lad","ohx11las","ohx11lap","ohx11laa")])
NHANES_09_14$max_cal_12 <- max_row(NHANES_09_14[,c("ohx12lad","ohx12las","ohx12lap","ohx12laa")])
NHANES_09_14$max_cal_13 <- max_row(NHANES_09_14[,c("ohx13lad","ohx13las","ohx13lap","ohx13laa")])
NHANES_09_14$max_cal_14 <- max_row(NHANES_09_14[,c("ohx14lad","ohx14las","ohx14lap","ohx14laa")])
NHANES_09_14$max_cal_15 <- max_row(NHANES_09_14[,c("ohx15lad","ohx15las","ohx15lap","ohx15laa")])
NHANES_09_14$max_cal_18 <- max_row(NHANES_09_14[,c("ohx18lad","ohx18las","ohx18lap","ohx18laa")])
NHANES_09_14$max_cal_19 <- max_row(NHANES_09_14[,c("ohx19lad","ohx19las","ohx19lap","ohx19laa")])
NHANES_09_14$max_cal_20 <- max_row(NHANES_09_14[,c("ohx20lad","ohx20las","ohx20lap","ohx20laa")])
NHANES_09_14$max_cal_21 <- max_row(NHANES_09_14[,c("ohx21lad","ohx21las","ohx21lap","ohx21laa")])
NHANES_09_14$max_cal_22 <- max_row(NHANES_09_14[,c("ohx22lad","ohx22las","ohx22lap","ohx22laa")])
NHANES_09_14$max_cal_23 <- max_row(NHANES_09_14[,c("ohx23lad","ohx23las","ohx23lap","ohx23laa")])
NHANES_09_14$max_cal_24 <- max_row(NHANES_09_14[,c("ohx24lad","ohx24las","ohx24lap","ohx24laa")])
NHANES_09_14$max_cal_25 <- max_row(NHANES_09_14[,c("ohx25lad","ohx25las","ohx25lap","ohx25laa")])
NHANES_09_14$max_cal_26 <- max_row(NHANES_09_14[,c("ohx26lad","ohx26las","ohx26lap","ohx26laa")])
NHANES_09_14$max_cal_27 <- max_row(NHANES_09_14[,c("ohx27lad","ohx27las","ohx27lap","ohx27laa")])
NHANES_09_14$max_cal_28 <- max_row(NHANES_09_14[,c("ohx28lad","ohx28las","ohx28lap","ohx28laa")])
NHANES_09_14$max_cal_29 <- max_row(NHANES_09_14[,c("ohx29lad","ohx29las","ohx29lap","ohx29laa")])
NHANES_09_14$max_cal_30 <- max_row(NHANES_09_14[,c("ohx30lad","ohx30las","ohx30lap","ohx30laa")])
NHANES_09_14$max_cal_31 <- max_row(NHANES_09_14[,c("ohx31lad","ohx31las","ohx31lap","ohx31laa")])
}

NHANES_09_14 %>% select("ohx02lad","ohx02las","ohx02lap","ohx02laa","max_cal_2") %>% slice(1:10)

#cal <- NHANES_09_14 %>% select(ends_with("lad"), ends_with("las"), ends_with("lap"),
#ends_with("laa"))

#Getting the maximum interproximal PD
NHANES_09_14[,c("ohx02pcd","ohx02pcs","ohx02pcp","ohx02pca")]

{
  NHANES_09_14$max_pd_2 <- max_row(NHANES_09_14[,c("ohx02pcd","ohx02pcs","ohx02pcp","ohx02pca")])
  NHANES_09_14$max_pd_3 <- max_row(NHANES_09_14[,c("ohx03pcd","ohx03pcs","ohx03pcp","ohx03pca")])
  NHANES_09_14$max_pd_4 <- max_row(NHANES_09_14[,c("ohx04pcd","ohx04pcs","ohx04pcp","ohx04pca")])
  NHANES_09_14$max_pd_5 <- max_row(NHANES_09_14[,c("ohx05pcd","ohx05pcs","ohx05pcp","ohx05pca")])
  NHANES_09_14$max_pd_6 <- max_row(NHANES_09_14[,c("ohx06pcd","ohx06pcs","ohx06pcp","ohx06pca")])
  NHANES_09_14$max_pd_7 <- max_row(NHANES_09_14[,c("ohx07pcd","ohx07pcs","ohx07pcp","ohx07pca")])
  NHANES_09_14$max_pd_8 <- max_row(NHANES_09_14[,c("ohx08pcd","ohx08pcs","ohx08pcp","ohx08pca")])
  NHANES_09_14$max_pd_9 <- max_row(NHANES_09_14[,c("ohx09pcd","ohx09pcs","ohx09pcp","ohx09pca")])
  NHANES_09_14$max_pd_10 <- max_row(NHANES_09_14[,c("ohx10pcd","ohx10pcs","ohx10pcp","ohx10pca")])
  NHANES_09_14$max_pd_11 <- max_row(NHANES_09_14[,c("ohx11pcd","ohx11pcs","ohx11pcp","ohx11pca")])
  NHANES_09_14$max_pd_12 <- max_row(NHANES_09_14[,c("ohx12pcd","ohx12pcs","ohx12pcp","ohx12pca")])
  NHANES_09_14$max_pd_13 <- max_row(NHANES_09_14[,c("ohx13pcd","ohx13pcs","ohx13pcp","ohx13pca")])
  NHANES_09_14$max_pd_14 <- max_row(NHANES_09_14[,c("ohx14pcd","ohx14pcs","ohx14pcp","ohx14pca")])
  NHANES_09_14$max_pd_15 <- max_row(NHANES_09_14[,c("ohx15pcd","ohx15pcs","ohx15pcp","ohx15pca")])
  NHANES_09_14$max_pd_18 <- max_row(NHANES_09_14[,c("ohx18pcd","ohx18pcs","ohx18pcp","ohx18pca")])
  NHANES_09_14$max_pd_19 <- max_row(NHANES_09_14[,c("ohx19pcd","ohx19pcs","ohx19pcp","ohx19pca")])
  NHANES_09_14$max_pd_20 <- max_row(NHANES_09_14[,c("ohx20pcd","ohx20pcs","ohx20pcp","ohx20pca")])
  NHANES_09_14$max_pd_21 <- max_row(NHANES_09_14[,c("ohx21pcd","ohx21pcs","ohx21pcp","ohx21pca")])
  NHANES_09_14$max_pd_22 <- max_row(NHANES_09_14[,c("ohx22pcd","ohx22pcs","ohx22pcp","ohx22pca")])
  NHANES_09_14$max_pd_23 <- max_row(NHANES_09_14[,c("ohx23pcd","ohx23pcs","ohx23pcp","ohx23pca")])
  NHANES_09_14$max_pd_24 <- max_row(NHANES_09_14[,c("ohx24pcd","ohx24pcs","ohx24pcp","ohx24pca")])
  NHANES_09_14$max_pd_25 <- max_row(NHANES_09_14[,c("ohx25pcd","ohx25pcs","ohx25pcp","ohx25pca")])
  NHANES_09_14$max_pd_26 <- max_row(NHANES_09_14[,c("ohx26pcd","ohx26pcs","ohx26pcp","ohx26pca")])
  NHANES_09_14$max_pd_27 <- max_row(NHANES_09_14[,c("ohx27pcd","ohx27pcs","ohx27pcp","ohx27pca")])
  NHANES_09_14$max_pd_28 <- max_row(NHANES_09_14[,c("ohx28pcd","ohx28pcs","ohx28pcp","ohx28pca")])
  NHANES_09_14$max_pd_29 <- max_row(NHANES_09_14[,c("ohx29pcd","ohx29pcs","ohx29pcp","ohx29pca")])
  NHANES_09_14$max_pd_30 <- max_row(NHANES_09_14[,c("ohx30pcd","ohx30pcs","ohx30pcp","ohx30pca")])
  NHANES_09_14$max_pd_31 <- max_row(NHANES_09_14[,c("ohx31pcd","ohx31pcs","ohx31pcp","ohx31pca")])
}

NHANES_09_14 %>% select("ohx02pcd","ohx02pcs","ohx02pcp","ohx02pca","max_pd_2") %>% slice(1:10)


View(NHANES_09_14 %>% select(ohx02lad:ohx31lad))
View(NHANES_09_14 %>% select(-ohx02lad:ohx31lad))
View(NHANES_09_14 %>% select(where(ohx02lad:ohx31lad))

NHANES_13_14[,"ohx02lad"]

NHANES_09_14 %>% select(starts_with("ohx"))



NHANES_09_14 %>% select(ends_with("lad", "las", "lap", "laa"))

NHANES_09_14 %>% select(ends_with("cjd", "cjp", "cjs","cja"))



teeth <- NHANES_09_14 %>% select(ohx02tc:ohx31tc)
teeth <- NHANES_09_14[,47:76]

for (i in seq_along(teeth)){
NHANES_09_14$perm_tooth_(i) <-teeth(i)==2}

for (i in seq_along(teeth)){
  NHANES_09_14$perm_tooth(i) <-teeth[i]==2}

for (i in 1:30){
  NHANES_09_14$perm_tooth(i) <- if (teeth[i]==2){
  1} 
  else 
  {0}
}

#This worked over columns but all IDs have the same count
NHANES_09_14$toothcount <- 0
for (i in 1:28) {
  if (teeth[i]==2)
  {NHANES_09_14$toothcount <- NHANES_09_14$toothcount+1
  }
}

NHANES_09_14$toothcount 


NHANES_09_14$toothcount <- 0
for (i in 1:30) {
  if (teeth[i]==2)
  {NHANES_09_14 %>% group_by(seqn) %>%  NHANES_09_14$toothcount <- NHANES_09_14$toothcount+1
  }
}

#Through mutate across and apply function
check <- NHANES_09_14 %>% mutate(across(.col=c(ohx02tc:ohx31tc), .fns=as.logical)) 

check <- NHANES_09_14 %>% mutate(across(.col=c(ohx02tc:ohx31tc), ~if_else(.col ==22),T,F)) 


sum(NHANES_09_14$permtooth_31, na.rm = T)


sub <- NHANES_09_14[,675:702]
sapply(sub,sum, na.rm=T)
mapply(sub,sum, na.rm=T)
lapply(sub,sum, na.rm=T)
apply(sub,1,sum, na.rm=T)
dim(NHANES_09_14[,675:702])

mapply(==2, ohx02tc:ohx31tc)
mapply(if )

NHANES_09_14[,678:705]


NHANES_09_14$toothcount <- 0
for (i in 1:nrow(NHANES_09_14[,678:705])) {
  if (NHANES_09_14[,678:705][i]==T)
  {NHANES_09_14$toothcount <- NHANES_09_14$toothcount+1
  }
}

for (b in 1:nrow(NHANES_09_14)){
NHANES_09_14$toothcount[b] <- 0}

for (b in 1:nrow(teeth)){
  teeth$toothcount[b] <- 0}

for (b in 1:nrow(teeth)){
  for (i in ncol(teeth)) {
  if (teeth[i]==2)
  {teeth$toothcount[b] <- teeth$toothcount[b]+1
  }
  }
}

teeth$toothcount

for (b in 1:nrow(NHANES_09_14)){
  NHANES_09_14$age_sqr[b] <- NHANES_09_14$ridageyr[b]*NHANES_09_14$ridageyr[b]
    }

NHANES_09_14 %>% select(seqn, ridageyr,age_sqr) %>% slice (1:20)

#Getting col number
grep("ohx02tc",colnames(NHANES_09_14))
grep("ohx31tc",colnames(NHANES_09_14))


for (b in 1:nrow(NHANES_09_14[,47:76])){
  for (i in 1:ncol(NHANES_09_14[,47:76])) {
    if (NHANES_09_14[b,i]==2)
    {NHANES_09_14$toothcount[b,] <- NHANES_09_14$toothcount[b,]+1
    }
  }
}

for (b in 1:nrow(teeth)){
  for (i in 1:ncol(teeth)) {
    if (teeth[b,i]==2)
    {teeth$toothcount[b,] <- teeth$toothcount[b,]+1
    }
  }
}

for (seqn in nrow(teeth)){
  for (i in ncol(teeth)) {
    if (teeth[seqn,i]==2)
    {teeth$toothcount[seqn,] <- teeth$toothcount[seqn,]+1
    }
  }
}

View(teeth$toothcount)
View(NHANES_09_14[,47:76])
nrow(NHANES_09_14[,47:76])
ncol(NHANES_09_14[,47:76])
nrow(NHANES_09_14)
ncol(NHANES_09_14)
length(NHANES_09_14$seqn)
nrow(NHANES_09_14$seqn)
dim(NHANES_09_14$seqn)
dim(teeth)
View(teeth)
NHANES_09_14$toothcount 

class(teeth)
NHANES_09_14 %>% select(seqn, ohx02tc:ohx31tc, toothcount) %>% slice (1:2)

for (i in 1:
table(NHANES_09_14$perm_tooth)
prop.table(table(NHANES_09_14$perm_tooth))

dim(NHANES_09_14_2)
colnames(NHANES_09_14_2)










