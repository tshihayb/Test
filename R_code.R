# Initialization
rm(list=ls())

# Setting the working space so objects can be saved in it without refering to it again in saving functions
#you can change the path below to a location you prefer
#Try / or \\ or \ if you are using Mac
setwd("C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Output")

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

#Reading in NHANES 2009-2010 SAS xport datasets using SASxport package
demo_F <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/DEMO_F.XPT')
OHXDEN_F <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/OHXDEN_F.XPT')
OHXPER_F <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/OHXPER_F.XPT')

#Merging NHANES 2009-2010 datasets
#Wrong merging
NHANES_09_10 <- demo_F %>% full_join(OHXDEN_F,OHXPER_F, by='SEQN')
#Correct merging
NHANES_09_10 <- list(demo_F, OHXDEN_F, OHXPER_F) %>% reduce(left_join, by = "SEQN")

#Reading in NHANES 2011-2012 SAS xport datasets using SASxport package
demo_G <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/DEMO_G.XPT')
OHXDEN_G <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/OHXDEN_G.XPT')
OHXPER_G <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/OHXPER_G.XPT')

#Merging NHANES 2011-2012 datasets
#Wrong merging
NHANES_11_12 <- demo_G %>% full_join(OHXDEN_G,OHXPER_G, by='SEQN')
#Correct merging
NHANES_11_12 <- list(demo_G, OHXDEN_G, OHXPER_G) %>% reduce(left_join, by = "SEQN")


#Reading in NHANES 2013-2014 SAS xport datasets using SASxport package
demo_H <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/DEMO_H.XPT')
OHXDEN_H <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/OHXDEN_H.XPT')
OHXPER_H <- read.xport(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Raw data/OHXPER_H.XPT')

#Merging NHANES 2013-2014 datasets
#Wrong merging
NHANES_13_14 <- demo_H %>% full_join(OHXDEN_H,OHXPER_H, by='SEQN')
#Correct merging
NHANES_13_14 <- list(demo_H, OHXDEN_H, OHXPER_H) %>% reduce(left_join, by = "SEQN")


#Stacking all NHANES datasets
NHANES_09_14 <- bind_rows(NHANES_09_10,NHANES_11_12,NHANES_13_14,id=NULL)

View(NHANES_09_14)

#Create a tibble from data frame
tibble(NHANES_09_14)

# Cleaning names of variables (e.g., making them all with small letters)
# or seperating words by underscores
NHANES_09_14 <- clean_names(NHANES_09_14)

#Changing values of clinical perio variables with 99 to missing 
#This worked
variables <- NHANES_09_14[,58:588]
NHANES_09_14 <- NHANES_09_14 %>% mutate(variables=na_if(variables,99))

#Checking missing dtaa for clinical perio variables
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


dim(NHANES_09_14_2)
colnames(NHANES_09_14_2)

NHANES_09_14[variables]<- lapply(mutate[,variables], na_if(99))
head(NHANES_09_14_2)

View(NHANES_09_14[,85:588])
colnames(NHANES_09_14)

tibble(demo_F)






