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
library(summarytools)
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

#Alternative and more efficient way of calculating toothcount
NHANES_09_14$toothcount <- count_row_if(2, NHANES_09_14 %>% select(c(ohx02tc:ohx31tc),-c(ohx16tc:ohx17tc)))

#Checking if this way results in same numbers
View(NHANES_09_14 %>% select(c(ohx02tc:ohx31tc),-c(ohx16tc:ohx17tc),toothcount) %>% slice(1:10))
     
#Restricting to those >=30, had complete perio exam, and had at least
#one permanent tooth
NHANES_09_14 <- NHANES_09_14 %>% filter(toothcount>=1 & ohdpdsts==1 & ridageyr>=30)

#Comparing toothcount here with SAS
View(NHANES_09_14 %>% select(seqn ,toothcount) %>% arrange(seqn)) %>% slice(1:10)


#Saving the dataset
#Specifying location of saved dataset
setwd("C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Output")

write_csv(NHANES_09_14, 'perio.csv',na = "NA", append = FALSE, col_names = T)

# CLear environment
rm(list=ls())

#Load the saved dataset
NHANES_09_14 <- read_csv(file='perio.csv',col_names = T)
NHANES_09_14 <- read_csv(file='C:/Users/Tshih/OneDrive/Periodontal side differences in NHANES/Test/Output/perio.csv', col_names = T)

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

NHANES_09_14 %>% select(ohx02lad,ohx02las,ohx02lap,ohx02laa,max_cal_2) %>% slice(1:10)

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

NHANES_09_14 %>% select(ohx02pcd,ohx02pcs,ohx02pcp,ohx02pca,max_pd_2) %>% slice(1:10)

#Creating periodontitis variable
#Number of teeth with CAL>=6 mm
grep("max_cal_2",colnames(NHANES_09_14))
grep("max_cal_31",colnames(NHANES_09_14))

NHANES_09_14$tooth_cal_ge_6 <- count_row_if(ge(6), NHANES_09_14[,703:730])

View(NHANES_09_14 %>% select(max_cal_2:max_cal_31, tooth_cal_ge_6) %>% slice(1:10))

#This gives the sum (not what we want)
NHANES_09_14$tooth_cal_ge_6 <- sum_row_if(ge(6), NHANES_09_14[,703:730])

#Number of teeth with CAL>=4 mm
NHANES_09_14$tooth_cal_ge_4 <- count_row_if(ge(4), NHANES_09_14[,703:730])

View(NHANES_09_14 %>% select(max_cal_2:max_cal_31, tooth_cal_ge_4) %>% slice(1:10))

#Number of teeth with PD>=5 mm
grep("max_pd_2",colnames(NHANES_09_14))
grep("max_pd_31",colnames(NHANES_09_14))

NHANES_09_14$tooth_pd_ge_5 <- count_row_if(ge(5), NHANES_09_14[,731:758])

View(NHANES_09_14 %>% select(max_pd_2:max_pd_31, tooth_pd_ge_5) %>% slice(1:10))

#Number of teeth with PD>=4 mm
NHANES_09_14$tooth_pd_ge_4 <- count_row_if(ge(4), NHANES_09_14[,731:758])

View(NHANES_09_14 %>% select(max_pd_2:max_pd_31, tooth_pd_ge_4) %>% slice(1:10))

#Number of sites with CAL>=3 mm
site_NHANES <- NHANES_09_14 %>% select(-contains("lam"), -contains("lal"),
  -contains("cjd"), -contains("cjs"), -contains("cjp"), -contains("cja"),
  -contains("cjm"), -contains("cjl"), -contains("pcd"), -contains("pcs"),
  -contains("pcp"), -contains("pca"), -contains("pcm"), -contains("pcl"))

grep("ohx02lad",colnames(site_NHANES))
grep("ohx31laa",colnames(site_NHANES))

NHANES_09_14$site_cal_ge_3 <- count_row_if(ge(3), site_NHANES[,85:196])

View(NHANES_09_14 %>% select(ohx02lad, ohx03lad, ohx04lad, ohx05lad, ohx06lad, ohx07lad, ohx08lad,
                             ohx09lad, ohx10lad, ohx11lad, ohx12lad, ohx13lad, ohx14lad, ohx15lad,
                             ohx18lad, ohx19lad, ohx20lad, ohx21lad, ohx22lad, ohx23lad, ohx24lad, 
                             ohx25lad, ohx26lad, ohx27lad, ohx28lad, ohx29lad, ohx30lad, ohx31lad,
                             
                             ohx02las, ohx03las, ohx04las, ohx05las, ohx06las, ohx07las, ohx08las, 
                             ohx09las, ohx10las, ohx11las, ohx12las, ohx13las, ohx14las, ohx15las, 
                             ohx18las, ohx19las, ohx20las, ohx21las, ohx22las, ohx23las, ohx24las, 
                             ohx25las, ohx26las, ohx27las, ohx28las, ohx29las, ohx30las, ohx31las,
                             
                             ohx02lap, ohx03lap, ohx04lap, ohx05lap, ohx06lap, ohx07lap, ohx08lap, 
                             ohx09lap, ohx10lap, ohx11lap, ohx12lap, ohx13lap, ohx14lap, ohx15lap, 
                             ohx18lap, ohx19lap, ohx20lap, ohx21lap, ohx22lap, ohx23lap, ohx24lap, 
                             ohx25lap, ohx26lap, ohx27lap, ohx28lap, ohx29lap, ohx30lap, ohx31lap,
                             
                             ohx02laa, ohx03laa, ohx04laa, ohx05laa, ohx06laa, ohx07laa, ohx08laa, 
                             ohx09laa, ohx10laa, ohx11laa, ohx12laa, ohx13laa, ohx14laa, ohx15laa, 
                             ohx18laa, ohx19laa, ohx20laa, ohx21laa, ohx22laa, ohx23laa, ohx24laa, 
                             ohx25laa, ohx26laa, ohx27laa, ohx28laa, ohx29laa, ohx30laa, ohx31laa,
                             site_cal_ge_3) %>% slice(1:10))

#Creating periodontitis
#COunting number of sites missing CAL and set periodontitis to missing if all
#112 sites were missing CAL
NHANES_09_14$miss_count <- count_row_if(NA, NHANES_09_14 %>% select(ohx02lad, ohx03lad, ohx04lad, ohx05lad, ohx06lad, ohx07lad, ohx08lad,
  ohx09lad, ohx10lad, ohx11lad, ohx12lad, ohx13lad, ohx14lad, ohx15lad,
  ohx18lad, ohx19lad, ohx20lad, ohx21lad, ohx22lad, ohx23lad, ohx24lad, 
  ohx25lad, ohx26lad, ohx27lad, ohx28lad, ohx29lad, ohx30lad, ohx31lad,
                                                                    
  ohx02las, ohx03las, ohx04las, ohx05las, ohx06las, ohx07las, ohx08las, 
  ohx09las, ohx10las, ohx11las, ohx12las, ohx13las, ohx14las, ohx15las, 
  ohx18las, ohx19las, ohx20las, ohx21las, ohx22las, ohx23las, ohx24las, 
  ohx25las, ohx26las, ohx27las, ohx28las, ohx29las, ohx30las, ohx31las,
                                                                    
  ohx02lap, ohx03lap, ohx04lap, ohx05lap, ohx06lap, ohx07lap, ohx08lap, 
  ohx09lap, ohx10lap, ohx11lap, ohx12lap, ohx13lap, ohx14lap, ohx15lap, 
  ohx18lap, ohx19lap, ohx20lap, ohx21lap, ohx22lap, ohx23lap, ohx24lap, 
  ohx25lap, ohx26lap, ohx27lap, ohx28lap, ohx29lap, ohx30lap, ohx31lap,
                                                                    
  ohx02laa, ohx03laa, ohx04laa, ohx05laa, ohx06laa, ohx07laa, ohx08laa, 
  ohx09laa, ohx10laa, ohx11laa, ohx12laa, ohx13laa, ohx14laa, ohx15laa, 
  ohx18laa, ohx19laa, ohx20laa, ohx21laa, ohx22laa, ohx23laa, ohx24laa, 
  ohx25laa, ohx26laa, ohx27laa, ohx28laa, ohx29laa, ohx30laa, ohx31laa))

#Checking distribution of missing cal sites
table(NHANES_09_14$miss_count)

#Coding periodontitis
NHANES_09_14 <- NHANES_09_14 %>% mutate(periodontitis=case_when(
  tooth_cal_ge_6>=2 & tooth_pd_ge_5>= 1 ~ 3,
  tooth_cal_ge_4>=2 | tooth_pd_ge_5>= 2 ~ 2,
  site_cal_ge_3>= 2 & (tooth_pd_ge_4>= 2 | tooth_pd_ge_5>= 1) ~ 1,
  miss_count==112~NA_real_,
  T ~ 0,))

#Converting periodontitis to a factor
class(NHANES_09_14$periodontitis)
NHANES_09_14$periodontitis <- as.factor(NHANES_09_14$periodontitis)
class(NHANES_09_14$periodontitis)
levels(NHANES_09_14$periodontitis)

#Checking coding of periodontitis
View(NHANES_09_14 %>% select(tooth_cal_ge_6, tooth_pd_ge_5,
  tooth_cal_ge_4, tooth_pd_ge_5,
  site_cal_ge_3, tooth_pd_ge_4, tooth_pd_ge_5, periodontitis) %>% slice(1:10))

#Checking frequency of periodontitis
table(NHANES_09_14$periodontitis, useNA = "ifany")
prop.table(table(NHANES_09_14$periodontitis, useNA = "ifany"))


#Creating binary periodontitis varaibles with different cut-off points
#Transform periodontitis to numeric back first
NHANES_09_14$periodontitis <- as.numeric(NHANES_09_14$periodontitis)

#Mild as the cut-off
NHANES_09_14 <- NHANES_09_14 %>% mutate(perio_ge_mild=if_else(periodontitis>=1,"1","0"))

#Moderate as the cut-off
NHANES_09_14 <- NHANES_09_14 %>% mutate(perio_ge_mod=if_else(periodontitis>=2,"1","0"))

#Severe as the cut-off
NHANES_09_14 <- NHANES_09_14 %>% mutate(perio_ge_sev=if_else(periodontitis>=3,"1","0"))

#Change periodontitis variables to factors
NHANES_09_14[,c("periodontitis","perio_ge_mild","perio_ge_mod","perio_ge_sev")] <- apply(NHANES_09_14[,c("periodontitis","perio_ge_mild","perio_ge_mod","perio_ge_sev")],2,as.factor)

#Checking class after conversion
class(NHANES_09_14$periodontitis)
class(NHANES_09_14$perio_ge_mild)
class(NHANES_09_14$perio_ge_mod)
class(NHANES_09_14$perio_ge_sev)


#Getting the mean interproximal CAL per tooth
NHANES_09_14[,c("ohx02lad","ohx02las","ohx02lap","ohx02laa")]

{
  NHANES_09_14$mean_cal_2 <- mean_row(NHANES_09_14[,c("ohx02lad","ohx02las","ohx02lap","ohx02laa")],na.rm = T)
  NHANES_09_14$mean_cal_3 <- mean_row(NHANES_09_14[,c("ohx03lad","ohx03las","ohx03lap","ohx03laa")],na.rm = T)
  NHANES_09_14$mean_cal_4 <- mean_row(NHANES_09_14[,c("ohx04lad","ohx04las","ohx04lap","ohx04laa")],na.rm = T)
  NHANES_09_14$mean_cal_5 <- mean_row(NHANES_09_14[,c("ohx05lad","ohx05las","ohx05lap","ohx05laa")],na.rm = T)
  NHANES_09_14$mean_cal_6 <- mean_row(NHANES_09_14[,c("ohx06lad","ohx06las","ohx06lap","ohx06laa")],na.rm = T)
  NHANES_09_14$mean_cal_7 <- mean_row(NHANES_09_14[,c("ohx07lad","ohx07las","ohx07lap","ohx07laa")],na.rm = T)
  NHANES_09_14$mean_cal_8 <- mean_row(NHANES_09_14[,c("ohx08lad","ohx08las","ohx08lap","ohx08laa")],na.rm = T)
  NHANES_09_14$mean_cal_9 <- mean_row(NHANES_09_14[,c("ohx09lad","ohx09las","ohx09lap","ohx09laa")],na.rm = T)
  NHANES_09_14$mean_cal_10 <- mean_row(NHANES_09_14[,c("ohx10lad","ohx10las","ohx10lap","ohx10laa")],na.rm = T)
  NHANES_09_14$mean_cal_11 <- mean_row(NHANES_09_14[,c("ohx11lad","ohx11las","ohx11lap","ohx11laa")],na.rm = T)
  NHANES_09_14$mean_cal_12 <- mean_row(NHANES_09_14[,c("ohx12lad","ohx12las","ohx12lap","ohx12laa")],na.rm = T)
  NHANES_09_14$mean_cal_13 <- mean_row(NHANES_09_14[,c("ohx13lad","ohx13las","ohx13lap","ohx13laa")],na.rm = T)
  NHANES_09_14$mean_cal_14 <- mean_row(NHANES_09_14[,c("ohx14lad","ohx14las","ohx14lap","ohx14laa")],na.rm = T)
  NHANES_09_14$mean_cal_15 <- mean_row(NHANES_09_14[,c("ohx15lad","ohx15las","ohx15lap","ohx15laa")],na.rm = T)
  NHANES_09_14$mean_cal_18 <- mean_row(NHANES_09_14[,c("ohx18lad","ohx18las","ohx18lap","ohx18laa")],na.rm = T)
  NHANES_09_14$mean_cal_19 <- mean_row(NHANES_09_14[,c("ohx19lad","ohx19las","ohx19lap","ohx19laa")],na.rm = T)
  NHANES_09_14$mean_cal_20 <- mean_row(NHANES_09_14[,c("ohx20lad","ohx20las","ohx20lap","ohx20laa")],na.rm = T)
  NHANES_09_14$mean_cal_21 <- mean_row(NHANES_09_14[,c("ohx21lad","ohx21las","ohx21lap","ohx21laa")],na.rm = T)
  NHANES_09_14$mean_cal_22 <- mean_row(NHANES_09_14[,c("ohx22lad","ohx22las","ohx22lap","ohx22laa")],na.rm = T)
  NHANES_09_14$mean_cal_23 <- mean_row(NHANES_09_14[,c("ohx23lad","ohx23las","ohx23lap","ohx23laa")],na.rm = T)
  NHANES_09_14$mean_cal_24 <- mean_row(NHANES_09_14[,c("ohx24lad","ohx24las","ohx24lap","ohx24laa")],na.rm = T)
  NHANES_09_14$mean_cal_25 <- mean_row(NHANES_09_14[,c("ohx25lad","ohx25las","ohx25lap","ohx25laa")],na.rm = T)
  NHANES_09_14$mean_cal_26 <- mean_row(NHANES_09_14[,c("ohx26lad","ohx26las","ohx26lap","ohx26laa")],na.rm = T)
  NHANES_09_14$mean_cal_27 <- mean_row(NHANES_09_14[,c("ohx27lad","ohx27las","ohx27lap","ohx27laa")],na.rm = T)
  NHANES_09_14$mean_cal_28 <- mean_row(NHANES_09_14[,c("ohx28lad","ohx28las","ohx28lap","ohx28laa")],na.rm = T)
  NHANES_09_14$mean_cal_29 <- mean_row(NHANES_09_14[,c("ohx29lad","ohx29las","ohx29lap","ohx29laa")],na.rm = T)
  NHANES_09_14$mean_cal_30 <- mean_row(NHANES_09_14[,c("ohx30lad","ohx30las","ohx30lap","ohx30laa")],na.rm = T)
  NHANES_09_14$mean_cal_31 <- mean_row(NHANES_09_14[,c("ohx31lad","ohx31las","ohx31lap","ohx31laa")],na.rm = T)
}

NHANES_09_14 %>% select(ohx02lad,ohx02las,ohx02lap,ohx02laa,mean_cal_2) %>% slice(1:10)

View(NHANES_09_14 %>%  select(ohx02lad,ohx02las,ohx02lap,ohx02laa,mean_cal_2) %>% slice(1:10))
View(NHANES_09_14 %>%  select(ohx03lad,ohx03las,ohx03lap,ohx03laa,mean_cal_3) %>% slice(1:10))
View(NHANES_09_14 %>%  select(ohx04lad,ohx04las,ohx04lap,ohx04laa,mean_cal_4) %>% slice(1:10))

#Calculating mouth mean of mean interproximal CAL per tooth
grep("mean_cal_2", colnames(NHANES_09_14))
grep("mean_cal_31", colnames(NHANES_09_14))

NHANES_09_14$mean_mean_cal <- mean_row(NHANES_09_14[,769:796],na.rm=T)

#Checking if mean of mean cal was correctly coded
NHANES_09_14 %>% select(seqn, mean_mean_cal) %>% slice(1:10)

#Getting the mean of all interproximal sites in mouth
NHANES_09_14$mean_cal_mouth <- rowMeans(NHANES_09_14 %>% select(
      ohx02lad, ohx03lad, ohx04lad, ohx05lad, ohx06lad, ohx07lad, ohx08lad,
      ohx09lad, ohx10lad, ohx11lad, ohx12lad, ohx13lad, ohx14lad, ohx15lad,
      ohx18lad, ohx19lad, ohx20lad, ohx21lad, ohx22lad, ohx23lad, ohx24lad, 
      ohx25lad, ohx26lad, ohx27lad, ohx28lad, ohx29lad, ohx30lad, ohx31lad,
                                                                    
      ohx02las, ohx03las, ohx04las, ohx05las, ohx06las, ohx07las, ohx08las, 
      ohx09las, ohx10las, ohx11las, ohx12las, ohx13las, ohx14las, ohx15las, 
      ohx18las, ohx19las, ohx20las, ohx21las, ohx22las, ohx23las, ohx24las, 
      ohx25las, ohx26las, ohx27las, ohx28las, ohx29las, ohx30las, ohx31las,
                                                                    
      ohx02lap, ohx03lap, ohx04lap, ohx05lap, ohx06lap, ohx07lap, ohx08lap, 
      ohx09lap, ohx10lap, ohx11lap, ohx12lap, ohx13lap, ohx14lap, ohx15lap, 
      ohx18lap, ohx19lap, ohx20lap, ohx21lap, ohx22lap, ohx23lap, ohx24lap, 
      ohx25lap, ohx26lap, ohx27lap, ohx28lap, ohx29lap, ohx30lap, ohx31lap,
                                                                    
      ohx02laa, ohx03laa, ohx04laa, ohx05laa, ohx06laa, ohx07laa, ohx08laa, 
      ohx09laa, ohx10laa, ohx11laa, ohx12laa, ohx13laa, ohx14laa, ohx15laa, 
      ohx18laa, ohx19laa, ohx20laa, ohx21laa, ohx22laa, ohx23laa, ohx24laa, 
      ohx25laa, ohx26laa, ohx27laa, ohx28laa, ohx29laa, ohx30laa, ohx31laa), 
      na.rm = T)

#Getting the sum of all interproximal sites in mouth
NHANES_09_14$sum_cal_mouth <- rowSums(NHANES_09_14 %>% select(
  ohx02lad, ohx03lad, ohx04lad, ohx05lad, ohx06lad, ohx07lad, ohx08lad,
  ohx09lad, ohx10lad, ohx11lad, ohx12lad, ohx13lad, ohx14lad, ohx15lad,
  ohx18lad, ohx19lad, ohx20lad, ohx21lad, ohx22lad, ohx23lad, ohx24lad, 
  ohx25lad, ohx26lad, ohx27lad, ohx28lad, ohx29lad, ohx30lad, ohx31lad,
  
  ohx02las, ohx03las, ohx04las, ohx05las, ohx06las, ohx07las, ohx08las, 
  ohx09las, ohx10las, ohx11las, ohx12las, ohx13las, ohx14las, ohx15las, 
  ohx18las, ohx19las, ohx20las, ohx21las, ohx22las, ohx23las, ohx24las, 
  ohx25las, ohx26las, ohx27las, ohx28las, ohx29las, ohx30las, ohx31las,
  
  ohx02lap, ohx03lap, ohx04lap, ohx05lap, ohx06lap, ohx07lap, ohx08lap, 
  ohx09lap, ohx10lap, ohx11lap, ohx12lap, ohx13lap, ohx14lap, ohx15lap, 
  ohx18lap, ohx19lap, ohx20lap, ohx21lap, ohx22lap, ohx23lap, ohx24lap, 
  ohx25lap, ohx26lap, ohx27lap, ohx28lap, ohx29lap, ohx30lap, ohx31lap,
  
  ohx02laa, ohx03laa, ohx04laa, ohx05laa, ohx06laa, ohx07laa, ohx08laa, 
  ohx09laa, ohx10laa, ohx11laa, ohx12laa, ohx13laa, ohx14laa, ohx15laa, 
  ohx18laa, ohx19laa, ohx20laa, ohx21laa, ohx22laa, ohx23laa, ohx24laa, 
  ohx25laa, ohx26laa, ohx27laa, ohx28laa, ohx29laa, ohx30laa, ohx31laa), 
  na.rm = T)

#Getting the count of all interproximal sites in mouth
NHANES_09_14$count_cal_mouth <- count_row_if(not_na, NHANES_09_14 %>% select(
  ohx02lad, ohx03lad, ohx04lad, ohx05lad, ohx06lad, ohx07lad, ohx08lad,
  ohx09lad, ohx10lad, ohx11lad, ohx12lad, ohx13lad, ohx14lad, ohx15lad,
  ohx18lad, ohx19lad, ohx20lad, ohx21lad, ohx22lad, ohx23lad, ohx24lad, 
  ohx25lad, ohx26lad, ohx27lad, ohx28lad, ohx29lad, ohx30lad, ohx31lad,
  
  ohx02las, ohx03las, ohx04las, ohx05las, ohx06las, ohx07las, ohx08las, 
  ohx09las, ohx10las, ohx11las, ohx12las, ohx13las, ohx14las, ohx15las, 
  ohx18las, ohx19las, ohx20las, ohx21las, ohx22las, ohx23las, ohx24las, 
  ohx25las, ohx26las, ohx27las, ohx28las, ohx29las, ohx30las, ohx31las,
  
  ohx02lap, ohx03lap, ohx04lap, ohx05lap, ohx06lap, ohx07lap, ohx08lap, 
  ohx09lap, ohx10lap, ohx11lap, ohx12lap, ohx13lap, ohx14lap, ohx15lap, 
  ohx18lap, ohx19lap, ohx20lap, ohx21lap, ohx22lap, ohx23lap, ohx24lap, 
  ohx25lap, ohx26lap, ohx27lap, ohx28lap, ohx29lap, ohx30lap, ohx31lap,
  
  ohx02laa, ohx03laa, ohx04laa, ohx05laa, ohx06laa, ohx07laa, ohx08laa, 
  ohx09laa, ohx10laa, ohx11laa, ohx12laa, ohx13laa, ohx14laa, ohx15laa, 
  ohx18laa, ohx19laa, ohx20laa, ohx21laa, ohx22laa, ohx23laa, ohx24laa, 
  ohx25laa, ohx26laa, ohx27laa, ohx28laa, ohx29laa, ohx30laa, ohx31laa))

#Comparing mean of mean cal and mean of all interproximal sites
NHANES_09_14 %>% select(seqn, mean_mean_cal,mean_cal_mouth) %>% slice(1:10)

View(NHANES_09_14 %>% select(seqn, toothcount,
                                                ohx02lad, ohx03lad, ohx04lad, ohx05lad, ohx06lad, ohx07lad, ohx08lad,
                                                ohx09lad, ohx10lad, ohx11lad, ohx12lad, ohx13lad, ohx14lad, ohx15lad,
                                                ohx18lad, ohx19lad, ohx20lad, ohx21lad, ohx22lad, ohx23lad, ohx24lad, 
                                                ohx25lad, ohx26lad, ohx27lad, ohx28lad, ohx29lad, ohx30lad, ohx31lad,
                                                
                                                ohx02las, ohx03las, ohx04las, ohx05las, ohx06las, ohx07las, ohx08las, 
                                                ohx09las, ohx10las, ohx11las, ohx12las, ohx13las, ohx14las, ohx15las, 
                                                ohx18las, ohx19las, ohx20las, ohx21las, ohx22las, ohx23las, ohx24las, 
                                                ohx25las, ohx26las, ohx27las, ohx28las, ohx29las, ohx30las, ohx31las,
                                                
                                                ohx02lap, ohx03lap, ohx04lap, ohx05lap, ohx06lap, ohx07lap, ohx08lap, 
                                                ohx09lap, ohx10lap, ohx11lap, ohx12lap, ohx13lap, ohx14lap, ohx15lap, 
                                                ohx18lap, ohx19lap, ohx20lap, ohx21lap, ohx22lap, ohx23lap, ohx24lap, 
                                                ohx25lap, ohx26lap, ohx27lap, ohx28lap, ohx29lap, ohx30lap, ohx31lap,
                                                
                                                ohx02laa, ohx03laa, ohx04laa, ohx05laa, ohx06laa, ohx07laa, ohx08laa, 
                                                ohx09laa, ohx10laa, ohx11laa, ohx12laa, ohx13laa, ohx14laa, ohx15laa, 
                                                ohx18laa, ohx19laa, ohx20laa, ohx21laa, ohx22laa, ohx23laa, ohx24laa, 
                                                ohx25laa, ohx26laa, ohx27laa, ohx28laa, ohx29laa, ohx30laa, ohx31laa,
                                                mean_mean_cal,mean_cal_mouth,sum_cal_mouth, count_cal_mouth) %>% slice(1:10))

#Getting the mean interproximal PD per tooth
NHANES_09_14[,c("ohx02pcd","ohx02pcs","ohx02pcp","ohx02pca")]

{
  NHANES_09_14$mean_pd_2 <- mean_row(NHANES_09_14[,c("ohx02pcd","ohx02pcs","ohx02pcp","ohx02pca")],na.rm = T)
  NHANES_09_14$mean_pd_3 <- mean_row(NHANES_09_14[,c("ohx03pcd","ohx03pcs","ohx03pcp","ohx03pca")],na.rm = T)
  NHANES_09_14$mean_pd_4 <- mean_row(NHANES_09_14[,c("ohx04pcd","ohx04pcs","ohx04pcp","ohx04pca")],na.rm = T)
  NHANES_09_14$mean_pd_5 <- mean_row(NHANES_09_14[,c("ohx05pcd","ohx05pcs","ohx05pcp","ohx05pca")],na.rm = T)
  NHANES_09_14$mean_pd_6 <- mean_row(NHANES_09_14[,c("ohx06pcd","ohx06pcs","ohx06pcp","ohx06pca")],na.rm = T)
  NHANES_09_14$mean_pd_7 <- mean_row(NHANES_09_14[,c("ohx07pcd","ohx07pcs","ohx07pcp","ohx07pca")],na.rm = T)
  NHANES_09_14$mean_pd_8 <- mean_row(NHANES_09_14[,c("ohx08pcd","ohx08pcs","ohx08pcp","ohx08pca")],na.rm = T)
  NHANES_09_14$mean_pd_9 <- mean_row(NHANES_09_14[,c("ohx09pcd","ohx09pcs","ohx09pcp","ohx09pca")],na.rm = T)
  NHANES_09_14$mean_pd_10 <- mean_row(NHANES_09_14[,c("ohx10pcd","ohx10pcs","ohx10pcp","ohx10pca")],na.rm = T)
  NHANES_09_14$mean_pd_11 <- mean_row(NHANES_09_14[,c("ohx11pcd","ohx11pcs","ohx11pcp","ohx11pca")],na.rm = T)
  NHANES_09_14$mean_pd_12 <- mean_row(NHANES_09_14[,c("ohx12pcd","ohx12pcs","ohx12pcp","ohx12pca")],na.rm = T)
  NHANES_09_14$mean_pd_13 <- mean_row(NHANES_09_14[,c("ohx13pcd","ohx13pcs","ohx13pcp","ohx13pca")],na.rm = T)
  NHANES_09_14$mean_pd_14 <- mean_row(NHANES_09_14[,c("ohx14pcd","ohx14pcs","ohx14pcp","ohx14pca")],na.rm = T)
  NHANES_09_14$mean_pd_15 <- mean_row(NHANES_09_14[,c("ohx15pcd","ohx15pcs","ohx15pcp","ohx15pca")],na.rm = T)
  NHANES_09_14$mean_pd_18 <- mean_row(NHANES_09_14[,c("ohx18pcd","ohx18pcs","ohx18pcp","ohx18pca")],na.rm = T)
  NHANES_09_14$mean_pd_19 <- mean_row(NHANES_09_14[,c("ohx19pcd","ohx19pcs","ohx19pcp","ohx19pca")],na.rm = T)
  NHANES_09_14$mean_pd_20 <- mean_row(NHANES_09_14[,c("ohx20pcd","ohx20pcs","ohx20pcp","ohx20pca")],na.rm = T)
  NHANES_09_14$mean_pd_21 <- mean_row(NHANES_09_14[,c("ohx21pcd","ohx21pcs","ohx21pcp","ohx21pca")],na.rm = T)
  NHANES_09_14$mean_pd_22 <- mean_row(NHANES_09_14[,c("ohx22pcd","ohx22pcs","ohx22pcp","ohx22pca")],na.rm = T)
  NHANES_09_14$mean_pd_23 <- mean_row(NHANES_09_14[,c("ohx23pcd","ohx23pcs","ohx23pcp","ohx23pca")],na.rm = T)
  NHANES_09_14$mean_pd_24 <- mean_row(NHANES_09_14[,c("ohx24pcd","ohx24pcs","ohx24pcp","ohx24pca")],na.rm = T)
  NHANES_09_14$mean_pd_25 <- mean_row(NHANES_09_14[,c("ohx25pcd","ohx25pcs","ohx25pcp","ohx25pca")],na.rm = T)
  NHANES_09_14$mean_pd_26 <- mean_row(NHANES_09_14[,c("ohx26pcd","ohx26pcs","ohx26pcp","ohx26pca")],na.rm = T)
  NHANES_09_14$mean_pd_27 <- mean_row(NHANES_09_14[,c("ohx27pcd","ohx27pcs","ohx27pcp","ohx27pca")],na.rm = T)
  NHANES_09_14$mean_pd_28 <- mean_row(NHANES_09_14[,c("ohx28pcd","ohx28pcs","ohx28pcp","ohx28pca")],na.rm = T)
  NHANES_09_14$mean_pd_29 <- mean_row(NHANES_09_14[,c("ohx29pcd","ohx29pcs","ohx29pcp","ohx29pca")],na.rm = T)
  NHANES_09_14$mean_pd_30 <- mean_row(NHANES_09_14[,c("ohx30pcd","ohx30pcs","ohx30pcp","ohx30pca")],na.rm = T)
  NHANES_09_14$mean_pd_31 <- mean_row(NHANES_09_14[,c("ohx31pcd","ohx31pcs","ohx31pcp","ohx31pca")],na.rm = T)
}

NHANES_09_14 %>% select(ohx02pcd,ohx02pcs,ohx02pcp,ohx02pca,mean_pd_2) %>% slice(1:10)

#pdculating mouth mean of mean interproximal PD per tooth
grep("mean_pd_2", colnames(NHANES_09_14))
grep("mean_pd_31", colnames(NHANES_09_14))

NHANES_09_14$mean_mean_pd <- mean_row(NHANES_09_14[,797:824],na.rm=T)

#Checking if mean of mean pd was correctly coded
NHANES_09_14 %>% select(seqn, mean_mean_pd) %>% slice(1:10)

#Getting the mean of all interproximal sites in mouth
NHANES_09_14$mean_pd_mouth <- rowMeans(NHANES_09_14 %>% select(
  ohx02pcd, ohx03pcd, ohx04pcd, ohx05pcd, ohx06pcd, ohx07pcd, ohx08pcd,
  ohx09pcd, ohx10pcd, ohx11pcd, ohx12pcd, ohx13pcd, ohx14pcd, ohx15pcd,
  ohx18pcd, ohx19pcd, ohx20pcd, ohx21pcd, ohx22pcd, ohx23pcd, ohx24pcd, 
  ohx25pcd, ohx26pcd, ohx27pcd, ohx28pcd, ohx29pcd, ohx30pcd, ohx31pcd,
  
  ohx02pcs, ohx03pcs, ohx04pcs, ohx05pcs, ohx06pcs, ohx07pcs, ohx08pcs, 
  ohx09pcs, ohx10pcs, ohx11pcs, ohx12pcs, ohx13pcs, ohx14pcs, ohx15pcs, 
  ohx18pcs, ohx19pcs, ohx20pcs, ohx21pcs, ohx22pcs, ohx23pcs, ohx24pcs, 
  ohx25pcs, ohx26pcs, ohx27pcs, ohx28pcs, ohx29pcs, ohx30pcs, ohx31pcs,
  
  ohx02pcp, ohx03pcp, ohx04pcp, ohx05pcp, ohx06pcp, ohx07pcp, ohx08pcp, 
  ohx09pcp, ohx10pcp, ohx11pcp, ohx12pcp, ohx13pcp, ohx14pcp, ohx15pcp, 
  ohx18pcp, ohx19pcp, ohx20pcp, ohx21pcp, ohx22pcp, ohx23pcp, ohx24pcp, 
  ohx25pcp, ohx26pcp, ohx27pcp, ohx28pcp, ohx29pcp, ohx30pcp, ohx31pcp,
  
  ohx02pca, ohx03pca, ohx04pca, ohx05pca, ohx06pca, ohx07pca, ohx08pca, 
  ohx09pca, ohx10pca, ohx11pca, ohx12pca, ohx13pca, ohx14pca, ohx15pca, 
  ohx18pca, ohx19pca, ohx20pca, ohx21pca, ohx22pca, ohx23pca, ohx24pca, 
  ohx25pca, ohx26pca, ohx27pca, ohx28pca, ohx29pca, ohx30pca, ohx31pca), 
  na.rm = T)

#Getting the sum of all interproximal sites in mouth
NHANES_09_14$sum_pd_mouth <- rowSums(NHANES_09_14 %>% select(
  ohx02pcd, ohx03pcd, ohx04pcd, ohx05pcd, ohx06pcd, ohx07pcd, ohx08pcd,
  ohx09pcd, ohx10pcd, ohx11pcd, ohx12pcd, ohx13pcd, ohx14pcd, ohx15pcd,
  ohx18pcd, ohx19pcd, ohx20pcd, ohx21pcd, ohx22pcd, ohx23pcd, ohx24pcd, 
  ohx25pcd, ohx26pcd, ohx27pcd, ohx28pcd, ohx29pcd, ohx30pcd, ohx31pcd,
  
  ohx02pcs, ohx03pcs, ohx04pcs, ohx05pcs, ohx06pcs, ohx07pcs, ohx08pcs, 
  ohx09pcs, ohx10pcs, ohx11pcs, ohx12pcs, ohx13pcs, ohx14pcs, ohx15pcs, 
  ohx18pcs, ohx19pcs, ohx20pcs, ohx21pcs, ohx22pcs, ohx23pcs, ohx24pcs, 
  ohx25pcs, ohx26pcs, ohx27pcs, ohx28pcs, ohx29pcs, ohx30pcs, ohx31pcs,
  
  ohx02pcp, ohx03pcp, ohx04pcp, ohx05pcp, ohx06pcp, ohx07pcp, ohx08pcp, 
  ohx09pcp, ohx10pcp, ohx11pcp, ohx12pcp, ohx13pcp, ohx14pcp, ohx15pcp, 
  ohx18pcp, ohx19pcp, ohx20pcp, ohx21pcp, ohx22pcp, ohx23pcp, ohx24pcp, 
  ohx25pcp, ohx26pcp, ohx27pcp, ohx28pcp, ohx29pcp, ohx30pcp, ohx31pcp,
  
  ohx02pca, ohx03pca, ohx04pca, ohx05pca, ohx06pca, ohx07pca, ohx08pca, 
  ohx09pca, ohx10pca, ohx11pca, ohx12pca, ohx13pca, ohx14pca, ohx15pca, 
  ohx18pca, ohx19pca, ohx20pca, ohx21pca, ohx22pca, ohx23pca, ohx24pca, 
  ohx25pca, ohx26pca, ohx27pca, ohx28pca, ohx29pca, ohx30pca, ohx31pca), 
  na.rm = T)

#Getting the count of all interproximal sites in mouth
NHANES_09_14$count_pd_mouth <- count_row_if(not_na, NHANES_09_14 %>% select(
  ohx02pcd, ohx03pcd, ohx04pcd, ohx05pcd, ohx06pcd, ohx07pcd, ohx08pcd,
  ohx09pcd, ohx10pcd, ohx11pcd, ohx12pcd, ohx13pcd, ohx14pcd, ohx15pcd,
  ohx18pcd, ohx19pcd, ohx20pcd, ohx21pcd, ohx22pcd, ohx23pcd, ohx24pcd, 
  ohx25pcd, ohx26pcd, ohx27pcd, ohx28pcd, ohx29pcd, ohx30pcd, ohx31pcd,
  
  ohx02pcs, ohx03pcs, ohx04pcs, ohx05pcs, ohx06pcs, ohx07pcs, ohx08pcs, 
  ohx09pcs, ohx10pcs, ohx11pcs, ohx12pcs, ohx13pcs, ohx14pcs, ohx15pcs, 
  ohx18pcs, ohx19pcs, ohx20pcs, ohx21pcs, ohx22pcs, ohx23pcs, ohx24pcs, 
  ohx25pcs, ohx26pcs, ohx27pcs, ohx28pcs, ohx29pcs, ohx30pcs, ohx31pcs,
  
  ohx02pcp, ohx03pcp, ohx04pcp, ohx05pcp, ohx06pcp, ohx07pcp, ohx08pcp, 
  ohx09pcp, ohx10pcp, ohx11pcp, ohx12pcp, ohx13pcp, ohx14pcp, ohx15pcp, 
  ohx18pcp, ohx19pcp, ohx20pcp, ohx21pcp, ohx22pcp, ohx23pcp, ohx24pcp, 
  ohx25pcp, ohx26pcp, ohx27pcp, ohx28pcp, ohx29pcp, ohx30pcp, ohx31pcp,
  
  ohx02pca, ohx03pca, ohx04pca, ohx05pca, ohx06pca, ohx07pca, ohx08pca, 
  ohx09pca, ohx10pca, ohx11pca, ohx12pca, ohx13pca, ohx14pca, ohx15pca, 
  ohx18pca, ohx19pca, ohx20pca, ohx21pca, ohx22pca, ohx23pca, ohx24pca, 
  ohx25pca, ohx26pca, ohx27pca, ohx28pca, ohx29pca, ohx30pca, ohx31pca))

#Comparing mean of mean pd and mean of all interproximal sites
NHANES_09_14 %>% select(seqn, mean_mean_pd,mean_pd_mouth) %>% slice(1:10)

View(NHANES_09_14 %>% select(seqn, toothcount,
                             ohx02pcd, ohx03pcd, ohx04pcd, ohx05pcd, ohx06pcd, ohx07pcd, ohx08pcd,
                             ohx09pcd, ohx10pcd, ohx11pcd, ohx12pcd, ohx13pcd, ohx14pcd, ohx15pcd,
                             ohx18pcd, ohx19pcd, ohx20pcd, ohx21pcd, ohx22pcd, ohx23pcd, ohx24pcd, 
                             ohx25pcd, ohx26pcd, ohx27pcd, ohx28pcd, ohx29pcd, ohx30pcd, ohx31pcd,
                             
                             ohx02pcs, ohx03pcs, ohx04pcs, ohx05pcs, ohx06pcs, ohx07pcs, ohx08pcs, 
                             ohx09pcs, ohx10pcs, ohx11pcs, ohx12pcs, ohx13pcs, ohx14pcs, ohx15pcs, 
                             ohx18pcs, ohx19pcs, ohx20pcs, ohx21pcs, ohx22pcs, ohx23pcs, ohx24pcs, 
                             ohx25pcs, ohx26pcs, ohx27pcs, ohx28pcs, ohx29pcs, ohx30pcs, ohx31pcs,
                             
                             ohx02pcp, ohx03pcp, ohx04pcp, ohx05pcp, ohx06pcp, ohx07pcp, ohx08pcp, 
                             ohx09pcp, ohx10pcp, ohx11pcp, ohx12pcp, ohx13pcp, ohx14pcp, ohx15pcp, 
                             ohx18pcp, ohx19pcp, ohx20pcp, ohx21pcp, ohx22pcp, ohx23pcp, ohx24pcp, 
                             ohx25pcp, ohx26pcp, ohx27pcp, ohx28pcp, ohx29pcp, ohx30pcp, ohx31pcp,
                             
                             ohx02pca, ohx03pca, ohx04pca, ohx05pca, ohx06pca, ohx07pca, ohx08pca, 
                             ohx09pca, ohx10pca, ohx11pca, ohx12pca, ohx13pca, ohx14pca, ohx15pca, 
                             ohx18pca, ohx19pca, ohx20pca, ohx21pca, ohx22pca, ohx23pca, ohx24pca, 
                             ohx25pca, ohx26pca, ohx27pca, ohx28pca, ohx29pca, ohx30pca, ohx31pca,
                             mean_mean_pd,mean_pd_mouth,sum_pd_mouth, count_pd_mouth) %>% slice(1:10))

#Rename age
NHANES_09_14 <- NHANES_09_14 %>% rename(age=ridageyr)
NHANES_09_14 <- NHANES_09_14 %>% rename(gender=riagendr)

#Descriptive statistics
#Defining a function for calculating standard error
se <- function(x, na.rm=FALSE) {
  if (na.rm) x <- na.omit(x)
  sqrt(var(x)/length(x))
}

#Defining a function for calculating mean while ignoring missing values
mean.ignoreNA <- function(vec) {
  return(mean(vec, na.rm=TRUE))
}
mean.ignoreNA(data1$AGE)

#Getting means of age and toothcount
View(NHANES_09_14 %>% summarise(across(.col=c(age, toothcount), .fns=list("mean"=mean,"sd"=sd,"SEM"=se,"median"=median,"IQR"=IQR), na.rm=T)))

#Summary using rstatix package
NHANES_09_14 %>% 
  get_summary_stats(
    age, toothcount,  # columns to calculate for
    type = "common")                    # summary stats to return

#Summary using summarytools package
summarytools::view(dfSummary(NHANES_09_14))

#Summary using gtsummary package
vtable(NHANES_09_14)
NHANES_09_14 %>% select(gender,periodontitis) %>% tbl_summary()

#Using skimr package
skim(NHANES_09_14)

#Getting frequencies of specific variables
NHANES_09_14 %>% tabyl(c(NHANES_09_14[,c("gender","age")]))

#Reprodcuing Table 1. Tooth level absolute comparisons of clinical measurement sites (mm) by
#tooth type and disease status*/ 










NHANES_09_14$toothcount <- apply(NHANES_09_14[,674:701],1,sum(case_when(NHANES_09_14[,674:701]>=6)), na.rm=T)

NHANES_09_14 %>% NHANES_09_14[,674:701]case_when

NHANES_09_14$toothcount <- apply(NHANES_09_14[,674:701],1,sum, na.rm=T)

NHANES_09_14$tooth_cal_ge_6 <- apply(NHANES_09_14[,703:730],1,sum_if(6,~), data=NULL)
NHANES_09_14$tooth_cal_ge_6 <- apply(NHANES_09_14[,703:730],1,sum, na.rm=T)

NHANES_09_14$tooth_cal_ge_6 <- apply(NHANES_09_14[,c("max_cal_2":"max_cal_31")],1,sum_if(ge(6)), na.rm=T)



sum_if(6, NHANES_09_14[,703:730])
sum_col_if(6, NHANES_09_14[,703:730])

View(NHANES_09_14[,703:730])



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


teeth$sci <- ifelsejdgmh



difft <- ((NHANES_09_14$ohx02lap)-(NHANES_09_14$ohx02las))+()/

  NHANES_09_14 %>%
  gather(key = tooth, value = ntooth, starts_with("dvar"))

