###################################################################################################
# Statistical analysis of Reminiscens Sessions and Participants - Data preparations               #
# Author: Cristhian Parra                                                                         #
###################################################################################################
# Auxiliary Packages to use
library(ggplot2)
library(manipulate)
library(plyr)
library(xtable)
library(reporttools)
library(reshape)

# Personal and External Function sources
source("scripts/reminiscens.tests.utils.R")
source("external/friedman.test.with.pohs.hoc.R") # Friedman test as implemented by [1] 

# Some globals
datadir <- "data/"
sessfname <- "sessions_dataset_1.csv"
partfname <- "participants_dataset_1.csv"
sessfile <- paste(datadir,sessfname,sep="")
partfile <- paste(datadir,partfname,sep="")
figdir <- "plots/"

# Load Reminiscens sessions dataset
print("Reading and preparing the data...")
sessfulltable <- read.csv(sessfile,header=T,sep=";")  
partfulltable <- read.csv(partfile,header=T,sep=";")  
attach(sessfulltable)
attach(partfulltable)

# Filter out sessions
sesspilot <- sessfulltable[done==1&group==0,]   # Pilot study sessions
sess <- sessfulltable[done==1&group!=0,]    	# Only done sessions
sessstabl <- sess[sess$group==1,]				# Stable group sessions
sess1time <- sess[sess$group==2,]				# 1-Time group sessions
sess1week <- sess[sess$week==1,]    			# First week sessions
sessfamily <- sess[sess$family==1,]				# Sessions in Family
sessnofamily <- sess[sess$family==0,]			# Sessions not in Family
sessnofamilystabl <- 
    sessstabl[sessstabl$family==0,]    		    # Sessions not in Family

# Filter out participants
partpilot = 
    partfulltable[partfulltable$type==0,]		# Pilot study participants
listpilot = 
    partpilot[partpilot$type==0
              &partpilot$islistener==1,]      	# Pilot group listeners
narrpilot = 
    partpilot[partpilot$type==0
              &partpilot$islistener==0,]  		# Pilot group narrators

part = partfulltable[partfulltable$type!=0,]	# All the participants
partstabl = part[part$type==1,]				    # Stable group participants
part1time = part[part$type==2,]					# 1-Time group participants
list = part[part$islistener==1,]				# Listeners
narr = part[part$islistener==0,]				# Narrators
liststabl = list[list$type==1,]				    # Stable group listeners
list1time = list[list$type==2,]					# 1-Time group listeners
narrstabl = narr[narr$type==1,]				    # Stable group narrators
narr1time = narr[narr$type==2,]					# 1-Time group narrators

appvnames <- c("Personal","Questions only", "Context only", "All included")

########################################################################################
# Auxiliary Data Preparations techniques, looking for unbalance in data
subset_columns <- c("week", "nid", "stat_stories_from_log", "appversion", "group")
vcolumn <- "stat_stories_from_log"
gcolumn <- "appversion"
pcolumn <- "nid"

# Looking at first 4 weeks first
df <- sess[,subset_columns]
str(df)
table(df$week)

# --> Take first only the first 4 weeks
df <- df[df$week<5 & df$group==1,]
str(df)

# --> Check balance in a pivot table of groups versus participants
cast(df,nid ~ appversion, value="stat_stories_from_log")
# nid 1 2 3 4
# 1  69 1 1 1 1
# 2  70 1 1 1 1
# 3  73 1 1 1 1
# 4  75 1 1 1 1
# 5  77 0 1 2 1  ==> Imputation (delete or simulate value)
# 6  78 1 1 1 1
# 7  79 1 1 1 1
# 8  83 1 1 1 1
# 9  98 1 1 1 1

# --> Deal with unbalanced data
# --> * Taking out nid=77 becase it didn't try appversion 1 in first 4 week
cast(df,nid ~ appversion, value="stat_stories_from_log",subset=nid!=77)     # automatically, pivot table holds sums now

# --> Filtered data for the FIRST 4 WEEKS OF THE STABLE


df <- sess[,subset_columns]
str(df)
table(df$week)

# --> Take only the last 4 weeks
df <- df[df$week>4 & df$group==1,]
str(df)

# --> Check balance in a pivot table of groups versus participants
cast(df,nid ~ appversion, value=vcolumn)
# --> Deal with unbalanced data
# --> * Taking out nid=75, 77, 79, 83, 98 nid becase it didn't try appversion 1 in first 4 week
# nid 1 2 3 4
# 1  69 1 1 1 1
# 2  70 1 1 1 1
# 3  73 1 1 1 1
# 4  75 2 0 0 2  ==> Imputation (delete or simulate value)
# 5  77 1 0 1 2  ==> Imputation (delete or simulate value)
# 6  78 1 1 1 1
# 7  79 1 1 0 0  ==> Imputation (delete or simulate value)
# 8  83 1 0 0 1  ==> Imputation (delete or simulate value)
# 9  98 0 0 1 1  ==> Imputation (delete or simulate value)

cast(df,nid ~ appversion, value=vcolumn,subset=nid!=77 & nid!=75 & nid!=79 & nid!=83 & nid!=98 )     # automatically, pivot table holds sums now

# Look at only first sessions
df <- sess[,subset_columns]
str(df)
table(df$week)

df <- df[df$week==1,]
str(df)

# --> Check balance in a pivot table of groups versus participants
# Is a between subjects
cast(df,nid ~ appversion, value=vcolumn)
# nid  1  2  3  4
# 1   69 NA NA NA  3
# 2   70 NA NA  2 NA
# 3   73 NA  3 NA NA
# 4   75  0 NA NA NA
# 5   77 NA NA NA  2
# 6   78 NA NA  2 NA
# 7   79 NA  3 NA NA
# 8   83  8 NA NA NA
# 9   96 NA NA NA  3
# 10  98 NA NA NA  2
# 11 100 NA NA  3 NA
# 12 103 NA  3 NA NA
# 13 104  5 NA NA NA
# 14 105 NA NA NA  1
# 15 106 NA NA  4 NA


# Look at mood against weeks
subset_columns <- c("week", "nid", "mood_after_narrator", "mood_after_narrator2", "mood_after_listener", "group")
vcolumn <- "mood_after_narrator"
gcolumn <- "week"
pcolumn <- "nid"

str(sess)

df <- sess[,subset_columns]
str(df)
table(df$week)

df$mood_after_narrator

df <- df[df$mood_narrator_after!=NA,]
str(df)

# --> Check balance in a pivot table of groups versus participants
# Is a between subjects
cast(df,nid ~ week, value=vcolumn)
#nid  1  2  3  4  5  6  7  8
# 1   69 34* 35 38 36 31 21 38 39    * 1 ==> 34 in first week
# 2   70 38 33 38 34 38 40 40 37*    * 8
# 3   *73 38 NA 40 NA NA NA 38 NA    * 2, 4, 5, 6, 8 ***
# 4   75 32 34 35 35 35 29 40 36
# 5   77 39 35 37 36 29 39 35 36
# 6   78 40 39 35 40 40 40 40 40
# 7   79 32 36 39 34 38 39 36* 36*    * 6, 8
# 8   83 34 35 37 34 38 37 36* 36*    * 6, 8
# 9   96 NA NA NA NA NA NA NA NA
# 10  98 29 31 NA NA 29 36 NA NA
# 11 100 35 NA NA NA NA NA NA NA
# 12 103 30 NA NA NA NA NA NA NA
# 13 104 40 NA NA NA NA NA NA NA
# 14 105 40 NA NA NA NA NA NA NA
# 15 106 33 NA NA NA NA NA NA NA


# Look at weeks against qualitative scores
subset_columns <- c("week", "nid", "group", 
                    "p_score", 
                    "s_max_score","sg_score", "se_score", "set_score",
                    "st_score", "sr_score",
                    "pc_score", "pnc_score",
                    "cc_score", "cnc_score",
                    "c_max_score",
                    "appversion")
vcolumn <- "se_score"
gcolumn <- "week"
pcolumn <- "nid"

str(sess)

df <- sess[,subset_columns]
str(df)
table(df$week)

# --> Check balance in a pivot table of groups versus participants
# Is a between subjects
cast(df,nid ~ week, value=vcolumn)
# 
# nid  1  2  3  4  5  6  7  8
# 1   69  0  1  2  1  3  3  2  1
# 2   70  3  2  3  3  3  3  3  3
# 3   73  2  3  0  1  2  3  2  1
# 4   75  2  0  1  2  2  0  3  0
# 5   77  2  3  1  2  1  2  1  3
# 6   78  2  2  1  1  2  3  2  1
# 7   79  2  3  2  2  2  2 NA NA
# 8   83  3  2  2  3  1  3 NA NA
