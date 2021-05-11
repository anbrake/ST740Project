
comarea_init <- read.csv("ComArea_ACS14_f.csv")

##### Check the variables to make sure they're accurate/have nothing missing, 
##### by going to the original data sets they're taken from 

# Checking variables derived from the ACS 2014

# Pop2012 and Pop2014 look good, according to Reference_CCAProfiles_2010_2014.csv in the
# original_data_sets folder. 
# I found the above csv at (https://datahub.cmap.illinois.gov/dataset/community-data-snapshots-raw-data)
# and clicking on Archive: June 2016 CDS Raw Tables. 
# But for the other ACS 2014 variables, I can't seem to find
# them in Reference_CCAProfiles_2010_2014.csv. 

# Since two variables are verified, I'll go ahead and trust all the other variables derived from 
# ACS 2014.



# Checking variables derived from the City of Chicago Data Portal

# The original data set is 
# Public_Health_Statistics-_Selected_public_health_indicators_by_Chicago_community_area.csv
# from (https://data.cityofchicago.org/Health-Human-Services/Public-Health-Statistics-Selected-public-health-in/iqnk-2tcu)

# It looks like many variables are rounded. Let's re-merge all the variables by community area, just in case.

orig_health_indic <- read.csv("original_data_sets/Public_Health_Statistics-_Selected_public_health_indicators_by_Chicago_community_area.csv")




# delete the health indicator variables from comarea

comarea_merge <- comarea_init[, -c(66:86)]

# merge the original health indicator variables into comarea

names(comarea_merge)[1] <- "ComAreaID"

names(orig_health_indic)[1] <- "ComAreaID"

comarea_merge <- merge(comarea_merge, orig_health_indic)



# there are extra variables that comarea_init didn't have. Let's remove the variables
# that seem redundant with the ACS 2014 ones.
# I keep "Crowded.Housing" and "Dependency", however.

comarea <- comarea_merge[, -which(names(comarea_merge) %in% 
                                    c("Community.Area.Name" , "Below.Poverty.Level",
                                      "No.High.School.Diploma", "Per.Capita.Income", "Unemployment"))]





