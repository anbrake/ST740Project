####ST740 Project####

# Aaron's data cleaning code to get comareaCleaned.RData



# comarea_init = read.csv("~/Documents/PublicHealthCh.csv")
# orig_health_indic = read.csv("~/Documents/ComArea_ACS14_f.csv")
# names(orig_health_indic) = clean_names
# 
# comarea_merge <- comarea_init[, -c(66:86)]
# names(comarea_merge)[1] <- "ComAreaID"
# names(orig_health_indic)[1] <- "ComAreaID"
# comarea_merge <- merge(comarea_merge, orig_health_indic)
# comarea <- comarea_merge[, -which(names(comarea_merge) %in% 
#                                     c("Community.Area.Name" , "Below.Poverty.Level", "No.High.School.Diploma", "Per.Capita.Income", "Unemployment"))]
# 
# #Drop useless/repeated Columns
# #Rename the Childhood lead poisoning column 
# comarea$ChlLeadP = comarea$Childhood.Lead.Poisoning
# 
# 
# #Get the column numbers of the columns to keep
# keepCols = c(1:8,13,14,23:25,31,33,37,39,41,43,45, 47,49, 51,53,55,57,
#              59,61,63,68:74,76,78,80,82,84:88, 106)
# #Make the dataframe with the correct columns
# comareaCleaned = cbind(comarea[,keepCols],CentroidLat, CentroidLon)
# #Make a column with percents of Childhood lead
# comareaCleaned$ChlLeadPerc = comareaCleaned$ChlLeadP / 100
# #Make the column of childhood lead counts (rounded to nearest whole)
# comareaCleaned$ChlLeadPois = round(comareaCleaned$ChlLeadPerc * comarea$Pop2014)





######################### RUN FROM HERE #####

library(tidyverse)

### continuation of data cleaning

load("comareaCleaned.Rdata")



# Under18P Over18P add up to 100
# So make separate proportion categories for age

comareaCleaned$Bw5and18P <- comareaCleaned$Under18P - comareaCleaned$Under5P

comareaCleaned$Bw18and21P <- comareaCleaned$Over18P - comareaCleaned$Over21P 

comareaCleaned$Bw21and65P <- comareaCleaned$Over21P - comareaCleaned$Over65P

# the rest of the people should be over 65. I just keep Under5P among the original variables.



# add in Dist2Loop

load("spatial_vars.RData")

comareaCleaned$Dist2Loop <- Dist2Loop



# put PerCInc14 and Pov14 (as proportion) back in

comarea_init <- read.csv("ComArea_ACS14_f.csv")
comarea_init <- comarea_init[order(comarea_init$ComAreaID.N.18.0),]

comareaCleaned$PerCInc14 <- comarea_init$PerCInc14.N.18.0

# making Pov14P a percentage
comareaCleaned$Pov14P <- comarea_init$Pov14.N.18.0 / comarea_init$Pop2014.N.18.0 * 100



# I was thinking to make variables indicating ranges like for age (e.g. Bw18and21P)
# for poverty (Pov50P to Pov200P). However, I'll just leave poverty variables as it is.
# Shouldn't make a difference.



# Make ChldPov to Unemp into percentages

vars_to_divide <- c("ChldPov14", "NoHS14", "HSGrad14", "SmClg14", "ClgGrad14",
                    "LaborFrc", "Unemp14")

comareaCleaned[, which(names(comareaCleaned) %in% vars_to_divide)] <- 
  comareaCleaned[, which(names(comareaCleaned) %in% vars_to_divide)] / comarea_init$Pop2014.N.18.0 * 100

# names(comareaCleaned[, which(names(comareaCleaned) %in% vars_to_divide)]) <- 
#   c("ChldPov14P", "NoHS14P", "HSGrad14P", "SmClg14P", "ClgGrad14P",
#     "LaborFrcP", "Unemp14P")

comareaCleaned <- rename(comareaCleaned, ChldPov14P = ChldPov14, NoHS14P = NoHS14,
       HSGrad14P = HSGrad14, SmClg14P = SmClg14, ClgGrad14P = ClgGrad14,
       LaborFrcP = LaborFrc, Unemp14P = Unemp14)



# rename Perc to Dec for childhood blood levels

comareaCleaned <- rename(comareaCleaned, ChlLeadDec = ChlLeadPerc)



# make sure to exclude ComAreaID and community, 
# as well as Under18P, Over18P, Over21P, and Over65P. Just keep Under5P among the ages
# drop ChlLeadP as well

drop_col_names <- c("ComAreaID", "community", "PopChng", "Under18P",
                    "Over18P", "Over21P", "Over65P", "ChlLeadP")

comareaCleaned <- comareaCleaned[, !(names(comareaCleaned) %in% drop_col_names)]



# reorder column names

# desired order

# for ease of copy/paste
paste(names(comareaCleaned), collapse = "', '")

colname_order <- c('Birth.Rate', 'General.Fertility.Rate', 'Low.Birth.Weight',
                   'Prenatal.Care.Beginning.in.First.Trimester', 'Preterm.Births',
                   'Teen.Birth.Rate', 'Assault..Homicide.', 'Firearm.related',
                   'Infant.Mortality.Rate', 'Crowded.Housing', 'Dependency',
                   'PopMP', 'Under5P', 'Bw5and18P', 'Bw18and21P', 'Bw21and65P',
                   'Wht14P', 'Blk14P', 'AI14P', 'AS14P',
                   'NHP14P', 'Oth14P', 'Hisp14P', 'PropCrRt', 'VlntCrRt',
                   'ChldPov14P', 'NoHS14P', 'HSGrad14P', 'SmClg14P', 'ClgGrad14P',
                   'LaborFrcP', 'Unemp14P', 'Pov14P', 'Pov50P', 'Pov125P', 'Pov150P',
                   'Pov185P', 'Pov200P', 'COIave', 'HISave', 'SESave',
                   'Hlitave', 'CentroidLat', 'CentroidLon', 'Dist2Loop',
                   'PerCInc14', 'ChlLeadDec', 'ChlLeadPois')

comareaCleaned <- comareaCleaned[colname_order]

# save it as comarea, instead of comareaCleaned

comarea <- comareaCleaned

save(comarea, file = "comarea.RData")



com_popl <- comarea_init$Pop2014.N.18.0

save(com_popl, file = "com_popl.R")
