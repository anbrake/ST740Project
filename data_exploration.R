
# We should stick with proportions, rates, etc. to make sure variables are 
# standardized across the community areas



# Verifying the "rate" in violent crime rate, property crime rate, etc., is
# just the count / population

cbind(comarea$community.C.254, comarea$Pop2014.N.18.0, comarea$Violent_C.N.18.0, 
      comarea$VlntCrRt.N.18.4, comarea$Violent_C.N.18.0 / comarea$Pop2014.N.18.0)

# Weird variables 
#' "PopChng.N.18.4" 
#' "field_37.N.18.4" 

