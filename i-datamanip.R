#Manipulate data 
#Currently does this one province at a time

#Import data using the naturecounts R package. 

#first look in file. If it does not exist, download from database.
data <-try(read.csv(paste(dat.dir, ".RawData.csv", sep = "")), silent = T)

if(class(data) == 'try-error'){
  
  data <- nc_data_dl(collections = collection, fields_set="extended", username =ID, info ="Ethier data download NOS data for national analysis")
  write.csv(data, paste(dat.dir, ".RawData.csv", sep = ""), row.names = FALSE)
 
} #end else

#Now that the data are downloaded we will want to select the columns needed for the analysis. You may not have all the variables below if you didn't change the `field_set` to "extend". That is OK! You may not need all the auxiliary data for your analytical purposes.
in.data<-data %>% filter (protocol_id != 29) #remove old MB data collected under an old protcol

in.data<-in.data %>% select(SamplingEventIdentifier, SurveyAreaIdentifier,RouteIdentifier, Locality, SiteCode, collection, survey_day, survey_month, survey_year, survey_week, protocol_id, CollectorNumber, EffortUnits1, EffortMeasurement1, EffortUnits3, EffortMeasurement3, EffortUnits5, EffortMeasurement5, EffortUnits11, EffortMeasurement11, EffortUnits14, EffortMeasurement14, species_id, CommonName, ScientificName, latitude, longitude, bcr, StateProvince, ObservationDescriptor, ObservationCount)

#Notice here that we don't keep all the ObservationCount fields. There are more that could be retained that capture owls call during each broadcast period.
#For the purposes of this analysis, we keep ObservationCount2 + ObservationCount3 = Number of owls detected before call playback is used (i.e., silent listening period only). This is nationally standardized. Some protocols do not have call playback, and we can therefore use ObservationCount (totals) for the analysis. 
#If you want to keep all counts, inclusive of the silent listening + call playback, keep ObservationCount.  

#Filter out the data we know we don't want before proceeding.

#Drop Newfoundland & Labrador based on StateProvince 
in.data<-in.data %>% filter (StateProvince!="Newfoundland") %>% droplevels()
#remove the new WESO survey in BC
in.data<-in.data %>% filter (protocol_id != 63)

#Drop Northwest Terriroties based on route identifier, survey started in 2018, and therefore not enough data yet for a national assessment
in.data<-filter(in.data, !grepl("NT", RouteIdentifier)) %>%  droplevels()

#Remove surveys with missing Month, Day, Year
in.data<-in.data %>% filter (!is.na(survey_day), !is.na(survey_month), !is.na(survey_year))

#Some Ontario Routes only have lat/long for the start point, so this can't be used for the national assessment. 
#Remove surveys with missing lat long
#in.data<-in.data %>% filter (!is.na(latitude), !is.na(longitude))
in.data<-in.data %>% filter(latitude != "NULL", longitude != "NULL")

#Remove survyes with missing protocol ID 
in.data<-in.data %>% filter (!is.na(protocol_id))

#remove site who naming cause issue on re-import into R 
in.data<-in.data %>% filter(!(RouteIdentifier %in% c("50N/Gaudry Road", "50N/Gaudry Rd, St. L" )))

#You may want to fix some data inconsistencies in StateProvince naming. However, you may want to use `collection` or `ProtocolCode` rather than `StateProvince` to do the analysis since there are some points that cross the provincial boundaries. Something to consider. 

in.data$StateProvince[in.data$StateProvince  == "Ontario"]  <-  "ON"
in.data$StateProvince[in.data$StateProvince  == "British Columbia and Yukon"]  <-  "BCY"
in.data$StateProvince[in.data$StateProvince  == "ME"]  <- "QC"
in.data$StateProvince[in.data$StateProvince  == "Newfoundland"]  <- "NL"
in.data$StateProvince[in.data$StateProvince  == "Nova Scotia"]  <- "NS"
in.data$StateProvince[in.data$StateProvince  == "Manitoba"]  <- "MB"
in.data$StateProvince[in.data$StateProvince  == "MN"]  <- "MB"
in.data$StateProvince[in.data$StateProvince  == "Alberta"]  <- "AB"
in.data$StateProvince[in.data$StateProvince  == "British Columbia"]  <- "BC"
in.data<- in.data %>% filter(StateProvince != "NL")


#Next we add a day-of-year column using the `format_dates` [helper function](https://birdstudiescanada.github.io/naturecounts/reference/format_dates.html).
#create a doy variable with the `format_dates` NatureCounts function. 
in.data<-format_dates(in.data)

#Filter based on effort measurement, where applicable

#Remove repeats 
in.data <- in.data %>%
  filter((EffortMeasurement14 != "Y" & EffortMeasurement14 != 1) | is.na(EffortMeasurement14))

dat<-in.data
#Provincial specific cleaning

#QC 
dat$StateProvince[dat$protocol_id == "35" & dat$StateProvince == "ON"] <- "QC"

#ON
dat$StateProvince[dat$protocol_id == "22" & dat$StateProvince == "QC"] <- "ON"
dat$StateProvince[dat$protocol_id == "36" & dat$StateProvince == "QC"] <- "ON"

#BC 
dat<-dat %>% filter(survey_month<6) #remove FLAM surveys

#AB
# Create a logical index for rows where Collection is "ABOWLS"
is_abowls <- dat$collection == "ABOWLS"
# For those specific rows, perform the reassignments
dat$RouteIdentifier[is_abowls] <- dat$SiteCode[is_abowls]
dat$SiteCode[is_abowls] <- dat$SurveyAreaIdentifier[is_abowls]


# Create a data frame with your protocol-specific minimum years, temperature
# Removed that were done in inappropriate environmental conditions 

min_map <- data.frame(
  protocol_id = c(37, 39, 40, 150, 152, 44, 27, 22, 36, 35, 100, 157, 151),
  min_year = c(2001, 2002, 2001, 2000, 2000, 2000, 2000, 1995, 1995, 2008, 2000, 2002, 2002), 
  min_temp = c(-15, -15, -15, -15, -15, -10, -20, -20, -20, -15, -20, -20, -20)
)

dat <- dat %>%
  left_join(min_map, by = "protocol_id") %>%
  filter(survey_year >= min_year) %>%
  filter(EffortMeasurement3>= min_temp | is.na(EffortMeasurement3)) %>% 
  filter(EffortMeasurement1 <= 3 | is.na(EffortMeasurement1)) %>%  #Start wind
  filter(EffortMeasurement5 <= 1 | is.na(EffortMeasurement5)) %>%  #Start precip
  select(-min_year, -min_temp) # This last line is optional, it removes the helper 'min_year' column

    
  ##____________________________________________________________________
  #Create a dataframe with a single lat long per route ID (what the first stop in a route). This is necessary because some Ontario routes only have a lat long for the first stop in a route. 
  loc.dat <-NULL #clear old
  
  loc.dat<-dat %>% filter(latitude!="NA") %>% distinct(RouteIdentifier, .keep_all = TRUE) %>% select(RouteIdentifier, latitude, longitude, protocol_id) %>% distinct()
  write.table(loc.dat, file = paste(out.dir, "Map.csv", sep = ""), row.names = FALSE, append = TRUE, quote = FALSE, sep = ",", col.names = FALSE)
  
  ##____________________________________________________________________
  #The number of stops on a route within a year is used an an effort covariate in the model
  stop.year<-dat %>% group_by(RouteIdentifier, survey_year) %>% summarize(nstop=n_distinct(SurveyAreaIdentifier))
  dat<-left_join(dat, stop.year, by=c("RouteIdentifier", "survey_year"))
  
  ##___________________________________________________________________
  #Filter incomplete routes, which are defined here as routes that had less than 5 stops complete in a year. 
  dat<- dat %>% filter(nstop>=5)
  
  
  #Note: Since our response variable in the analysis is counts at the route-level, the sampling event is a route within a year. If the analysis is changed to be at the stop level, you will want to include `month` and `day` in the code below.   
  event.data <-NULL #clear old
  event.data <- dat %>%
    select(collection, RouteIdentifier, StateProvince, survey_year, survey_month, survey_day, CollectorNumber) %>%  
    distinct() %>%
    ungroup() %>%
    as.data.frame()
  
  #merge with the loc.data to assign unique lat long to each route
  event.data<-left_join(event.data, loc.dat, by=c("RouteIdentifier"))
  
  
  ##____________________________________________________________________
  #filter for target species
  sp.list<-c("Great Horned Owl", "Boreal Owl", "Northern Saw-whet Owl", "Barred Owl", "Great Grey Owl")
  dat <- dat[dat$CommonName %in% sp.list, ]
  
  ##____________________________________________________________________
  #Calculate count per survey route in a year.
  dat<-dat %>% group_by(collection, protocol_id, StateProvince, RouteIdentifier, nstop, survey_year, survey_month, survey_day, doy, CommonName, species_id) %>% summarise(Count = sum(ObservationCount))
  
  
  #Create the output tables for writing the data cleaning results
  write.table(event.data, file = paste(out.dir, "SamplingEvents.csv", sep = ""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")
  
  write.table(dat, file = paste(out.dir, "NationalOwlData",".csv", sep = ""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")
  
  write.table(loc.dat, file=paste(out.dir, "Map.csv", sep = ""), row.names = FALSE, append = FALSE, quote = FALSE, sep = ",")
  
  #Create events maps for review
  
 events_sf <- st_as_sf(event.data,coords = c("longitude", "latitude"), crs = 4326)
 
 ggplot(data = events_sf) +
    # Select a basemap
    annotation_map_tile(type = "cartolight", zoom = NULL, progress = "none") +
    # Plot the points, color-coded by survey_year
    geom_sf(aes(color = as.factor(survey_year)), size = 1) +
    # Facet by survey_year to create the multi-paneled map
    facet_wrap(~ survey_year) +
    # Add a theme with a minimal design and change the font styles, to your preference
    theme_minimal() +
   theme(legend.position = "none")+
    # Define the title and axis names
    labs(title = "Owl Routes by Survey Year",
         x = "Longitude",
         y = "Latitude")
