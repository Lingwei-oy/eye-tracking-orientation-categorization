# set up working directory for each participant, now only analyze participant 98 
setwd("/Users/TBB/Desktop/eye-tracking data")

# select valid stimuli, exclude fixation point and blank screen 
stimuli <- read.csv("stimulus_order.csv", stringsAsFactors = FALSE)

# create data frame for all the stimuli of one subject
E1 <- read.csv("E98.csv", stringsAsFactors = FALSE)

# create subset data frame for only MEDIA_ID, FPOGX, FPOGY, exclude non-picture stimuli
E2 <- subset(E1, select = c(MEDIA_ID, FPOGX, FPOGY), MEDIA_ID %in% stimuli$index)

# add x_change and y_change columns and split dataframe into patches arranged by MEDIA_ID
E2$x_change <- NULL
E2$y_change <- NULL
E2_split <- split(E2, E2$MEDIA_ID)

# calculate direction and rate for each patch, rate = change/ time, time = 1/(60 Hz) = 1/60 s, need to get threshold for h/v
for (j in 1:length(E2_split)){
  for (i in 2:length(E2_split[[j]][["FPOGX"]])){
   E2_split[[j]][i,"x_change"] <- E2_split[[j]][i, "FPOGX"]-E2_split[[j]][i-1, "FPOGX"]
   E2_split[[j]][i,"y_change"] <- E2_split[[j]][i, "FPOGY"]-E2_split[[j]][i-1, "FPOGY"]
   E2_split[[j]][i,"direction"] <- atan2(E2_split[[j]][i, "x_change"], E2_split[[j]][i, "y_change"])      
  }
}






