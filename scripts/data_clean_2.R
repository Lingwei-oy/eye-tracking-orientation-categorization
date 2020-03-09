# This is the first part to run, read stimuli files
# set up working directory for each participant
setwd("/Users/TBB/Desktop/eye-tracking data")

# changed 443 to 445, and 558 to 559 before loaded 2/2/2019 23:26
# select valid stimuli, exclude fixation point and blank screen 
stimuli <- read.csv("stimulus_order.csv", stringsAsFactors = FALSE)

# read data in Cosku version
new_sub <- read.csv("0202_Master Data for PO Study.csv", stringsAsFactors = FALSE)

# Use new version of Cosku data, 2/3/2019 10:54, reshuffle subject in increasing order in excel
# delete the blank line in new_sub in excel, which is line -5617 2/2/2019 23:37
# sort Participant in increasing order 2/2/2019 23:38

# find new names in Cosku
new_name <- unique(new_sub$Participant)

# DIF is function to calculate position difference, returns a vector
DIF <- function(x, y = c()){
    z <- tapply(x, y ,diff)
    DIF <- lapply (z, append, NA, 0)
    unlist(DIF)
}

# list files in the data file
filenames <- list.files("data2")

# organize files in a list
collector <- data.frame()

for (i in 1:length(filenames)){
    # read the file
    E1 <- read.csv(paste("data2/", filenames[i], sep = ''), header = TRUE, stringsAsFactors = FALSE)
    
    # change the fourth column name as E1
    colnames(E1)[4] <- "TIME"
    
    # create subset data frame for only MEDIA_ID, FPOGX, FPOGY, exclude non-picture stimuli
    E2 <- subset(E1, select = c(MEDIA_ID, FPOGX, FPOGY, TIME), E1$MEDIA_ID %in% stimuli$index)
    
    # add DIFF back to E2
    E2$DIFF_X <- DIF(x= E2$FPOGX, y = E2$MEDIA_ID)
    E2$DIFF_Y <- DIF(x = E2$FPOGY, y = E2$MEDIA_ID)
    
    # add subject mark to E2
    E2$subject <- rep(filenames[i], length(E2$MEDIA_ID))
    
    # add the new dataframe
    collector <- rbind(collector,E2)
    
}

# return 20% quantile of collector

quantile.x <- quantile(collector$DIFF_X,c(0.1, 0.9), na.rm = T) 
quantile.y <- quantile(collector$DIFF_Y,c(0.1, 0.9), na.rm = T)

# label saccades/drift and fixation

condition2 <- collector$DIFF_X > quantile.x[2] | collector$DIFF_X < quantile.x[1] | collector$DIFF_Y >  quantile.y[2] | collector$DIFF_Y < quantile.y[1]
condition1 <- is.na(collector$DIFF_X) | is.na(collector$DIFF_Y)

collector$gaze <- ifelse(condition1, NA , 
                                         ifelse(condition2, "saccade", "fixation"))
                                                                
collector$tan <- ifelse (collector$gaze == "saccade", collector$DIFF_Y/collector$DIFF_X, NA)

collector$direction <- ifelse(is.na(collector$tan), "fixation",
                              ifelse(abs(collector$tan)>tan(pi/3), "vertical", 
                                    ifelse(abs(collector$tan) < tan(pi/6), "horizontal", "diagonal")))

# reorganize all the data
redata <- read.csv("20190301.csv", stringsAsFactors = FALSE)
names <- unique(redata$Participant)
data_final <- NULL
for (name in names){
    subject <- redata[redata$Participant == name, ]
    for (j in 7:12){
    s <- tapply(subject[ ,j],subject[ ,'Type'], mean)
    s <- c(name, names(redata)[j], unlist(s))
    data_final <- rbind(data_final, s)
    }
}
