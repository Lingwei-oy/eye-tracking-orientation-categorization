# load data
load("data_final.RData")
# This is the first part to run, read stimuli files
# set up working directory for each participant
setwd("/Users/Lingwei/Desktop/perceptual_organization_data_analysis")

# have changed 443 to 445, 558 to 559 in stimulus_order.csv 
# select valid stimuli, exclude fixation point and blank screen 
stimuli <- read.csv("stimulus_order.csv", stringsAsFactors = FALSE)


# DIF is function to calculate position difference, returns a vector
DIF <- function(x, y = c()){
    z <- tapply(x, y ,diff)
    DIF <- lapply (z, append, NA, 0)
    unlist(DIF)
}

# organize eye-tracking files for 36 subjects in a list
filenames <- list.files("data2")
collector <- data.frame()

# generate exploratory figures for the first participant
library(data.table)

# differentiate fixation and saccade based on 80% of each participant
for (i in 1:length(filenames)){
print(i)
test1 <- fread(paste("data2/", filenames[i], sep = ""), select = c(1,4,6,7,9,10,11), header = TRUE, stringsAsFactors = FALSE)
E2 <- subset(test1, test1$MEDIA_ID %in% stimuli$index)
colnames(E2)[2] <- "time"
E2$subject <- rep(gsub(".csv", "" , filenames[i]), length(E2$MEDIA_ID))
E2$DIFF_X <- DIF(x= E2$FPOGX, y = E2$MEDIA_ID)
E2$DIFF_Y <- DIF(x = E2$FPOGY, y = E2$MEDIA_ID)
E2$distance <- sqrt(E2$DIFF_X**2 + E2$DIFF_Y**2)
E2$percentile <- rep(quantile(E2$distance, 0.8, na.rm = T), length(E2$MEDIA_ID))

E2$gaze <- ifelse(!(is.na(E2$DIFF_X)), ifelse(E2$distance < E2$percentile, "fixation", "saccade"), NA)
E2$tan <- tan(E2$DIFF_Y/E2$DIFF_X)
E2$direction <- ifelse(!(is.na(E2$tan)), 
                       ifelse(E2$gaze == "saccade",
                       ifelse(abs(E2$tan) < tan(pi/3), 
                              ifelse(abs(E2$tan) < tan(pi/6), "horizontal", "diagonal"), 
                              "vertical"), "fixation"), NA)

collector <- rbind(collector,E2)
    
}

write.csv(collector, file = "collector.csv")

# calculate proportion of each direction
output <- c()

for (subj in filenames){
        data <- collector[collector$subject==gsub(".csv", "", subj),c('MEDIA_ID','direction')]
        for(id in stimuli$index){
                data_id <- data[data$MEDIA_ID==id,'direction']
                l <- nrow(data_id)
                h <- nrow(data_id[data_id$direction=="horizontal",])/l
                v <- nrow(data_id[data_id$direction=="vertical", ])/l
                d <- nrow(data_id[data_id$direction=="diagonal", ])/l
                f <- nrow(data_id[data_id$direction=="fixation", ])/l
                output <- rbind(output, c(subj,id,h,v,d,f))
        }
}

colnames(output) <- c('subject', 'MEDIA_ID', 'H', 'V', 'D', 'F')


# merge eye-tracking data with behavioral data
other_info <- read.csv(file = "trial_wise_total_fixed_4.csv", header = TRUE, stringsAsFactors = FALSE)
output_new <- cbind(output, other_info[, c(4,5,6,7,9,11)])
write.csv(file = "output_new.csv", output_new)

# get the row number of which that are all NA's, drop two rows here
apply(other_info, 2, function(x){sum(is.na(x))})
output_new <- na.omit(output_new)

# calculate stimuli_wise average 

# save R workspace
save.image("data_final.RData")
