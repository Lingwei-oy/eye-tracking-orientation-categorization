# This is the first part to run, read stimuli files
# set up working directory for each participant
setwd("/Users/TBB/Desktop/eye-tracking data")

# select valid stimuli, exclude fixation point and blank screen 
stimuli <- read.csv("stimulus_order.csv", stringsAsFactors = FALSE)

# set up working directory again for organizing files
setwd("/Users/TBB/Desktop/eye-tracking data/data2")

# DIF is function to calculate position difference, returns a vector
DIF <- function(x, y = c()){
    z <- tapply(x, y ,diff)
    DIF <- lapply (z, append, NA, 0)
    unlist(DIF)
}

# organize sample files in a list
filenames <- list.files()
collector <- data.frame()

for (i in 1:length(filenames)){
    # read the file
    E1 <- read.csv(filenames[i], header = TRUE, stringsAsFactors = FALSE)
    
    # change the fourth column name as E1
    colnames(E1)[4] <- "TIME"
    
    # create subset data frame for only MEDIA_ID, FPOGX, FPOGY, exclude non-picture stimuli
    E2 <- subset(E1, select = c(MEDIA_ID, FPOGX, FPOGY, TIME), E1$MEDIA_ID %in% stimuli$index)
    
    # add DIFF back to E2
    E2$DIFF_X <- DIF(x= E2$FPOGX, y = E2$MEDIA_ID)
    E2$DIFF_Y <- DIF(E2$FPOGY, y = E2$MEDIA_ID)
    
    # add subject mark to E2
    E2$subject <- rep(filenames[i], length(E2$MEDIA_ID))
    
    # add the new dataframe
    collector <- rbind(collector,E2)
    
}

# return 90%, 85%, 80%, 75% quantile of collector

quantile <- list(c(0.05, 0.95), c(0.075, 0.925), c(0.1, 0.9), c(0.125, 0.875))
quantile.x <- sapply(quantile, function(x) quantile(collector$DIFF_X, x, na.rm = T)) 
colnames(quantile.x) <- c("10percent", "15percent", "20percent", "25percent")
rownames(quantile.x) <- c("lower", "upper")
quantile.y <- sapply(quantile, function(x) quantile(collector$DIFF_Y, x, na.rm = T))
colnames(quantile.y) <- c("10percent", "15percent", "20percent", "25percent")
rownames(quantile.y) <- c("lower", "upper")


# randomly choose ten subjects
set.seed(3)
sample.files <- sample(filenames, 10)

# extract names of sampled patches
sample.patch <- data.frame()
for (i in 1:length(sample.files)){
    for (j in 1:length(chosen.ID[[i]])){
    
    sample.patch <- rbind(sample.patch, collector[collector$subject == sample.files[i] & collector$MEDIA_ID == chosen.ID[[i]][j], ])
    }
}

patch_chosen <- split(sample.patch, sample.patch$MEDIA_ID)

# plot randomly-chosen patch
print <- function(patch = data.frame()){
    for (i in 1:4){
         png(filename=paste(colnames(quantile.y)[i], '-', patch$MEDIA_ID[1], ".png", sep = ''), width = 1000, height = 500)
        plot(patch$TIME, patch$FPOGY, col = ifelse(patch$DIFF_Y < quantile.y["lower", i] | patch$DIFF_Y > quantile.y["upper", i], "green", "black"), xlab= "time", ylab = "y", main = paste(colnames(quantile.y)[i], patch$subject[1]))
        dev.off()
    }
}

# plot and save all patch_chosen 
sapply(patch_chosen, print)

# write data into the mass spread sheet
final <- read.csv("final.csv", header = TRUE, stringsAsFactors = FALSE)
subf <- final[ ,c("Media_ID", "Type", 'Participant', 'H..', 'V..', 'D..', 'Fixations..')]
output[order(output["subject"]), ]
for (subj in filenames){
    person <- subf[subf$Participant == output["subject"], ]
                 
}
