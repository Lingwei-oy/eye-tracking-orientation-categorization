# This is the first part to run, read stimuli files
# set up working directory for each participant
setwd("/Users/Lingwei/Desktop/eye-tracking data")

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

for (i in 1:length(filenames)){
    print(i)
    # read the file
    E1 <- read.csv(paste("data2/", filenames[i], sep = ""), header = TRUE, stringsAsFactors = FALSE)
    
    # change the fourth column name E1 as TIME
    colnames(E1)[4] <- "TIME"
    
    # create subset data frame for only MEDIA_ID, FPOGX, FPOGY, exclude non-patch stimuli
    E2 <- subset(E1, select = c(MEDIA_ID, FPOGX, FPOGY, TIME), E1$MEDIA_ID %in% stimuli$index)
    
    # add DIFF back to E2
    E2$DIFF_X <- DIF(x= E2$FPOGX, y = E2$MEDIA_ID)
    E2$DIFF_Y <- DIF(x = E2$FPOGY, y = E2$MEDIA_ID)
    
    # add subject mark to E2
    E2$subject <- rep(filenames[i], length(E2$MEDIA_ID))
    
    # add the new dataframe
    collector <- rbind(collector,E2)
    
}

# calculate distance change from point to point:
x <- collector$DIFF_X
y <- collector$DIFF_Y
collector$distance <- NULL
for (i in length(x)){
  collector$distance <- (x**2 + y**2)**1/2
 
}

# exploratory analysis to collector$distance
d <- collector$distance
d <- na.omit(d)
summary(d)
boxplot(d, ylim = c(min(d, na.rm = TRUE), max(0.0002, na.rm = TRUE)))
summary(collector$distance)
# plot histogram
hist(d[d < summary(d)[5]],probability=T, main="Histogram of dispersion less than 1st quantile",xlab="dispersion")
lines(density(d < summary(d)[5]),col=2)

# check normality
library(nortest)
ad.test(d)
shapiro.test(d[0:5000])
qqnorm(d)
qqline(d, col = 2)
# return 20% quantile of collector


q <- quantile(collector$distance,0.8, na.rm = T) 


# label saccades/drift and fixation

condition2 <- collector$distance > q["0.8"] 
condition1 <- is.na(collector$DIFF_X) | is.na(collector$DIFF_Y)

collector$gaze <- ifelse(condition1, NA , 
                                         ifelse(condition2, "saccade", "fixation"))
                                                                
collector$tan <- ifelse (collector$gaze == "saccade", collector$DIFF_Y/collector$DIFF_X, NA)

collector$direction <- ifelse(is.na(collector$tan), 0,
                              ifelse(abs(collector$tan)>tan(pi/3), "vertical", 
                                    ifelse(abs(collector$tan) < tan(pi/6), "horizontal", "diagonal")))
# for every patch in every subject, calculate proportion of vertical, horizontal and diagonal
collect <- split(subset(collector, select = c(MEDIA_ID, subject, direction)), collector$subject)
library(purrr)
library(plyr)

prop <- function(x = data.frame()){
    y <- count(x$direction, 'direction')
    y[, 'freq'] <- y[ ,'freq']/nrow(x$direction)
}
test <- tapply(collect[[1]], prop, INDEX = "MEDIA_ID")
