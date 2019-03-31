# This is the first part to run, read stimuli files
# set up working directory for each participant
setwd("/Users/TBB/Desktop/eye-tracking data")


master <- read.csv("master.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(master)

mas <- master[, c(3, 4, 5, 6, 7, 8, 10, 11, 12, 18, 19, 20, 21)]
## No 443 or 558 patch for every subject, even in original excel spreadsheet
## exclude them from here
mas <- mas[!(mas$Media_ID == 558 | mas$Media_ID == 443), ] 

# assign value to blank lines in subject ESCR006 patch 183 and 64
test <- as.numeric(output[output[,"MEDIA_ID"] == "183" & output[ ,"subject"] == "ESCR006_all_gaze.csv", c("H", "V", "D", "0")])  
mas[mas$Participant == "ESCR006" & mas$Media_ID == 183, c("H..", "V..", "D..", "Fixations..")] <- test

# split mas 
ma <- split(mas, mas$Participant)
sapply(ma, function(x)sum(is.na(x$H..)))

# output cleaned dataset
write.csv(mas, file = "mas.csv")

# do manova analysis
res.mas <- aov(V..[mas$Orientation == 'H'] ~ as.factor(Level)[mas$Orientation == 'H'], data = mas)
summary(res.mas)

# return mean for H/V/D/F in Orientation, Level, Accuracy
a <- c('H..', 'V..', 'D..', 'Fixations..')
b <- c('Orientation', 'Level', 'Accuracy')

# level <- list()
# for (i in length(b))
# level[[i]] <- levels(as.factor(mas[, b[i]]))
m <- list()
for (j in length(a)){
    m[[j]] <- NaN
    for (i in length(b)){
        m[[j]] <- unlist(tapply(mas[ ,a[j]], INDEX = mas[ ,b[i]], mean))
    }
 }
