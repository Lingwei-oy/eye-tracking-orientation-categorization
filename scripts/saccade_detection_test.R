# set up working directory for each participant, now only analyze participant 98 
setwd("/Users/TBB/Desktop/eye-tracking data")

# select valid stimuli, exclude fixation point and blank screen 
stimuli <- read.csv("stimulus_order.csv", stringsAsFactors = FALSE)

# create data frame for all the stimuli of one subject
E1 <- read.csv("E98.csv", stringsAsFactors = FALSE)

# create subset data frame for only MEDIA_ID, FPOGX, FPOGY, exclude non-picture stimuli
E2 <- subset(E1, select = c(MEDIA_ID, FPOGX, FPOGY, TIME), E1$MEDIA_ID %in% stimuli$index)

# add names to picture
for (i in 1:length(E2$MEDIA_ID)){
        E2[i, "name"] <- stimuli[stimuli$index == E2[i, "MEDIA_ID"],'stimuli']
    }

# DIF is function to calculate position difference, returns a vector
DIF <- function(x){
    y <- tapply(x,E2$MEDIA_ID,diff)
    DIF <- lapply (y, append, NA, 0)
    unlist(DIF)
}

# add DIFF back to E2
E2$DIFF_X <- DIF(E2$FPOGX)
E2$DIFF_Y <- DIF(E2$FPOGY)

# generate all points selected by quantile and quantile vector
# QUANTILE <- function(x = c(), upper, lower = 1- upper){
   
    # y<- x[ !is.na(x) & (x > quantile(x, upper, na.rm = T) | x < quantile(x, lower, na.rm = T) ) ]
# }

# categorize stimuli in their level, h/v and type
category <- factor(stimuli$stimuli)
cat <- data.frame(cbind(summary, levels(category)))

# divide 48 pictures(2 in each kind) to 10 groups randomly


# decide quantiles
q <- quantile(E2$DIFF_X, c(0.05, 0.95), na.rm = T)
# plot randomly-chosen patch
plot(E2$TIME[E2$name == x], E2$DIFF_X[E2$name == x], 
     col = ifelse(((E2$DIFF_X) < q[1] | (E2$DIFF_X) > q[2]), "red", "black"))
