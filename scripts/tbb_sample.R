# This is the first part to run, it only needs to run once
# generate patches for each subject

# set up working directory 
setwd("/Users/TBB/Desktop/eye-tracking data")

# read the file of stimuli
stimuli <- read.csv("stimulus_order.csv", stringsAsFactors = FALSE)

# categorize stimuli in their level, h/v and type
category <- factor(stimuli$stimuli)
cat <- data.frame(cbind(summary, levels(category)))

# randomly assign 24 types to 10 subjects
tbb_sample <- function(n = 24, m = 10){
  set.seed(1)
  x <- sample(1:n)
  y <- sort(sample(1:(n-1))[1:(m-1)])
  y <- c(y,n)
  s <- c()
  i <- 1
  sample.list = list()
  for(j in 1:m){
    sample.list[[j]] <- sort(x[i:y[j]])
    i <- y[j]+1
  }
  sample.list
}

sample.list <- tbb_sample()

# find out patch name for the assignment
name.list <- list()
for (i in 1:10){
    name.list[[i]] <- NaN
    for (j in 1:length(sample.list[[i]])){
    name.list[[i]][j] <- levels(category)[sample.list[[i]][j]]
    }
}

# decide the exact MEDIA_ID
# randomly choose an index from when the stimuli name match
sample.ID <- function(x = c()){
    ID <- c()
    set.seed(2)
    for (i in 1: length(x)){
    index.pool <- stimuli[stimuli$stimuli == x[i], "index"]
    ID[i] <- sample(index.pool, size = 1)
    }
    ID
}
chosen.ID <- lapply(name.list, sample.ID)    

