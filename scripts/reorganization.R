# reorganize the output
#set up working directory
setwd("/Users/lingwei/Desktop/eye-tracking data")
data <- read.csv("20190323.csv", stringsAsFactors = FALSE)
data2 <- t(data)
output <- data.frame()
for (i in 1:36){
  output <- rbind(output, data2[3:26 ,(6*i-5):(6*i)])
}

# add subject
id <- unique(data$subject)
output$subject <- rep(id, each = 24)

# add orientation/type/level
stimuli <- colnames(data)[3:26]
s <- data.frame()
for (i in 1:24){
  s[i , "sti"] <- stimuli[i]
  s[i, "orientation"] <- ifelse(length(grep("h" , s$sti[i])) == 1, "H", "V")
  s[i, "type"] <- ifelse(length(grep("col", s$sti[i])) == 1, "color", ifelse (length(grep("gab", s$sti[i])) == 1, "gabor", "luminance"))
  s[i, "level"] <- ifelse(length(grep("100", s$sti[i])) == 1, "100", ifelse (length(grep("90" , s$sti[i])) == 1, "90", ifelse (length(grep("80" , s$sti[i])) == 1,"80", "70")))
}
for (i in 1:4){
  output[7+i] <- rep(s, 36)[[i]]
}
colnames(output) <- c("RT", "Accuracy", "H", "V", "D", "F", "subject", "stimuli", "orientation", "type", "level")

write.csv(output, file = "20193024.csv")
res <- cor.test(as.numeric(output[output$orientation == "H" & output$level == "90" & output$type == "luminance","RT"]), as.numeric(output[output$orientation == "H" & output$level == "90" & output$type == "luminance","F"]), method = "pearson")
res
