# This file is used to output mean of each condition
output <- c()
# s=0
colnames(output) <- c('subject', 'MEDIA_ID', 'H', 'V', 'D', 'F')
for (subj in filenames){
    data <- collector[collector$subject==subj,c('MEDIA_ID','direction')]
    for(id in stimuli$index){
        data_id <- data[data$MEDIA_ID==id,'direction']
        l <- length(data_id)
        h <- length(data_id[data_id=="horizontal"])/l
        v <- length(data_id[data_id=="vertical"])/l
        d <- length(data_id[data_id=="diagonal"])/l
        f <- length(data_id[data_id=="fixation"])/l
#        if (is.na(h)|is.na(v)|is.na(d)|is.na(zero)){
#            s <- s+1
#        }
#       if (l==0){
#            s <- s+1
#       }
        output <- rbind(output, c(subj,id,h,v,d,f))
    }
}

# check NaN
x <- output[, 3:6] == "NaN"
outNA <- output[x[ ,1] == T, ]
View(outNA)

# output to csv file
write.csv(output, file = "output2.csv")
# add columns of H/V/D/F back to Cosku data by Excel
# don't need to change 559/445 because just data have been shuffled in the same order
# attention stimuli and output2 csv 559/445 aren't changed back to 558/443

# read aggregated data back again and begin analysis
master <- read.csv("0202_Master Data for PO Study.csv", header = TRUE, stringsAsFactors = FALSE)
colnames(master)

#subset data for direction analysis 
mas <- master[, c(3, 4, 5, 6, 7, 8, 10, 11, 12, 18, 19, 20, 21)]
colnames(mas)


#1 whether mean of H/V/D/F are different across H/V patches?
# the result shows none of H/V/D/F differs in H/V groups
# get descriptive analysis contrast two directions for each group
m <- c()
for (i in c(10:13)){
    res.mas <- aov(mas[ ,i] ~ mas$Orientation, data = mas)
    print(summary(res.mas))
    m <- rbind(m, unlist(tapply(mas[ ,i], mas$Orientation, summary)))
  
#    boxplot(mas[[i]] ~ mas$Orientation, main = 'direction by patch orientation', ylab = colnames(mas)[i], col=rainbow(2))
}
rownames(m) <- c("H", 'V', 'D', 'F')
write.csv(m, file = "results.csv")

#2 whether H/V/D/F differ in right/wrong choices 
m <- c()
for (i in c(10:13)){
    res.mas <- aov(mas[, i] ~ mas$Accuracy, data = mas)
    print(summary(res.mas))
    m <- rbind(m, unlist(tapply(mas[ ,i], mas$Accuracy, summary)))
}
rownames(m) <- c("H", 'V', 'D', 'F')

#3 whether H/V/D/F differ between different levels
for (i in c(10:13)){
    res.mas <- aov(mas[, i] ~ as.factor(mas$Level), data = mas)
    print(TukeyHSD(res.mas))
    print(summary(res.mas))
    print(tapply(mas[ ,i], mas$Level, summary))
}
# check posthoc analysis

#3.2 check consistency of movement and response
# add column response to the dataset
o <- mas$Orientation
a <- mas$Accuracy
d <- c('H', 'V')
mas$Response <- ifelse(a == 1, o, ifelse(a == d[1], d[2], d[1]))

for (i in c(10,11)){
    res.mas <- aov(mas[ ,i] ~ as.factor(mas$Response), data = mas)
    print(summary(res.mas))
    print(unlist(tapply(mas[ ,i], mas$Response, mean)))
}

#4 check whether people would show differences for three stimuli types
for (i in c(10:13)){
    res.mas <- aov(mas[, i] ~ as.factor(mas$Category), data = mas)
    print(TukeyHSD(res.mas))
    print(summary(res.mas))
    print(unlist(tapply(mas[ ,i], mas$Category, mean)))
}

# print std for all tests
for (i in c(10:13)){
    for (j in c('Response', 'Orientation', 'Category')){
    print(paste(i, j, unlist(tapply(mas[ ,i], as.factor(mas[ ,j]), sd))))
    }
}
#5 check whether reaction time is correlated with four types of movement
#5 check whether people show difference for three stimuli types across 
