source('anomaly_functions.R')
library(plyr)
folder <- getwd() 

######################################################
##############Sourcing Sentiment Data################
####################################################

# Create list of all .csv files in folder
file_list <- list.files(path=paste0(folder, '/Crimson Sentiment Files'), pattern="*.csv") 

# Read in each .csv file in file_list and create a data frame with the 
all_files <- do.call(rbind.fill, lapply(file_list, function(x) read.csv(paste0(paste0(folder, '/Crimson Sentiment Files'), '/',x), stringsAsFactors = FALSE)))

# Replace column headers with dev-friendly column headers
colnames(all_files) <- c('celebrityid', 'fullname', 'sentiment_date', 'basic_positive_sentiment', 'basic_neutral_sentiment', 'basic_negative_sentiment')

# Convert analysis_date from character to actual date object
all_files$sentiment_date <- as.Date(all_files$sentiment_date, format = "%Y-%m-%d")

all_files$percent_negative <- all_files$basic_negative_sentiment/(all_files$basic_positive_sentiment + all_files$basic_negative_sentiment)

#I am only interested in identifying anomalous spikes in negative sentiment so I will remove positive and neutral sentiment volume columns
all_files <- all_files[,-c(4,5)]

#save data frame including each celebrity's daily sentiment volumes
write.csv(all_files, paste0(folder, '/Data Output/all_files.csv'))


######################################################
##############Run anomaly signaling function#########
####################################################

for (i in 1:length(unique(all_files$celebrityid))){
  # Create a subset of input csv for every unique celebrityid
  subset <- subset(all_files, all_files$celebrityid == unique(all_files$celebrityid)[i])
  
  # Dynamic threshold set to be (x) standard deviations the robust max is away from the mean number of negative sentiment volume
  threshold <- floor((IQRmean(subset$basic_negative_sentiment) - mean(subset$basic_negative_sentiment))/sd(subset$basic_negative_sentiment))
  if (is.nan(threshold)){
    threshold <- 9
  }
  if (i == 1){
    negative_thresholding_alg_signals <- data.frame(ThresholdingAlgo(subset$basic_negative_sentiment,subset$sentiment_date,subset$celebrityid,threshold))
    
  }else{
    negative_thresholding_alg_signals <- rbind(negative_thresholding_alg_signals, data.frame(ThresholdingAlgo(subset$basic_negative_sentiment,subset$sentiment_date,subset$celebrityid,threshold)))
  }
}
#build out final data frame
negative_thresholding_alg_signals <- merge(all_files, negative_thresholding_alg_signals, by = c('celebrityid', 'sentiment_date'))

# only want to save positive sentiment signals 
negative_thresholding_pos_signals <- subset(negative_thresholding_alg_signals, negative_thresholding_alg_signals$signals == 1)

# save final data frame
write.csv(negative_thresholding_pos_signals, 'negative_thresholding_pos_signals.csv')
