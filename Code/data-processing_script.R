# clear everything
rm(list=ls())
closeAllConnections()

# Working directory Local
setwd("C:/Users/us424589/Documents/background_check/mpg_bgck/Code")
data.input.path <- "C:/Users/us424589/Documents/background_check/Data/Raw"
data.output.path <- "C:/Users/us424589/Documents/background_check/Data/Cleaned"
#output.path <- "Output"

####======================================= Data Load =======================================####
data <- read_csv(file.path(data.input.path, 'raw_dataset_3.csv'),
                 col_types = cols(FinalCharge = col_character(),
                                  DispistionDate = col_integer()))
  
# Set US state names
states <- c('AL', 'AK', 'AZ', 'AR', 'CA', 'CO', 'CT', 'DE', 'DC', 'FL', 'GA', 'HI', 'ID',
            'IL', 'IN', 'IA', 'KS', 'KY', 'LA', 'ME', 'MD', 'MA', 'MI', 'MN', 'MS', 'MO',
            'MT', 'NE', 'NV', 'NH', 'NJ', 'NM', 'NY', 'NC', 'ND', 'OH', 'OK', 'OR', 'PA',
            'RI', 'SC', 'SD', 'TN', 'TX', 'UT', 'VT', 'VA', 'WA', 'WV', 'WI', 'WY')
  
# Create new column for State of offense
data <- transform(data, State_clean = ifelse(State %in% states, State, NA))

# Separate out misdemeanor and felony history
data <- transform(data, misdemeanor_history = ifelse(CaseLevel == 'M',1, 0))
data <- transform(data, felony_history = ifelse(CaseLevel == 'F',1, 0))

data$misdemenor_history <- data$misdemeanor_history %>% replace_na(0)
data$felony_history <- data$felony_history %>% replace_na(0)

# Indicate if a check has been failed
data <- transform(data, failed_check = ifelse(SearchScore == 'Consider',1, 0))


######## Merging into single line //!\\
#Iterate through each person and combine the rows
for (item in unique(data$BgOrderID))
{
  #Select the rows corresponding to 1 single person
  rows <- data[which(data$BgOrderID == item)]
}

# Sample merging code //!\\
print(sum)
tmp = which(data$BgOrderID == 89062129)

tmp_rows = data[tmp,]
View(tmp_rows)
#aggregate(tmp_rows)




