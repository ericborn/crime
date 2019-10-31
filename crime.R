library(plotly)
library(ggplot2)
library(corrplot)

# read the csv file into R
dat <- read.csv(file='F:/Code projects/BU/555 - analysis and vis/Final/SD-crime.csv', sep = ',')

# Remove columns 2-6, 8-9, 11-12, 15, 16 which are not being used during this analysis
crime.df <- dat[, c(1,7,10,13,14)]

# create a new column containing just the year
crime.df$year <- substr(crime.df$month, 5, 6)

# summary stats for data
full.stats <- data.frame('Measure' = c('Violent Crime', 'Burglary', 'Thefts', 
                                       'Vehicle Theft'),
                         'Min' = c(summary(crime.df$Total.Violent.Crime)[[1]],
                                   summary(crime.df$Total.Burglary)[[1]],
                                   summary(crime.df$Total.Thefts)[[1]],
                                   summary(crime.df$Motor.Vehicle.Theft)[[1]]),
                         
                         'q1' = c(summary(crime.df$Total.Violent.Crime)[[2]],
                                  summary(crime.df$Total.Burglary)[[2]],
                                  summary(crime.df$Total.Thefts)[[2]],
                                  summary(crime.df$Motor.Vehicle.Theft)[[2]]),
                         
                         'Median' = c(summary(crime.df$Total.Violent.Crime)[[3]],
                                      summary(crime.df$Total.Burglary)[[3]],
                                      summary(crime.df$Total.Thefts)[[3]],
                                      summary(crime.df$Motor.Vehicle.Theft)[[3]]),
                         
                         'Mean' = c(summary(crime.df$Total.Violent.Crime)[[4]],
                                    summary(crime.df$Total.Burglary)[[4]],
                                    summary(crime.df$Total.Thefts)[[4]],
                                    summary(crime.df$Motor.Vehicle.Theft)[[4]]),
                         
                         'q3' = c(summary(crime.df$Total.Violent.Crime)[[5]],
                                  summary(crime.df$Total.Burglary)[[5]],
                                  summary(crime.df$Total.Thefts)[[5]],
                                  summary(crime.df$Motor.Vehicle.Theft)[[5]]),
                         
                         'Max' = c(summary(crime.df$Total.Violent.Crime)[[6]],
                                   summary(crime.df$Total.Burglary)[[6]],
                                   summary(crime.df$Total.Thefts)[[6]],
                                   summary(crime.df$Motor.Vehicle.Theft)[[6]]))

# reset factors to order by Measure column
full.stats$Measure <- factor(full.stats$Measure, 
                             levels = c(as.character(full.stats$Measure)))

# Convert factors to character
full.stats$Measure <- as.character(full.stats$Measure)

# create table for summary stats
summary.stat.table <- plot_ly(
  type = 'table',
  height = 180,
  #width = 700,
  columnwidth = c(25, 15, 15, 15, 15),
  header = list(
    values = c('Measure', 'Min', '1st Q', 'Median', 'Mean', '3rd Q', 'Max'),
    line = list(width = 1, color = 'black'),
    fill = list(color = c('#1f77b4', '#1f77b4')),
    font = list(famile = 'Arial', size = 14, color = 'white')
  ),
  cells = list(
    values = rbind(full.stats$Measure, full.stats$Min, full.stats$q1, full.stats$Median, 
                   round(full.stats$Mean,1), full.stats$q3, full.stats$Max),
    align = c('center'),
    line = list(width = 1, color = 'black')
  ))

# Plot summary table
summary.stat.table

# create a new column to track changes from month to month
for (row in 1:nrow(crime.df)){
  if (is.na(crime.df$Total.Violent.Crime[row + 1]
            - crime.df$Total.Violent.Crime[row])) {
  } 
  else {
    crime.df$violent.change[row + 1] <- (crime.df$Total.Violent.Crime[row + 1]
                                         - crime.df$Total.Violent.Crime[row])
    crime.df$burglary.change[row + 1] <- (crime.df$Total.Burglary[row  + 1]
                                          - crime.df$Total.Burglary[row])
    crime.df$thefts.change[row + 1] <- (crime.df$Total.Thefts[row + 1]
                                        - crime.df$Total.Thefts[row])
    crime.df$vehicle.change[row + 1] <- (crime.df$Motor.Vehicle.Theft[row + 1]
                                         - crime.df$Motor.Vehicle.Theft[row])
  }
}

# create a dataframe containing only the monthly changes
crime.change <- crime.df[, c(7:10)]

# summary stats for data
change.stats <- data.frame('Measure' = c('Violent Crime', 'Burglary', 'Thefts', 
                                         'Vehicle Theft'),
                           'Min' = c(summary(crime.change$violent.change)[[1]],
                                     summary(crime.change$burglary.change)[[1]],
                                     summary(crime.change$thefts.change)[[1]],
                                     summary(crime.change$vehicle.change)[[1]]),
                           
                           'q1' = c(summary(crime.change$violent.change)[[2]],
                                    summary(crime.change$burglary.change)[[2]],
                                    summary(crime.change$thefts.change)[[2]],
                                    summary(crime.change$vehicle.change)[[2]]),
                           
                           'Median' = c(summary(crime.change$violent.change)[[3]],
                                        summary(crime.change$burglary.change)[[3]],
                                        summary(crime.change$thefts.change)[[3]],
                                        summary(crime.change$vehicle.change)[[3]]),
                           
                           'Mean' = c(summary(crime.change$violent.change)[[4]],
                                      summary(crime.change$burglary.change)[[4]],
                                      summary(crime.change$thefts.change)[[4]],
                                      summary(crime.change$vehicle.change)[[4]]),
                           
                           'q3' = c(summary(crime.change$violent.change)[[5]],
                                    summary(crime.change$burglary.change)[[5]],
                                    summary(crime.change$thefts.change)[[5]],
                                    summary(crime.change$vehicle.change)[[5]]),
                           
                           'Max' = c(summary(crime.change$violent.change)[[6]],
                                     summary(crime.change$burglary.change)[[6]],
                                     summary(crime.change$thefts.change)[[6]],
                                     summary(crime.change$vehicle.change)[[6]]))

# reset factors to order by Measure column
change.stats$Measure <- factor(change.stats$Measure, 
                               levels = c(as.character(full.stats$Measure)))

# Convert factors to character
change.stats$Measure <- as.character(change.stats$Measure)

# create change summary table
summary.change.table <- plot_ly(
  type = 'table',
  height = 180,
  #width = 700,
  columnwidth = c(25, 15, 15, 15, 15),
  header = list(
    values = c('Measure', 'Min', '1st Q', 'Median', 'Mean', '3rd Q', 'Max'),
    line = list(width = 1, color = 'black'),
    fill = list(color = c('#1f77b4', '#1f77b4')),
    font = list(famile = 'Arial', size = 14, color = 'white')
  ),
  cells = list(
    values = rbind(change.stats$Measure, change.stats$Min, change.stats$q1, change.stats$Median, 
                   round(change.stats$Mean,1), change.stats$q3, change.stats$Max),
    align = c('center'),
    line = list(width = 1, color = 'black')
  ))

# Plot summary table
summary.change.table

# create sums by year for each of the main categories
crime.year <- setNames(aggregate(list(crime.df$Total.Violent.Crime, crime.df$Total.Burglary, 
                                      crime.df$Total.Thefts, crime.df$Motor.Vehicle.Theft), 
                                 by=crime.df['year'], sum), c('year','violent', 'burglary', 'theft', 'vehicle'))

# create new columns to track yearly changes
crime.year$violent.change = 0
crime.year$burglary.change = 0
crime.year$thefts.change = 0
crime.year$vehicle.change = 0

# create new columns to track yearly percent changes
crime.year$violent.pct = 0
crime.year$burglary.pct = 0
crime.year$thefts.pct = 0
crime.year$vehicle.pct = 0

# create a new column to track changes from month to month
# calculate the amount change between years
for (row in 1:nrow(crime.year)){
  if (is.na(crime.year$violent[row + 1]
            - crime.year$violent[row])) {
  }   
  else {
    crime.year$violent.pct[row + 1] <- ((crime.year$violent[row + 1]
                                         / crime.year$violent[row]) * 100) - 100
    crime.year$violent.change[row + 1] <- (crime.year$violent[row + 1]
                                           - crime.year$violent[row])
    
    crime.year$burglary.pct[row + 1] <- ((crime.year$burglary[row + 1]
                                          / crime.year$burglary[row]) * 100) - 100
    crime.year$burglary.change[row + 1] <- (crime.year$burglary[row + 1]
                                            - crime.year$burglary[row])
    
    crime.year$thefts.pct[row + 1] <- ((crime.year$theft[row + 1]
                                        / crime.year$theft[row])* 100) - 100
    crime.year$thefts.change[row + 1] <- (crime.year$theft[row + 1]
                                          - crime.year$theft[row])
    crime.year$vehicle.pct[row + 1] <- ((crime.year$vehicle[row + 1]
                                         / crime.year$vehicle[row]) * 100) - 100
    crime.year$vehicle.change[row + 1] <- (crime.year$vehicle[row + 1]
                                           - crime.year$vehicle[row])
  }
}

# Percentage change table from 2008 to 2018
percent.change.table <- plot_ly(
  type = 'table',
  height = 115,
  columnwidth = c(10, 10, 10, 10),
  header = list(
    values = c('Violent Crime', 'Burglary', 'Theft', 'Vehicle Theft'),
    line = list(width = 1, color = 'black'),
    fill = list(color = c('#1f77b4', '#1f77b4')),
    font = list(famile = 'Arial', size = 14, color = 'white')
  ),
  cells = list(
    values = rbind(paste(round((((tail(crime.year$violent, 1) / crime.year$violent[1]) - 1)*100), 2),'%'),
                   paste(round((((tail(crime.year$burglary, 1) / crime.year$burglary[1]) - 1)*100), 2),'%'),
                   paste(round((((tail(crime.year$theft, 1) / crime.year$theft[1]) - 1)*100), 2),'%'),
                   paste(round((((tail(crime.year$vehicle, 1) / crime.year$vehicle[1]) - 1)*100), 2),'%')),
    align = c('center'),
    line = list(width = 1, color = 'black')
  ))

# Percentage change table from 2008 to 2018
percent.change.table

# prepends 20 to each year so its the full year, 2008 instead of 08
for(row in 1:nrow(crime.year)){
  crime.year[[1]][row] <- paste('20',crime.year[[1]][row], sep='')
}

# yearly totals
yearly.totals.table <- plot_ly(
  type = 'table',
  height = 320,
  width = 700,
  header = list(
    values = c('Year','Violent Crime', 'Burglary', 'Theft', 'Vehicle Theft'),
    line = list(width = 1, color = 'black'),
    fill = list(color = c('#1f77b4', '#1f77b4')),
    font = list(famile = 'Arial', size = 14, color = 'white')
  ),
  cells = list(
    values = rbind(crime.year$year, crime.year$violent, crime.year$burglary, 
                   crime.year$theft, crime.year$vehicle),
    align = c('center'),
    line = list(width = 1, color = 'black')
  ))

# yearly totals
yearly.totals.table

# summary of change by year
change.by.year <- data.frame(rbind(summary(crime.year[-1,6]), summary(crime.year[-1,7]),
                                   summary(crime.year[-1,8]), summary(crime.year[-1,9])))

# Create column with measure names
change.by.year$measure <- c('Violent Crime', 'Burglary', 'Thefts', 'Vehicle Theft')

# summary of change by year
change.by.year.pct <- data.frame(rbind(summary(crime.year[-1,10]), summary(crime.year[-1,11]),
                                       summary(crime.year[-1,12]), summary(crime.year[-1,13])))

# Create column with measure names
change.by.year.pct$measure <- c('Violent Crime %', 'Burglary %', 'Thefts %', 'Vehicle Theft %')

# create yearly change summary table
yearly.change.table <- plot_ly(
  type = 'table',
  height = 180,
  #width = 700,
  columnwidth = c(25, 15, 15, 15, 15, 15, 15),
  header = list(
    values = c('Measure', 'Min', '1st Q', 'Median', 'Mean', '3rd Q', 'Max'),
    line = list(width = 1, color = 'black'),
    fill = list(color = c('#1f77b4', '#1f77b4')),
    font = list(famile = 'Arial', size = 14, color = 'white')
  ),
  cells = list(
    values = rbind(change.by.year$measure, round(change.by.year$Min,2), round(change.by.year$X1st.Qu.,2), 
                   round(change.by.year$Median,2),round(change.by.year$Mean,2), 
                   round(change.by.year$X3rd.Qu.,2), round(change.by.year$Max,2)),
    align = c('center'),
    line = list(width = 1, color = 'black')
  ))

# Output summary of changes by year
yearly.change.table

# create yearly change summary table
yearly.change.pct.table <- plot_ly(
  type = 'table',
  height = 180,
  #width = 700,
  columnwidth = c(25, 15, 15, 15, 15, 15, 15),
  header = list(
    values = c('Measure', 'Min', '1st Q', 'Median', 'Mean', '3rd Q', 'Max'),
    line = list(width = 1, color = 'black'),
    fill = list(color = c('#1f77b4', '#1f77b4')),
    font = list(famile = 'Arial', size = 14, color = 'white')
  ),
  cells = list(
    values = rbind(change.by.year.pct$measure, 
                   paste(round(change.by.year.pct$Min,2),'%', sep=''),
                   paste(round(change.by.year.pct$X1st.Qu.,2),'%', sep=''),
                   paste(round(change.by.year.pct$Median,2),'%', sep=''),
                   paste(round(change.by.year.pct$Mean,2),'%', sep=''),
                   paste(round(change.by.year.pct$X3rd.Qu.,2),'%', sep=''),
                   paste(round(change.by.year.pct$Max,2),'%', sep='')),
    align = c('center'),
    line = list(width = 1, color = 'black')
  ))

# Output summary of changes by year
yearly.change.pct.table

# setup correlations beteen each category
cor.matrix <- cor(crime.df[, c(2:5)])

# Plot the correlation matrix
corrplot(cor.matrix, method = 'number', type = 'lower', order = 'hclust')

# Highest correlation is between motor vehicle theft and burglary
# generate a scatter plot to observe the correlation
scatter.corr <- ggplot(data = crime.df, aes(x = Motor.Vehicle.Theft, y = Total.Burglary)) + 
  geom_point(color='blue') +
  geom_smooth(color='red', method = "lm", se = FALSE) +
  ggtitle('Correlation between Motor Vehicle Theft and Total Burglaries') +
  theme(plot.title = element_text(hjust = 0.5))

# plot scatter correlation 
scatter.corr