#train <- read.csv("train.csv", stringsAsFactors=FALSE)

# Apply Holiday
combi$Holiday <- 'None'
combi$Holiday[combi$Date == as.Date('2010-02-12') | combi$Date == as.Date('2011-02-11') | 
                combi$Date == as.Date('2012-02-10') | combi$Date == as.Date('2013-02-08')] <- 'Super Bowl'
combi$Holiday[combi$Date == as.Date('2010-09-10') | combi$Date == as.Date('2011-09-09') | 
                combi$Date == as.Date('2012-09-07') | combi$Date == as.Date('2013-09-06')] <- 'Labor Day'
combi$Holiday[combi$Date == as.Date('2010-11-26') | combi$Date == as.Date('2011-11-25') | 
                combi$Date == as.Date('2012-11-23') | combi$Date == as.Date('2013-11-29')] <- 'Thanksgiving'
combi$Holiday[combi$Date == as.Date('2010-12-31') | combi$Date == as.Date('2011-12-30') | 
                combi$Date == as.Date('2012-12-28') | combi$Date == as.Date('2013-12-27')] <- 'Xmas'

                                                                                              #Easter?
combi$Holiday <- as.factor(combi$Holiday)
combi$IsHoliday <- NULL
