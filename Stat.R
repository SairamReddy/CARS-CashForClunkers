# Question 1
library(dplyr)
library(reshape2)
library(ggplot2)
library(data.table)
library(maps)
library(zipcode)
library(ggmap)
library(randomForest)
library(caret)
setwd("~/Downloads/MISC/Edmunds")
cars <- data.table(read.csv('CARS.csv'))
cars[,c(2,3,4,5,6,10,11,12,13,17,18,21,28,29,30,31,32,33) :=NULL]
str(cars)
# cars <- merge(cars, states, by = 'state')
# cars <- subset(cars, new_vehicle_car_mileage != 0)
# cars <- subset(cars, trade_in_mileage != 0)
# cars <- subset(cars, new_vehicle_MSRP > invoice_amount)
# cars <- subset(cars, trade_in_mileage != new_vehicle_car_mileage)
cars$invoice_date <- as.Date(cars$invoice_date, format = "%d-%b-%y")
cars$sale_date <- as.Date(cars$sale_date, "%d-%b-%y")
cars$mileage_diff <- cars$trade_in_mileage - cars$new_vehicle_car_mileage
cars$mileage_diff <- -cars$mileage_diff
cars$customer_cost <- cars$new_vehicle_MSRP - cars$invoice_amount
# cars$amount_per_mile <- cars$customer_cost/cars$mileage_diff
state_summary <- summarise(group_by(cars, state), 
                           sum_invoice_amount = sum(invoice_amount), 
                           avg_invoice_amount = mean(invoice_amount), 
                           avg_trade_in_mileage = mean(trade_in_mileage), 
                           avg_trade_in_odometer_reading = mean(trade_in_odometer_reading), 
                           avg_new_vehicle_car_mileage = mean(new_vehicle_car_mileage), 
                           avg_new_vehicle_MSRP = mean(new_vehicle_MSRP), 
                           avg_mileage_diff = mean(mileage_diff), 
                           avg_customer_cost = mean(customer_cost))
head(state_summary)
nrow(state_summary)
states <- data.table(table(cars$state))
colnames(states)[1:2] <- c('state', 'vechicles')
state_summary <- merge(state_summary, states, by = 'state')
state_summary <- (state_summary[order(vechicles, decreasing = TRUE),])
ggplot(state_summary[1:10,], aes(reorder(factor(state), vechicles), 
                                 vechicles, fill = factor(state))) + geom_bar(stat = "identity") + xlab('List of States') + ylab('Count of Vehicles') + ggtitle('Top 10 States vs Vehicles Count')

ggplot(state_summary[45:55,], aes(reorder(factor(state), vechicles), 
                                  vechicles, fill = factor(state))) + geom_bar(stat = "identity") + xlab('List of States') + ylab('Count of Vehicles') + ggtitle('Least 10 States vs Vehicles Count')

# Maps
data(state)
state_map <- data.frame("state" = state.abb, "Longitude" = state.center$x,
                        "Latitude" = state.center$y, "region" = state.name)
state_map$region <- tolower(state_map$region)
map_summary <- state_summary[,c(1,6,8,10),with=FALSE]
state_map <- merge(map_summary, state_map, by = 'state')
state_map <- na.omit(state_map)
raw <- map_data("state")
head(raw)
state_map <- merge(state_map, raw, by = 'region')
p <- ggplot()
p <- p + geom_polygon(data=state_map, aes(x=long, y=lat, group = group, fill=vechicles),
                      colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
p <- p + ggtitle('Density of Transaction of Vehicles by State')

p2 <- ggplot()
p2 <- p2 + geom_polygon(data=state_map, aes(x=long, y=lat, group = group, fill=avg_mileage_diff),
                      colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
p2 <- p2 + ggtitle('Average Mileage Difference by State')

p3 <- ggplot()
p3 <- p3 + geom_polygon(data=state_map, aes(x=long, y=lat, group = group, fill=avg_new_vehicle_car_mileage),
                        colour="white") + scale_fill_continuous(low = "thistle2", high = "darkred", guide="colorbar")
p3 <- p3 + ggtitle('Average New Car Mileage by State')

# Part 2 Invoice Amount
invoice_summary <- (state_summary[order(sum_invoice_amount, decreasing = TRUE),])
ggplot(invoice_summary[1:10,], aes(reorder(factor(state), sum_invoice_amount), 
                                   sum_invoice_amount, fill = factor(state))) + geom_bar(stat = "identity") + xlab('List of States') + ylab('Total Vocher Paid') + ggtitle('Top 10 States with Voucher Amount')
ggplot(invoice_summary[45:55,], aes(reorder(factor(state), sum_invoice_amount), 
                                    sum_invoice_amount, fill = factor(state))) + geom_bar(stat = "identity") + xlab('List of States') + ylab('Total Vocher Paid') + ggtitle('Least 10 States with Voucher Amount')


# Part 3 avg_mileage_diff
mileage_summary <- data.frame(state_summary[order(avg_mileage_diff, decreasing = TRUE),])
ggplot(mileage_summary[1:10,], aes(reorder(factor(state), avg_mileage_diff), 
                                   avg_mileage_diff, fill = factor(state))) + geom_bar(stat = "identity") + geom_bar(stat = "identity") + xlab('List of States') + ylab('Average Mileage Difference') + ggtitle('Top 10 States with Max Mileage Difference')
ggplot(mileage_summary[45:55,], aes(reorder(factor(state), avg_mileage_diff), 
                                    avg_mileage_diff, fill = factor(state))) + geom_bar(stat = "identity") + geom_bar(stat = "identity") + xlab('List of States') + ylab('Average Mileage Difference') + ggtitle('Last 10 States with Min Mileage Difference')


# Part 4 involves total transactions, type of cars
category_summary <- summarise(group_by(cars, new_vehicle_category, state), 
                              sum_invoice_amount = sum(invoice_amount), 
                              avg_invoice_amount = mean(invoice_amount), 
                              avg_trade_in_mileage = mean(trade_in_mileage), 
                              avg_trade_in_odometer_reading = mean(trade_in_odometer_reading), 
                              avg_new_vehicle_car_mileage = mean(new_vehicle_car_mileage), 
                              avg_new_vehicle_MSRP = mean(new_vehicle_MSRP), 
                              avg_mileage_diff = mean(mileage_diff), 
                              avg_customer_cost = mean(customer_cost))

count <- data.table(table(cars$new_vehicle_category, cars$state))
colnames(count)[1:3] <- c('new_vehicle_category', 'state', 'count')
category_summary <- merge(category_summary, count, by = c('new_vehicle_category', 'state'))
category_summary <- (category_summary[order(state),])
category_summary <- dcast(data = category_summary, state ~ new_vehicle_category, value.var = "count", fun.aggregate = sum)
# category_summary <- transform(category_summary, rowSums(category_summary[,-1]))
colnames(category_summary)[2:5] <- c('Type1', 'Type2', 'Type3', 'TypeP')
category_summary <- melt(data = category_summary)
category_summary <- merge(category_summary, states, by = c('state'))
category_summary <- (category_summary[order(vechicles, decreasing = TRUE),])
ggplot(category_summary[1:40], aes(reorder(factor(state), value), value, fill = variable)) + geom_bar(stat = "identity") + xlab('List of States') + ylab('Total Vehicle Transactions') + ggtitle('Top 10 States with Max Transactions')
ggplot(category_summary[181:220], aes(reorder(factor(state), value), value, fill = variable)) + geom_bar(stat = "identity") + xlab('List of States') + ylab('Total Vehicle Transactions') + ggtitle('Last 10 States with Min Transactions')



# Extra Credit Time Series and Spatial
ggplot(state_summary, aes(x = factor(state), y = avg_mileage_diff, fill = vechicles, group = 1) )  + geom_point() + geom_line() + xlab('List of States') + ylab('Average Mileage Difference') + ggtitle('All States with Avg Mileage Difference')
county_summary <- summarise(group_by(cars, ZIP), sum_invoice_amount = sum(invoice_amount), 
                            avg_invoice_amount = mean(invoice_amount), 
                            avg_trade_in_mileage = mean(trade_in_mileage), 
                            avg_trade_in_odometer_reading = mean(trade_in_odometer_reading), 
                            avg_new_vehicle_car_mileage = mean(new_vehicle_car_mileage), 
                            avg_new_vehicle_MSRP = mean(new_vehicle_MSRP), 
                            avg_mileage_diff = mean(mileage_diff), 
                            avg_customer_cost = mean(customer_cost))
head(county_summary)
data(zipcode)
county_summary$ZIP <- clean.zipcodes(county_summary$ZIP)
county_summary <- merge(county_summary, zipcode, by.x ='ZIP', by.y = 'zip')
maps <- get_map(location = 'united states', zoom = 4, maptype = "terrain", source = 'google', color = 'color')
ggmap(maps) + geom_point(
  aes(x=longitude, y=latitude, show_guide = TRUE, color = avg_mileage_diff),
  data = county_summary, alpha = .5, na.rm = T) + 
  scale_color_gradient(low="yellow", high ="blue") + ggtitle('Spatial Distribution of Mileage Distribution Across ZIP')

# month <- strftime(cars$sale_date, "%m")
# year <- strftime(cars$sale_date, "%Y")
# df <- data.frame(month, year, cars$new_vehicle_MSRP)
# df.agg <- aggregate(df$cars.new_vehicle_MSRP ~ month + year, df, FUN = sum)
# df.agg$date <- as.POSIXct(paste(df.agg$year, df.agg$month, "01", sep = "-"))
# head(df.agg)




# Question 2
cars$west <- cars$state
# cars$state[cars$state == c('CA', 'OR', 'WA', 'HI', 'AL')] <-'West' 
# cars$west<-recode(cars$state,"c('CA','OR','WA', 'HI', 'AL')='West'")
# cars$west[cars$west != 'West'] <- 'Other'
levels(cars$west) <- c(levels(cars$west), "West")
levels(cars$west)[levels(cars$west) %in%  c("CA","WA","OR","HI","AL")] <- "West"
levels(cars$west)[(levels(cars$west) != "West")] <- "AZ"
box1 <- ggplot(cars, aes(factor(west), new_vehicle_car_mileage)) + geom_boxplot(aes(fill = factor(west)))
box1 <- box1 + xlab('West vs All(AZ) Comparision') + ylab('New Car Milage') + ggtitle('West vs All compared by New Vehicle Mileage')
coast_category <- summarise(group_by(cars, west, new_vehicle_category), 
                            sum_invoice_amount = sum(invoice_amount), 
                            avg_invoice_amount = mean(invoice_amount), 
                            avg_trade_in_mileage = mean(trade_in_mileage), 
                            avg_new_vehicle_car_mileage = mean(new_vehicle_car_mileage), 
                            avg_mileage_diff = mean(mileage_diff))

only_west <- subset(cars,west == 'West')
not_west <- subset(cars, west == 'AZ')
only_west <- data.frame(only_west$new_vehicle_car_mileage)
not_west <- data.frame(not_west$new_vehicle_car_mileage)
t.test(only_west,not_west)
count <- data.table(table(cars$new_vehicle_category, cars$west))
colnames(count)[1:3] <- c('new_vehicle_category', 'west', 'count')
coast_category <- merge(coast_category, count, by = c('new_vehicle_category', 'west'))
plot1 <- ggplot(coast_category, aes(reorder(factor(west), count), (count), fill = factor(new_vehicle_category))) + geom_bar(stat = "identity")
plot1 + xlab('West vs Remaining') + ylab('Total Vehicle Transactions') + ggtitle('West vs All comparision by New Vehicle Category')
plot2 <- plot1 + coord_polar(theta = "y")
plot2 + ylab('West vs Remaining') + xlab('Total Vehicle Transactions') + ggtitle('West vs All comparision by New Vehicle Category')


# Pie Charts
car <- data.frame(table(cars$new_vehicle_model, cars$west))
colnames(car)[1:3] <- c('new_vehicle_model', 'west', 'count')
car_summary <- summarise(group_by(cars, west, new_vehicle_model, new_vehicle_make), sum_invoice_amount = sum(invoice_amount), avg_invoice_amount = mean(invoice_amount), avg_trade_in_mileage = mean(trade_in_mileage), avg_trade_in_odometer_reading = mean(trade_in_odometer_reading), avg_new_vehicle_car_mileage = mean(new_vehicle_car_mileage), avg_new_vehicle_MSRP = mean(new_vehicle_MSRP), avg_mileage_diff = mean(mileage_diff), avg_customer_cost = mean(customer_cost))
car_summary <- merge(car_summary, car, by = c('new_vehicle_model','west'))
car_summary <- (car_summary[order(count, decreasing = TRUE),])
car_summary_west <- subset(car_summary, west == 'West')
car_summary_notwest <- subset(car_summary, west == 'AZ')
plot3 <- ggplot(car_summary_notwest[1:10], aes(reorder(factor(new_vehicle_make), count), 
                                               (count), fill = factor(new_vehicle_make))) + geom_bar(stat = "identity")
plot3 + xlab('Vehicle Make') + ylab('Total Vehicle Transactions') + ggtitle('Comparision of New Vehicle Make in All States but West')
plot4 <- plot3 + coord_polar("y")
plot4 + ylab('Vehicle Make') + xlab('Total Vehicle Transactions') + ggtitle('Comparision of New Vehicle Make in All States but West')
plot5 <- ggplot(car_summary_west[1:10], aes(reorder(factor(new_vehicle_make), count), (count), fill = factor(new_vehicle_make))) + geom_bar(stat = "identity")
plot5 + xlab('Vehicle Make') + ylab('Total Vehicle Transactions') + ggtitle('Comparision of New Vehicle Make in West')
plot6 <- plot5 + coord_polar(theta = "y")
plot6 + ylab('Vehicle Make') + xlab('Total Vehicle Transactions') + ggtitle('Comparision of New Vehicle Make in West')


# Question 3: Modeling Behaviour
cars$sale_year <- format(cars$sale_date, "%Y")
cars$sale_year <- as.character(cars$sale_year)
cars$sale_year <- as.numeric(cars$sale_year)
cars$age_diff <- cars$sale_year - cars$trade_in_year
cars$sale_date <- as.numeric(cars$sale_date)
cars[,c(1,2,4,5,7,11,12,13,14,17,19,20,21,22,23,24,26,27) := NULL]
str(cars)
levels(cars$state) <- c(levels(cars$state), "West")
levels(cars$state)[levels(cars$state) %in%  c("CA","WA","OR")] <- "West"

# K fold cross validation R
x <- sample(nrow(cars),0.7*nrow(cars),replace = FALSE)
train <- cars[x,]
test <- cars[-x,]
train <- na.omit(train)
test <- na.omit(test)
cars_rf <-randomForest(new_vehicle_category~.,data=train, 
                       ntree=1000, mtry=3, importance = TRUE)
cars_rf
varImpPlot(cars_rf)
pred <-predict(cars_rf, test[,-8, with = FALSE])
pred <- data.frame(pred)
ref <- data.frame(test[,8,with=FALSE])
confusionMatrix(pred$pred, ref$new_vehicle_category)



