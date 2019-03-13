library(raster)
library(rgdal)
library(rgeos)

##########################
## Load and Clean data. ##
##########################

# load test data Windows
grizzly.locations <- readOGR("D:/Bucket_Data_TEST", "G175_2017")
variable.data <- readOGR("D:/Bucket_Data_TEST", "o05_Mines_Clipped")

# load test data MAC
grizzly.locations <- readOGR("/Volumes/HENRY/CSC_497/Bucket_Data_TEST", "Grizzly_Locations")
variable.data <- readOGR("/Volumes/HENRY/CSC_497/Bucket_Data_TEST", "o05_Mines_Clipped")

# reproject data
new.grizzly.data <- spTransform(grizzly.locations, crs(variable.data))

# remove all columns not needed
clean.grizzly.data <- new.grizzly.data[, c(2,5,6,7)]

# sort by date
sorted.data <- clean.grizzly.data[order(as.Date(clean.grizzly.data$loc_date, format="%Y/%m/%d")),]

# remove N/A's
sorted.data2 <- sorted.data[!is.na(sorted.data$loc_date),]

# placeholder for the distance values
global.distance <- matrix(nrow = 365, ncol = 2)


#######################
## Distance function ##
#######################

distance_function <- function(one.day.grizzly.data){
  
  
  # calculate the distance from each grizzly location to each polygon (variable section)
  distance.matrix <- gDistance(one.day.grizzly.data, variable.data, byid=TRUE)
  
  distance.bucket = matrix()
  counter <- 0
  
  for (i in 1:ncol(distance.matrix)) {
    
    # create new bucket of the size of the number of grizzly bear locations
    if (counter == 0){
      distance.bucket = matrix(nrow = ncol(distance.matrix))
    }
    counter <- 1
    
    # get minimum distance (i.e. nearest variable to that grizzly bear point) and add it to the bucket
    min.distance <- min(distance.matrix[,i])
    distance.bucket[i,] <- min.distance
  }
  average.distance <- mean(distance.bucket)
}


#############################
## Time bucketing function ##
#############################

time_bucketing_function <- function(cleaned.data){
  
  # iterating variables
  counter2 <- 0
  global.distance.counter <- 1
  average.day.dist <- 0
  day.counter <- 1
  j = 1
  
  # PROGRESS BAR
  total <- nrow(cleaned.data)
  pb <- winProgressBar(title = "progress bar", min = 0,max = total, width = 300)
  
  for (i in 1:nrow(cleaned.data)){
    
    # PROGRESS BAR 
    Sys.sleep(0.1)
    setWinProgressBar(pb, i, title=paste( round(i/total*100, 0),"% done"))
    
    # first iteration, create point data frame & days list & empty dataframe
    if (counter2 == 0){
      point.df <- data.frame(cleaned.data)
      days <- levels(point.df$loc_date)[point.df$loc_date]
      
      # create empty df of same size to point.df
      bucket.df <- point.df
      bucket.df[,] <- matrix(ncol=ncol(point.df), rep(NA, prod(dim(point.df))))
    }
    counter2 <- 1
    
    ## end of data reached.
    if(i == nrow(cleaned.data)){
      break
    }
    
    if (days[day.counter] == days[day.counter+1]){
      # add entire row (day) data to bucket
      bucket.df[j, ] <- point.df[j, ]
      j = j + 1
      day.counter = day.counter + 1
      
    } else {
      # take the last day it was on and add it.
      bucket.df[j, ] <- point.df[j, ]
      
      # remove N/A's
      bucket.df <- bucket.df[!is.na(bucket.df$bear_ID),]
      
      # convert to spatial points data frame
      bucket.spdf <- SpatialPointsDataFrame(coords = bucket.df[,c(5,6)], bucket.df, proj4string = crs(variable.data))
      
      # call distance function
      average.day.dist <- distance_function(bucket.spdf)
      
      # add distance to global day/distance placeholder
      global.distance[global.distance.counter] <<- global.distance.counter
      global.distance[global.distance.counter, 2] <<- average.day.dist
      global.distance.counter = global.distance.counter + 1
      
      # clean bucket
      bucket.df[,] <- matrix(ncol=ncol(point.df), rep(NA, prod(dim(point.df))))
      
      # remove completed days from the 'point.df'.
      point.df <- point.df[-c(1:j), ]
      
      # reset/increment counter
      j <- 1
      day.counter = day.counter + 1
    }
    
    # if final days batch reached, perform alg on final batch
    if (day.counter == length(days)){
      # take the last day it was on and add it.
      bucket.df[j, ] <- point.df[j, ]
      
      # remove N/A's
      bucket.df <- bucket.df[!is.na(bucket.df$bear_ID),]
      
      # convert to spatial points data frame
      bucket.spdf <- SpatialPointsDataFrame(coords = bucket.df[,c(5,6)], bucket.df, proj4string = crs(variable.data))
      
      # call distance function
      average.day.dist <- distance_function(bucket.spdf)
      
      # add distance to global day/distance placeholder
      global.distance[global.distance.counter] <<- global.distance.counter
      global.distance[global.distance.counter, 2] <<- average.day.dist
      
      # clean bucket
      bucket.df[,] <- matrix(ncol=ncol(point.df), rep(NA, prod(dim(point.df))))
      
      # remove completed days from the 'point.df'.
      point.df <- point.df[-c(1:j), ]
    }
  }
  close(pb)
}




###################
## Run Script    ##
###################

time_bucketing_function(sorted.data2)


####################
## Plot Creation  ##
####################
library(tmap)
library(ggplot2)

# plot day vs distance
colnames(global.distance) <- c("day","distance")
bear.dist <- data.frame(global.distance)

# basic line plot
ggplot(data = bear.dist, aes(x = day, y = distance))+
  geom_line(color = "#00AFBB", size = 1)

ggsave("o05_mines.png", plot = last_plot(), path = "D:/Plots")

# plot grizzly data
# plot(variable.data, axes=TRUE, xlim=c(403000,406000), ylim=c(5830000,5837500))
# plot(new.grizzly.data, add=TRUE)
