#Setup


#USE THESE LINES TO SAVE/LOAD DATA FRAMES
#save(train,file="DigitRecognition/train.Rda")
#load("DigitRecognition/train.Rda")

#Load table
#pixels range from (0 to 783)
train <- read.table("DigitRecognition/train.csv",header=TRUE,sep=",")



#Feature ideas:
#First find the max height point, lowest point, furthest right, and furthest left
#4 features)Use those values 0 to 27 as four features
#The new box is the square using the 4 points previously mentioned
#Use the new box for all other features
#1 feature) % non-black points (value other than 0)
#13 more features) is point non-black?  The 13 points are
#-   -   -
#  -   -
#-   -   -
#  -   -
#-   -   -
#Last 3 features) How many times does following a path
#get you newly white spaces? The three features are created by
#using 3 horizontal lines on lines 2-4
#using 3 vertical lines on lines 2-4
#using a X
#You add up the number of points:
#If the first point is white then that counts as 1
#If it goes from black to white it is 1
#If it goes from white to white that is 0
#If it goes from black to black that is 0
#If it goes from white to black that is 0

########FUNCTIONS########################
#Return the row of the first non-black point. (0 to 27)
#firstNonBlackRow(train,2000)
firstNonBlackRow <- function(df, observation){
  for (dfCol in 2:785 ) {
    if (df[observation,dfCol] != 0){
      pixelIdx <- dfCol-2
      return(  floor(pixelIdx / 28)  ) 
    }
  }
}

#Return the row of the last non-black point. (0 to 27)
#lastNonBlackRow(train,2000)
lastNonBlackRow <- function(df, observation){
  for (dfCol in 785:2 ) {
    if (df[observation,dfCol] != 0){
      pixelIdx <- dfCol-2
      return(  floor(pixelIdx / 28)  )
    }
  }
}

#Return the column of the first non-black point. (0 to 27)
#firstNonBlackColumn(train,2000)
firstNonBlackColumn <- function(df, observation){
  for (i in 0:27){
    for (dfCol in seq(2+i,785,28) ) {
      if (df[observation,dfCol] != 0){
        return(i)
      }
    }
  }
}

#Return the column of the last non-black point. (0 to 27)
#lastNonBlackColumn(train,2000)
lastNonBlackColumn <- function(df, observation){
  for (i in 27:0){
    for (dfCol in seq(29-i,785,28) ) {
      if (df[observation,dfCol] != 0){
        return(i)
      }
    }
  }
}

# Set up the first four features
observations <- dim(train)[1]
#left
train$left <- -1
for (i in 1:observations) {
  train$left[i] <- firstNonBlackColumn(train,i)  
}
#right
train$right <- -1
for (i in 1:observations) {
  train$right[i] <- lastNonBlackColumn(train,i)  
}
#top
train$top <- -1
for (i in 1:observations) {
  train$top[i] <- firstNonBlackRow(train,i)
}
#bottom
train$bottom <- -1
for (i in 1:observations) {
  train$bottom[i] <- lastNonBlackRow(train,i)  
}

# NEXT FEATURE:
# This loop will go through all points of the new box
# the new box contains anything between left, right, top, bottom
# These values can be found in the dataframe's 786 to 789 columns
# To keep it from getting too messy they are passed as parameters.
pctNonBlackBoxes <- function(df, observation, left, right, top, bottom){
  nonBlackBoxes <- 0
  totalBoxes <- (1 + bottom - top) * (1 + right - left)
  for (i in left:right){
    for (dfCol in seq(28*top + 2+i,28*bottom+1,28) ) {
      if (df[observation,dfCol] != 0){
        nonBlackBoxes <- nonBlackBoxes + 1
      }
    }
  }
  return(nonBlackBoxes/totalBoxes)
}
#Adding the feature using the function
train$pctNonBlackBoxes <- -1
for (i in 1:observations) {
  train$pctNonBlackBoxes[i] <- pctNonBlackBoxes(train,i, train$left[i], train$right[i], train$top[i], train$bottom[i])  
}


#13 More features based on one pixel being nonBlack or black
#The pixels are inside the new box defined by top, bottom, left, and right
#These are the pixel locations
#-   -   -
#  -   -
#-   -   -
#  -   -
#-   -   -

