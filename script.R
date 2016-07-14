#Setup
# first run: install.packages("MASS")
#            install.packages("ISLR")
library(MASS)
library(ISLR)

#USE THESE LINES TO SAVE/LOAD DATA FRAMES
#save(train,file="DigitRecognition/train.Rda")
#load("DigitRecognition/train.Rda")
#save(test,file="DigitRecognition/test.Rda")
#load("DigitRecognition/test.Rda")

#Load table
#pixels range from (0 to 783)
train <- read.table("DigitRecognition/train.csv",header=TRUE,sep=",")



#Feature ideas:
#First find the max height point, lowest point, furthest right, and furthest left
#4 helper fields)Use those values 0 to 27 as four features
#The new box is the square using the 4 points previously mentioned
#Use the new box for all other features

#1 feature) % non-black points (value other than 0)
#13 more features) is point non-black?  The 13 points are
#-   -   -
#  -   -
#-   -   -
#  -   -
#-   -   -
#NEXT TWO FEATURES: width and height
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

###First function
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

# FIRST FEATURE:
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
#  m m m       -> midCOl1, midCol2, midCol3
#-   -   - top
#  -   -   midRow1
#-   -   - midRow2
#  -   -   midRow3
#-   -   - bottom

#First we find the middle midRow1, midRow2, midRow3, midCol1, midCol2, midCol3 for each observation
#FIND THE MIDPOINT OF START AND END (ie. left & right or top & bottom)
#THEN FIND THE MIDPOINT OF THAT MIDPOINT AND A SIDE (ie. midpoint & left)
findMidpoint <- function(df, observation, start, end){
  return(floor(0.5 * (start + end)))
}
#ADDING THE MID ROWS:
train$midRow2 <- -1
for (i in 1:observations) {
  train$midRow2[i] <- findMidpoint(train,i,train$top[i],train$bottom[i])
}
train$midRow1 <- -1
for (i in 1:observations) {
  train$midRow1[i] <- findMidpoint(train,i,train$top[i],train$midRow2[i])
}
train$midRow3 <- -1
for (i in 1:observations) {
  train$midRow3[i] <- findMidpoint(train,i,train$midRow2[i],train$bottom[i])
}
#ADDING THE MID COLUMNS:
train$midCol2 <- -1
for (i in 1:observations) {
  train$midCol2[i] <- findMidpoint(train,i,train$left[i],train$right[i])
}
train$midCol1 <- -1
for (i in 1:observations) {
  train$midCol1[i] <- findMidpoint(train,i,train$left[i],train$midCol2[i])
}
train$midCol3 <- -1
for (i in 1:observations) {
  train$midCol3[i] <- findMidpoint(train,i,train$midCol2[i],train$right[i])
}

#Determine if these 13 points are nonBlack.  The 13 new features are true or false (1 or 0)
#pointOfInterest1IsNonBlack, pointOfInterest2IsNonBlack, pointOfInterest3IsNonBlack, ... pointOfInterest13IsNonBlack. 
#-   -   - 
#  -   -   
#-   -   - 
#  -   -   
#-   -   - 
pointIsNonBlackColor <- function(df, observation, row, col){
  pixel <- col + row * 28
  dfCol <- pixel + 2  #the column this pixel is stored.
  if (df[observation,dfCol] != 0) {
    return(1)
  }
  else {
    return(0)
  }
}
#13 Points of interest
train$pointOfInterest1IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest1IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$top[i], train$left[i])
}
train$pointOfInterest2IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest2IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$top[i], train$midCol2[i])
}
train$pointOfInterest3IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest3IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$top[i], train$right[i])
}
train$pointOfInterest4IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest4IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$midRow1[i], train$midCol1[i])
}
train$pointOfInterest5IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest5IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$midRow1[i], train$midCol3[i])
}
train$pointOfInterest6IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest6IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$midRow2[i], train$left[i])
}
train$pointOfInterest7IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest7IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$midRow2[i], train$midCol2[i])
}
train$pointOfInterest8IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest8IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$midRow2[i], train$right[i])
}
train$pointOfInterest9IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest9IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$midRow3[i], train$midCol1[i])
}
train$pointOfInterest10IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest10IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$midRow3[i], train$midCol3[i])
}
train$pointOfInterest11IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest11IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$bottom[i], train$left[i])
}
train$pointOfInterest12IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest12IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$bottom[i], train$midCol2[i])
}
train$pointOfInterest13IsNonBlack <- -1
for (i in 1:observations) {
  train$pointOfInterest13IsNonBlack[i] <- pointIsNonBlackColor(train, i, train$bottom[i], train$right[i])
}

#NEXT 2 FEATURES, WIDTH AND HEIGHT
train$width <- -1
for (i in 1:observations) {
  train$width[i] <- 1 + train$right[i] - train$left[i]
}
train$height <- -1
for (i in 1:observations) {
  train$height[i] <- 1 + train$bottom[i] - train$top[i]
}

#LAST 3 FEATURES
#How many times does following a path
#get you newly white spaces? The three features are created by
#using 3 horizontal lines on lines 2-4
#using 3 vertical lines on lines 2-4
#using a X
generateListOfDFColsOnPath3HorizontalLines <- function(midRow1, midRow2, midRow3){
  pixels <- c( seq(midRow1 * 28, 27 + midRow1 * 28),
               seq(midRow2 * 28, 27 + midRow2 * 28),
               seq(midRow3 * 28, 27 + midRow3 * 28))
  dfCols <- pixels + 2 #the column this pixel is stored.
  return(dfCols)
}
generateListOfDFColsOnPath3VerticalLines <- function(midCol1, midCol2, midCol3){
  pixels <- c( seq(midCol1,783,28),
               seq(midCol2,783,28),
               seq(midCol3,783,28))
  dfCols <- pixels + 2 #the column this pixel is stored.
  return(dfCols)
}
#This function uses a slope of magnitude 1 and starts at the bottom left 
# and goes up-right and starts top left and goes down-right
# The cross stops before row 0 (at row 1) for the upward cross line
generateListOfDFColsOnPathCrossX <- function(bottom, left, top){
  pixels <- c( seq(bottom * 28 + left, 28, -27),
               seq(top * 28 + left, 783, 29))
  dfCols <- pixels + 2 #the column this pixel is stored.
  return(dfCols)
}

#Find the count of when the path enters a nonblack pixel, when the previous 
#pixel was black.
#dfCols is generated by the 3 functions above.  It is an array of the column
#locations of the pixels.
countNewlyNonBlacksOnPath <- function(df, observation, dfCols){
  count <- 0
  lastPixelBlack <- 1 #set as true initially
  for (pixelValue in df[observation,dfCols]) {
    if (pixelValue != 0 && lastPixelBlack == 1) {
      count = count + 1
    } 
    if (pixelValue == 0) {
      lastPixelBlack = 1
    } else {
      lastPixelBlack = 0
    }
  }
  return(count)
}
##THE LAST 3 FEATURES ARE ADDED
train$countNewlyNonBlackOn3HorizontalLinesPath <- -1
for (i in 1:observations) {
  dfCols <- generateListOfDFColsOnPath3HorizontalLines(train$midRow1[i], train$midRow2[i], train$midRow3[i])
  train$countNewlyNonBlackOn3HorizontalLinesPath[i] <- countNewlyNonBlacksOnPath(train, i, dfCols)
}
train$countNewlyNonBlackOn3VerticalLinesPath <- -1
for (i in 1:observations) {
  dfCols <- generateListOfDFColsOnPath3VerticalLines(train$midCol1[i], train$midCol2[i], train$midCol3[i])
  train$countNewlyNonBlackOn3VerticalLinesPath[i] <- countNewlyNonBlacksOnPath(train, i, dfCols)
}
train$countNewlyNonBlackOnCrossXPath <- -1
for (i in 1:observations) {
  dfCols <- generateListOfDFColsOnPathCrossX(train$bottom[i], train$left[i], train$top[i])
  train$countNewlyNonBlackOnCrossXPath[i] <- countNewlyNonBlacksOnPath(train, i, dfCols)
}

#Adding the feature using the function
train$pctNonBlackBoxesTopHalf <- -1
for (i in 1:observations) {
  train$pctNonBlackBoxesTopHalf[i] <- pctNonBlackBoxes(train,i, train$left[i], train$right[i], train$top[i], train$midRow2[i])  
}
train$pctNonBlackBoxesBottomHalf <- -1
for (i in 1:observations) {
  train$pctNonBlackBoxesBottomHalf[i] <- pctNonBlackBoxes(train,i, train$left[i], train$right[i], train$midRow2[i], train$bottom[i])  
}
train$pctNonBlackBoxesLeftHalf <- -1
for (i in 1:observations) {
  train$pctNonBlackBoxesLeftHalf[i] <- pctNonBlackBoxes(train,i, train$left[i], train$midCol2[i], train$top[i], train$bottom[i])  
}
train$pctNonBlackBoxesRightHalf <- -1
for (i in 1:observations) {
  train$pctNonBlackBoxesRightHalf[i] <- pctNonBlackBoxes(train,i, train$midCol2[i], train$right[i], train$top[i], train$bottom[i])  
}
train$pctNonBlackBoxesTopHalfDividedByBottomHalf <- -1
for (i in 1:observations) {
  train$pctNonBlackBoxesTopHalfDividedByBottomHalf[i] <- train$pctNonBlackBoxesTopHalf[i] / train$pctNonBlackBoxesBottomHalf[i]
}
train$pctNonBlackBoxesLeftHalfDividedByRightHalf <- -1
for (i in 1:observations) {
  train$pctNonBlackBoxesLeftHalfDividedByRightHalf[i] <- train$pctNonBlackBoxesLeftHalf[i] / train$pctNonBlackBoxesRightHalf[i]
}

### SAVED OBJECT HERE #############################

####FEATURES READY:
#pctNonBlackBoxes
#pointOfInterest1IsNonBlack
#pointOfInterest2IsNonBlack
#pointOfInterest3IsNonBlack
#pointOfInterest4IsNonBlack
#pointOfInterest5IsNonBlack
#pointOfInterest6IsNonBlack
#pointOfInterest7IsNonBlack
#pointOfInterest8IsNonBlack
#pointOfInterest9IsNonBlack
#pointOfInterest10IsNonBlack
#pointOfInterest11IsNonBlack
#pointOfInterest12IsNonBlack
#pointOfInterest13IsNonBlack
#width
#height
#countNewlyNonBlackOn3HorizontalLinesPath
#countNewlyNonBlackOn3VerticalLinesPath
#countNewlyNonBlackOnCrossXPath
#pctNonBlackBoxesTopHalf
#pctNonBlackBoxesBottomHalf
#pctNonBlackBoxesLeftHalf
#pctNonBlackBoxesRightHalf
#pctNonBlackBoxesTopHalfDividedByBottomHalf
#pctNonBlackBoxesLeftHalfDividedByRightHalf


###########ALL FUNCTIONS IN ONE PLACE (BELOW)
#### THESE FUNCTIONS ARE FOUND ABOVE AS WELL
firstNonBlackRow <- function(df, observation){
  for (dfCol in 2:785 ) {
    if (df[observation,dfCol] != 0){
      pixelIdx <- dfCol-2
      return(  floor(pixelIdx / 28)  ) 
    }
  }
}
lastNonBlackRow <- function(df, observation){
  for (dfCol in 785:2 ) {
    if (df[observation,dfCol] != 0){
      pixelIdx <- dfCol-2
      return(  floor(pixelIdx / 28)  )
    }
  }
}
firstNonBlackColumn <- function(df, observation){
  for (i in 0:27){
    for (dfCol in seq(2+i,785,28) ) {
      if (df[observation,dfCol] != 0){
        return(i)
      }
    }
  }
}
lastNonBlackColumn <- function(df, observation){
  for (i in 27:0){
    for (dfCol in seq(29-i,785,28) ) {
      if (df[observation,dfCol] != 0){
        return(i)
      }
    }
  }
}
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
findMidpoint <- function(df, observation, start, end){
  return(floor(0.5 * (start + end)))
}
pointIsNonBlackColor <- function(df, observation, row, col){
  pixel <- col + row * 28
  dfCol <- pixel + 2  #the column this pixel is stored.
  if (df[observation,dfCol] != 0) {
    return(1)
  }
  else {
    return(0)
  }
}
generateListOfDFColsOnPath3HorizontalLines <- function(midRow1, midRow2, midRow3){
  pixels <- c( seq(midRow1 * 28, 27 + midRow1 * 28),
               seq(midRow2 * 28, 27 + midRow2 * 28),
               seq(midRow3 * 28, 27 + midRow3 * 28))
  dfCols <- pixels + 2 #the column this pixel is stored.
  return(dfCols)
}
generateListOfDFColsOnPath3VerticalLines <- function(midCol1, midCol2, midCol3){
  pixels <- c( seq(midCol1,783,28),
               seq(midCol2,783,28),
               seq(midCol3,783,28))
  dfCols <- pixels + 2 #the column this pixel is stored.
  return(dfCols)
}
generateListOfDFColsOnPathCrossX <- function(bottom, left, top){
  pixels <- c( seq(bottom * 28 + left, 28, -27),
               seq(top * 28 + left, 783, 29))
  dfCols <- pixels + 2 #the column this pixel is stored.
  return(dfCols)
}
countNewlyNonBlacksOnPath <- function(df, observation, dfCols){
  count <- 0
  lastPixelBlack <- 1 #set as true initially
  for (pixelValue in df[observation,dfCols]) {
    if (pixelValue != 0 && lastPixelBlack == 1) {
      count = count + 1
    } 
    if (pixelValue == 0) {
      lastPixelBlack = 1
    } else {
      lastPixelBlack = 0
    }
  }
  return(count)
}
###########ALL FUNCTIONS IN ONE PLACE (ABOVE)

####LDA#############
#Can try QDA next after getting test error on the website

#### INITIAL FIT
lda.fit=lda(label ~ pctNonBlackBoxes + pointOfInterest1IsNonBlack + pointOfInterest2IsNonBlack +
              pointOfInterest3IsNonBlack + pointOfInterest4IsNonBlack + pointOfInterest5IsNonBlack +
              pointOfInterest6IsNonBlack + pointOfInterest7IsNonBlack + pointOfInterest8IsNonBlack +
              pointOfInterest9IsNonBlack + pointOfInterest10IsNonBlack + pointOfInterest11IsNonBlack +
              pointOfInterest12IsNonBlack + pointOfInterest13IsNonBlack + width + height +
              countNewlyNonBlackOn3HorizontalLinesPath + countNewlyNonBlackOn3VerticalLinesPath +
              countNewlyNonBlackOnCrossXPath + pctNonBlackBoxesTopHalf + pctNonBlackBoxesBottomHalf +
              pctNonBlackBoxesLeftHalf + pctNonBlackBoxesRightHalf +
              pctNonBlackBoxesTopHalfDividedByBottomHalf +
              pctNonBlackBoxesLeftHalfDividedByRightHalf,data=train,CV=FALSE)

qda.fit=qda(label ~ pctNonBlackBoxes + pointOfInterest1IsNonBlack + pointOfInterest2IsNonBlack +
              pointOfInterest3IsNonBlack + pointOfInterest4IsNonBlack + pointOfInterest5IsNonBlack +
              pointOfInterest6IsNonBlack + pointOfInterest7IsNonBlack + pointOfInterest8IsNonBlack +
              pointOfInterest9IsNonBlack + pointOfInterest10IsNonBlack + pointOfInterest11IsNonBlack +
              pointOfInterest12IsNonBlack + pointOfInterest13IsNonBlack + width + height +
              countNewlyNonBlackOn3HorizontalLinesPath + countNewlyNonBlackOn3VerticalLinesPath +
              countNewlyNonBlackOnCrossXPath + pctNonBlackBoxesTopHalf + pctNonBlackBoxesBottomHalf +
              pctNonBlackBoxesLeftHalf + pctNonBlackBoxesRightHalf +
              pctNonBlackBoxesTopHalfDividedByBottomHalf +
              pctNonBlackBoxesLeftHalfDividedByRightHalf,data=train,CV=FALSE)

#GET TEST DATA READY BY LOADING FEATURES
temp <- read.table("DigitRecognition/test.csv",header=TRUE,sep=",")
test = data.frame(0,temp) ## The feature building requires a column in the first location
###########GET ALL FEATURES FOR TEST SET (BELOW)
# Set up the first four features
observations <- dim(test)[1]
#left
test$left <- -1
for (i in 1:observations) {
  test$left[i] <- firstNonBlackColumn(test,i)  
}
#right
test$right <- -1
for (i in 1:observations) {
  test$right[i] <- lastNonBlackColumn(test,i)  
}
#top
test$top <- -1
for (i in 1:observations) {
  test$top[i] <- firstNonBlackRow(test,i)
}
#bottom
test$bottom <- -1
for (i in 1:observations) {
  test$bottom[i] <- lastNonBlackRow(test,i)  
}
#Adding the feature using the function
test$pctNonBlackBoxes <- -1
for (i in 1:observations) {
  test$pctNonBlackBoxes[i] <- pctNonBlackBoxes(test,i, test$left[i], test$right[i], test$top[i], test$bottom[i])  
}
#ADDING THE MID ROWS:
test$midRow2 <- -1
for (i in 1:observations) {
  test$midRow2[i] <- findMidpoint(test,i,test$top[i],test$bottom[i])
}
test$midRow1 <- -1
for (i in 1:observations) {
  test$midRow1[i] <- findMidpoint(test,i,test$top[i],test$midRow2[i])
}
test$midRow3 <- -1
for (i in 1:observations) {
  test$midRow3[i] <- findMidpoint(test,i,test$midRow2[i],test$bottom[i])
}
#ADDING THE MID COLUMNS:
test$midCol2 <- -1
for (i in 1:observations) {
  test$midCol2[i] <- findMidpoint(test,i,test$left[i],test$right[i])
}
test$midCol1 <- -1
for (i in 1:observations) {
  test$midCol1[i] <- findMidpoint(test,i,test$left[i],test$midCol2[i])
}
test$midCol3 <- -1
for (i in 1:observations) {
  test$midCol3[i] <- findMidpoint(test,i,test$midCol2[i],test$right[i])
}
#13 Points of interest
test$pointOfInterest1IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest1IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$top[i], test$left[i])
}
test$pointOfInterest2IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest2IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$top[i], test$midCol2[i])
}
test$pointOfInterest3IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest3IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$top[i], test$right[i])
}
test$pointOfInterest4IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest4IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$midRow1[i], test$midCol1[i])
}
test$pointOfInterest5IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest5IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$midRow1[i], test$midCol3[i])
}
test$pointOfInterest6IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest6IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$midRow2[i], test$left[i])
}
test$pointOfInterest7IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest7IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$midRow2[i], test$midCol2[i])
}
test$pointOfInterest8IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest8IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$midRow2[i], test$right[i])
}
test$pointOfInterest9IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest9IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$midRow3[i], test$midCol1[i])
}
test$pointOfInterest10IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest10IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$midRow3[i], test$midCol3[i])
}
test$pointOfInterest11IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest11IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$bottom[i], test$left[i])
}
test$pointOfInterest12IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest12IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$bottom[i], test$midCol2[i])
}
test$pointOfInterest13IsNonBlack <- -1
for (i in 1:observations) {
  test$pointOfInterest13IsNonBlack[i] <- pointIsNonBlackColor(test, i, test$bottom[i], test$right[i])
}
test$width <- -1
for (i in 1:observations) {
  test$width[i] <- 1 + test$right[i] - test$left[i]
}
test$height <- -1
for (i in 1:observations) {
  test$height[i] <- 1 + test$bottom[i] - test$top[i]
}
test$countNewlyNonBlackOn3HorizontalLinesPath <- -1
for (i in 1:observations) {
  dfCols <- generateListOfDFColsOnPath3HorizontalLines(test$midRow1[i], test$midRow2[i], test$midRow3[i])
  test$countNewlyNonBlackOn3HorizontalLinesPath[i] <- countNewlyNonBlacksOnPath(test, i, dfCols)
}
test$countNewlyNonBlackOn3VerticalLinesPath <- -1
for (i in 1:observations) {
  dfCols <- generateListOfDFColsOnPath3VerticalLines(test$midCol1[i], test$midCol2[i], test$midCol3[i])
  test$countNewlyNonBlackOn3VerticalLinesPath[i] <- countNewlyNonBlacksOnPath(test, i, dfCols)
}
test$countNewlyNonBlackOnCrossXPath <- -1
for (i in 1:observations) {
  dfCols <- generateListOfDFColsOnPathCrossX(test$bottom[i], test$left[i], test$top[i])
  test$countNewlyNonBlackOnCrossXPath[i] <- countNewlyNonBlacksOnPath(test, i, dfCols)
}
test$pctNonBlackBoxesTopHalf <- -1
for (i in 1:observations) {
  test$pctNonBlackBoxesTopHalf[i] <- pctNonBlackBoxes(test,i, test$left[i], test$right[i], test$top[i], test$midRow2[i])  
}
test$pctNonBlackBoxesBottomHalf <- -1
for (i in 1:observations) {
  test$pctNonBlackBoxesBottomHalf[i] <- pctNonBlackBoxes(test,i, test$left[i], test$right[i], test$midRow2[i], test$bottom[i])  
}
test$pctNonBlackBoxesLeftHalf <- -1
for (i in 1:observations) {
  test$pctNonBlackBoxesLeftHalf[i] <- pctNonBlackBoxes(test,i, test$left[i], test$midCol2[i], test$top[i], test$bottom[i])  
}
test$pctNonBlackBoxesRightHalf <- -1
for (i in 1:observations) {
  test$pctNonBlackBoxesRightHalf[i] <- pctNonBlackBoxes(test,i, test$midCol2[i], test$right[i], test$top[i], test$bottom[i])  
}
test$pctNonBlackBoxesTopHalfDividedByBottomHalf <- -1
for (i in 1:observations) {
  test$pctNonBlackBoxesTopHalfDividedByBottomHalf[i] <- test$pctNonBlackBoxesTopHalf[i] / test$pctNonBlackBoxesBottomHalf[i]
}
test$pctNonBlackBoxesLeftHalfDividedByRightHalf <- -1
for (i in 1:observations) {
  test$pctNonBlackBoxesLeftHalfDividedByRightHalf[i] <- test$pctNonBlackBoxesLeftHalf[i] / test$pctNonBlackBoxesRightHalf[i]
}

###########GET ALL FEATURES FOR TEST SET (ABOVE)

lda.pred=predict(lda.fit, test)
lda.class=lda.pred$class
output <- data.frame(1:28000,lda.class)
names(output) <- c("ImageId","Label")
write.table(output, file="DigitRecognition/submission.csv",sep=",", row.names=TRUE, col.names=FALSE)

qda.pred=predict(qda.fit, test)
qda.class=qda.pred$class
output2 <- data.frame(1:28000,qda.class)
names(output2) <- c("ImageId","Label")
write.table(output2, file="DigitRecognition/submission2.csv",sep=",", row.names=TRUE, col.names=FALSE)
