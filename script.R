#Setup


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
maxTop <- function(df, row){
  for (col in 2:785 ) {
    if (df[row,col] != 0){
      return(  floor( (col-2) / 28)  )
    }
  }
}

maxTop(train,1)
