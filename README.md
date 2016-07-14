# Digit-Recognizer

The Digit-Recognizer program will read an image and classify it as one of ten digits (0-9).  This program has a success rate on Kaggle's test dataset of 83.614%.

## Details
- Kaggle's test dataset has 28000 observations that need to be classified into one of ten possible categories (0-9).
- The program takes an image of 784 pixels (28x28).
- Each pixel ranges from 0 to 255 brightness.
- The Quadratic Discriminant Analysis model fit uses 25 features.

## Models submitted
- Linear Discriminant Analysis without cross-validation gave a 79.9% success rate on the test set.
- Quadratic Discriminant Analysis without cross-validation gave a 83.614% success rate on the test set.

## Files
- "script.R" holds the R code used to build the model and test the model.
- "SubmissionResultsKaggle.PNG" and "SubmissionResultsKaggle 2nd Attempt.PNG" have the Kaggle results.
- "Compressed train and test files" holds the "train.csv", "test.csv", "test.Rda" and "train.Rda" files.
- The files "train.csv" and "test.csv" were used to train and test the model respectively.  
- The files "test.Rda" and "train.Rda" are files that can be loaded into R with the features already added.

## Area of improvement for the model
- Use 5-fold cross-validation on the training set.  This would very likely give a better success rate than 83.614%.

## Feature Names:
- pctNonBlackBoxes
- pointOfInterest1IsNonBlack
- pointOfInterest2IsNonBlack
- pointOfInterest3IsNonBlack
- pointOfInterest4IsNonBlack
- pointOfInterest5IsNonBlack
- pointOfInterest6IsNonBlack
- pointOfInterest7IsNonBlack
- pointOfInterest8IsNonBlack
- pointOfInterest9IsNonBlack
- pointOfInterest10IsNonBlack
- pointOfInterest11IsNonBlack
- pointOfInterest12IsNonBlack
- pointOfInterest13IsNonBlack
- width
- height
- countNewlyNonBlackOn3HorizontalLinesPath
- countNewlyNonBlackOn3VerticalLinesPath
- countNewlyNonBlackOnCrossXPath
- pctNonBlackBoxesTopHalf
- pctNonBlackBoxesBottomHalf
- pctNonBlackBoxesLeftHalf
- pctNonBlackBoxesRightHalf
- pctNonBlackBoxesTopHalfDividedByBottomHalf
- pctNonBlackBoxesLeftHalfDividedByRightHalf