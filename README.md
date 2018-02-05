Read. Me for the run_analysis program

The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used
for later analysis.  The data was downloaded from the uci.edu archive for Human Activity Recognition Using Smartphones.
There were two data files. One for the data and one for the training data.

Activity labels and features were read into R.  

Test subject data was read into R and the variable name changed to Subject. This is necessary for the merge.
Test activities was then read into R and the labels were added to the variabes.  
Test data was read into R and variable names with mean or standard deviation were extracted into a data set called MeanStd.
The names for this dataset were edited so dashes and parentheses were removed.  The names were then added to this data set.
The subjectid's, activities and test data was merged using the cbind function.  In this step an identifier called group was added
so this data could later be identified as the test data.

This was repeated for the training data.  It's group was identified as train.
Subject, activity and grp was formatted as a factor.

The means calculated by subject, group and activity, were then calculated by using the melt and dcast functions. The final
dataset was exported using the write table function.  The output dataset was called HumanActivityMeans.txt 