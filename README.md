README
=========================
Coursera DataScience Specialisation Track

Getting and Cleaning Data

Course Project

Alfred Anand      20 May 2014

Synopsis
--------
The purpose of this project is to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. 

The data used is data collected from the accelerometers from the Samsung Galaxy S smartphone. 

A full description is available at the site where the data was obtained: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Data for the project is available here: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

The primary script is run_analysis.R, which does the following:

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive activity names. 
5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

There will be 2 output files:

1. tidydata1.csv - Full set of observations which are means and standard deviations
2. tidydata2.csv - Means and standard deviations averaged per subject and activity

For details of the data definition, please refer to CodeBook.md

Data Processing - A Guide to run_analysis.R
---------------
The data is processed using the following steps:

- Dataset file is downloaded and unzipped

- The following data files are loaded to a data frame of the same name:
      - features - strings in column 2 are changed to human-read form using gsub
      - activity_labels - strings in column 2 are changed to human-read form using gsub
      - subject_train
      - X_train
      - y_train
      - subject_test
      - X_test
      - y_test
      
- Test and training data are merged as follows:
      - Test and training dataframes (X_test and X_train) are appended with a column Source to indicate if they are test or training data
      - Subject_train and y_train are appended as columns to test and training data X_train. Similarly for test X_test
      - Second columns of features data frame is converted to char
      - 3 rows are added to features, to cater for the new columns created in X_train and X_test
      - New data frame X is created by merge X_train and X-test, and its column names are taken from the second column of the features dataset

- Measurements on the mean and standard deviation for each measurement are extracted as follows:
      - SubX data frame is created from data frame X for measurements of mean and standard deviations, by using the indexing and grep capabilities. Therefore, any column name in X with the string "Mean" or "StandardDeviation" in it will be included in SubX
      - SubX data frame is appended with the last 3 columns of X, which shows Source, Subject and Activity
      
- Descriptive activity names to name the activities in the data set are set & Label the data set appropriately with descriptive activity names

      - The activity column values in the SubX dataframe is replaced with values lookedup from the activity_labels dataset. (Lookup column 1 and replace with column 2)     
      - Copy SubX data frame to tidydata1 dataframe, and write it out to a .csv of the same name

- Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
      - A new data frame tidydata2 is derived by aggregating column 1 of tidydata1 data frame by subject and activity. This new data frame will have 3 columns at this point
      - A for loop appends columns to tidydata2 by aggregating columns 2 onwards of data frame tidydata1
      - tidydata2 dataframe is writen out to a .csv of the same name
      
Information on data set
----------------------
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

The dataset includes the following files:


- 'README.txt'
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

Notes: 

- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License:
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

### End of Document