# Data set Description
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

##For each record it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

##The dataset includes the following files:

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.

- 'activity_labels.txt': Links the class labels with their activity name.

- 'train/X_train.txt': Training set.

- 'train/y_train.txt': Training labels.

- 'test/X_test.txt': Test set.

- 'test/y_test.txt': Test labels.

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.
 
- 'test/subject_test.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.

# Variables in run_analysis.R
extracted.features :  columns that we need for the analysis

extracted.features.names :   Names of the columns in the generated data set 

#Functions in run_analysis.R
## Function download_data
The purpose of this function is downloading the dataset from the web and unzip, if it haven't existed yet.

fileUrl : It is the URL where the data set is located

## Function merge_test_train
The purpose of this function is the generation of the data set with the features data, activity data and subject data for the data test set and data train set.
Later on join both datasets in clean.data that it is sent as result.

Note: The activity information, subject information is added as columns in the corresponding data set.

## Function extract_mean_std
The purpose of this function is to extract the data that we need and giving the names of each column in the data set
This information are in the variables extracted.features and extracted.features.names, that are declared at the beginning of run_analysis.R

##Function activity_names
This function read the activity labels file and enrich the information of the activities description, making a join by the activity_id field in the data frame activity_labels and the data frame with all the data (datos)

#Steps
1.- download the data (download_data())
2.- Merge the test and train data sets (merge_test_train()) as store in testtrain.data
3.- Extract only the measurements of mean and std, activity and subject ids.
4.- Enrich the data activity in the generated data set with the activity description and store in act.data 
5.- Generate a new column id with the activity_id, activity_name and subject_id that it is going to help when later on, we group the activities
6.- Calculate the mean of each variable with the activity_id, activity_name and subject_id and store in datos.final
7.- Write in cleaned_data.txt the result


## Steps in the ra
