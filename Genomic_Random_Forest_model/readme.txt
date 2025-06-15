According to the classification of the samples when constructing the CNN model, the exploration cohort samples were divided into the training set and the validation set (7:3 division).
Incorporate all gene mutation data to construct the initial RF model and view the variable importance of the model. 
Sort the variables according to the MDA or MDG in the RF model importance, select the number of variables to be included according to the 5 repetitions of the ten-fold cross-validation results, and combine the two results to include the variables to construct a new RF model.

Repeat the above steps until the model is stabilized.