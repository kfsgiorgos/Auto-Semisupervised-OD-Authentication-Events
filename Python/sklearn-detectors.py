import pandas as pd
from pyod.models.lof import LOF
from pyod.models.abod import ABOD
from pyod.models.hbos import HBOS
from pyod.models.iforest import IForest


def calculate_LOF(given_DT, given_neighbors):
  X_1 = pd.DataFrame(given_DT)
  X = X_1.values
  clf = LOF(n_neighbors=given_neighbors)
  clf.fit(X)
  X_scores = clf.decision_scores_#clf.decision_function(XX_1)
  return X_scores


import numpy as np
from sklearn import model_selection
from sklearn import svm
import time
from sklearn.model_selection import GridSearchCV

def OCSVM_train_test_AllSubspaces(My_Nu, My_Gamma, My_Kernel, given_Encoded_data, Association_string, NumberReportedOutliers_string):
   Encoded_data = pd.DataFrame(given_Encoded_data)
   print(Encoded_data)
   MaliciousEncoded1 = Encoded_data[Encoded_data['Predicted_Label_allSubspaces' == "Malicious"]]
   MaliciousEncoded = MaliciousEncoded1.drop(['Predicted_Label_allSubspaces'], axis=1)
   NormalEncoded1 = Encoded_data[Encoded_data['Predicted_Label_allSubspaces' == "Normal"]]
   NormalEncoded = NormalEncoded1.drop(['Predicted_Label_allSubspaces'], axis=1)
   print(NormalEncoded)
   X_train = NormalEncoded
   X_train.shape
   X_test = MaliciousEncoded
   clf1 = svm.OneClassSVM(random_state=0, nu=My_Nu, kernel=My_Kernel, gamma = My_Gamma)    
   start1 = time.time()
   print(start1)
   time.localtime(time.time())
   clf1.fit(X_train)
   end1 = time.time()
   print((end1 - start1)/60.)
   y_pred_test = pd.DataFrame(clf1.decision_function(X_test))
   
   stringtosave1 = # your_path
   stringtosave2 = str(My_Nu)
   stringtosave3 = str(My_Kernel)
   stringtosave4 = str(My_Gamma)
   stringtosave5 = str(NumberReportedOutliers_string) + "_Outliers_"
   stringtosave6 = str(Association_string) + "_.csv"
   y_pred_test.to_csv(stringtosave1 + stringtosave2 + stringtosave3 + stringtosave4 + stringtosave5 + stringtosave6)
   print(" End of this Iteration")
   x = "END"
   return x




def OCSVM_train_test_900PCs(My_Nu, My_Gamma, My_Kernel, Association_string, NumberReportedOutliers_string, Normaldata_input, Maliciousdata_input):

   MaliciousEncoded = pd.DataFrame(Maliciousdata_input)
   print("1")
   NormalEncoded = pd.DataFrame(Normaldata_input)
   print("2")
   
   X_train = NormalEncoded
   X_train.shape
   X_test = MaliciousEncoded
   print("3")
   clf1 = svm.OneClassSVM(random_state=0, nu=My_Nu, kernel=My_Kernel, gamma = My_Gamma)    
   start1 = time.time()
   print(start1)
   time.localtime(time.time())
   clf1.fit(X_train)
   end1 = time.time()
   print((end1 - start1)/60.)
   y_pred_test = pd.DataFrame(clf1.decision_function(X_test))
   stringtosave1 = # your_path
   stringtosave2 = str(My_Nu)
   stringtosave3 = str(My_Kernel)
   stringtosave4 = str(My_Gamma)
   stringtosave5 = str(NumberReportedOutliers_string) + "_Outliers_"
   stringtosave6 = str(Association_string) + "_.csv"
   # y_pred_test.to_csv(stringtosave1 + stringtosave2 + stringtosave3 + stringtosave4 + stringtosave5 + stringtosave6)
   print(" End of this Iteration")
   return y_pred_test


