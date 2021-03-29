# In the previous step, pred_c has been extracted from the pred dataframe.
neg_pred <- pred_c[grepl("nat|ugr", pred_c)]
# Ps. altitude and population and SAT are not included