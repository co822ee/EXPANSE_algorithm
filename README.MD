
## Single-year modelling
Each model was built for each year using observations from that year only. 
* o_01_model_slr_gwr_rf.R builds five models for each algorithm (SLR, GWR, RF) for each year using 5-fold CV data stratified by station type and climate zone type. The model performance were then evaluated using o_01_output_prediction_test_cv.R and 02_o_singleyr_5fold.Rmd.
* o_02_model_all.R builds one model for each algorithm for each year using all observations available in that year.
* o_03_vis_randomPoints.R visualizes the prediction values obtained at random points from GEE. 

Functions:
* fun_call_lib.R calls the R packages necessary for running the modelling. 
* fun_slr_for.R
* fun_output_slr_result.R
* fun_gen_pred_df.R
* fun_setupt_gwr.R
* fun_calibr_gwr.R
* fun_output_gwr_result.R
* fun_gen_df_gwr.R
* fun_plot_gwr_coef.R
* fun_tune_rf.R
* fun_gwr.R
* fun_opt_rf.R
* fun_plot_rf_vi.R



Geographically weighted regression: 
* 00_gwr_01_stepwise.R tests the stepwise procedure in GWR. 
* 00_gwr_02_stepwise_mixedGWR tests the stepwise procedure with mixed GWR used (some predictor surfaces are globally fixed).

