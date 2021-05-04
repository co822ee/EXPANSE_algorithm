# 
exc_names <- c("sta_code", "component_code", "component_caption", "obs", 
               "year", "id", "country_name", "sta_type", "area_type", "areaid", 
               "index", "nfold", "xcoord", "ycoord")
pred_c <- names(data_all1)[!names(data_all1)%in%exc_names]
neg_pred <- pred_c[grepl("nat|ugr", pred_c)]
# Ps. altitude and population and SAT are not included