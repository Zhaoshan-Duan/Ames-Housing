
#
# Prediction and Evaluation Script 
#
## transform Sale_Price
ames9 <- ames8 %>% 
  mutate(Sale_Price = log10(Sale_Price)) %>% 
  mutate(Year_Built = as.numeric(Year_Built), Year_Remod_Add = as.numeric(Year_Remod_Add))

library(tidymodels)

## set em seeds
set.seed(666)

## 80/20 
ames_split <- initial_split(ames9, prop = 0.80)
ames_split

ames_train <- training(ames_split)
ames_test  <-  testing(ames_split)

mlr_recipe <- recipe(Sale_Price ~ Total_Area + Price_Total_Per_Ft + 
                       TotRms_AbvGrd_15 + Bsmt_Qual_Excellent + BsmtFin_Type_2_No_Basement + 
                       Garage_Cars_3 + Neighborhood_Northridge + TotRms_AbvGrd_12 + 
                       Fireplaces_2 + Overall_Qual_Very_Excellent + Overall_Qual_Excellent + 
                       Full_Bath_3 + TotRms_AbvGrd_2 + Lot_Shape_Irregular + Half_Bath_1 + 
                       Price_Liv_Per_Ft + Total_Bsmt_SF + House_Style_SFoyer + Bldg_Type_TwnhsE + 
                       Bedroom_AbvGr_8 + Garage_Area + Overall_Qual_Average + TotRms_AbvGrd_4 + 
                       TotRms_AbvGrd_3 + MS_Zoning_Residential_Low_Density + Bedroom_AbvGr_5 + 
                       Year_Remod_Add + TotRms_AbvGrd_5 + Neighborhood_Meadow_Village + 
                       Overall_Qual_Poor + Bsmt_Qual_Fair + Exter_Cond_Fair + TotRms_AbvGrd_11 + 
                       Overall_Qual_Fair + Remodeled + Mas_Vnr_Area + Full_Bath_1 + 
                       House_Style_One_and_Half_Fin + Overall_Qual_Below_Average + 
                       Neighborhood_Green_Hills + Sale_Condition_Abnorml + Neighborhood_Northpark_Villa + 
                       Overall_Cond_Fair + Bedroom_AbvGr_3 + TotRms_AbvGrd_7 + Mas_Vnr_Type_None + 
                       Neighborhood_Briardale + Roof_Style_Flat + Full_Bath_4 + 
                       Exterior_1st_ImStucc+Gr_Liv_Area, data=ames_train) %>% 
  step_log(Gr_Liv_Area, base = 10)

lasso_recipe <- recipe(Sale_Price ~ Year_Built+Year_Remod_Add+Gr_Liv_Area+Garage_Area+Total_Area+Price_Total_Per_Ft+Price_Liv_Per_Ft+MS_Zoning_Residential_Low_Density+MS_Zoning_Residential_Medium_Density+
                         Exter_Qual_Good+Exter_Cond_Fair+Bsmt_Qual_Good+Bsmt_Qual_No_Basement+BsmtFin_Type_2_No_Basement+Heating_QC_Excellent+Kitchen_Qual_Good+
                         Garage_Type_Attchd+Garage_Type_No_Garage+Sale_Condition_Abnorml+Overall_Qual_Good+Overall_Qual_Below_Average+Overall_Qual_Fair+Overall_Qual_Poor+
                         Overall_Qual_Very_Excellent+Overall_Cond_Poor+Overall_Cond_Fair+Overall_Cond_Very_Poor+Full_Bath_1+Full_Bath_2+Half_Bath_0+Half_Bath_1+Bedroom_AbvGr_2+
                         TotRms_AbvGrd_7+TotRms_AbvGrd_5+TotRms_AbvGrd_4+TotRms_AbvGrd_12+TotRms_AbvGrd_3+TotRms_AbvGrd_2+TotRms_AbvGrd_15+Fireplaces_0+Garage_Cars_2+Garage_Cars_1,
                       data=ames_train) %>% 
  step_log(Gr_Liv_Area, base = 10) %>% 
  step_normalize(all_numeric(), -all_outcomes()) %>%
  step_zv(all_numeric(), -all_outcomes())

lm_model <- linear_reg() %>% set_engine("lm") %>% set_mode("regression")
lasso_model <- linear_reg(penalty= 0.5, mixture = 1) %>% set_engine("glmnet")
  
## Work flow 
lm_wflow <- 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(mlr_recipe)

lasso_wflow <- 
  workflow() %>% 
  add_model(lasso_model) %>% 
  add_recipe(lasso_recipe)


## fit on training 
lm_fit <- fit(lm_wflow, ames_train)
lasso_fit <- fit(lasso_wflow, ames_train)

## Prediction 
lm_test_res <- predict(lm_fit, new_data = ames_test %>% select(-Sale_Price))
lm_test_res <- bind_cols(lm_test_res, ames_test %>% select(Sale_Price))

lasso_test_res <- predict(lasso_fit, new_data = ames_test %>% select(-Sale_Price))
lasso_test_res <- bind_cols(lasso_test_res, ames_test %>% select(Sale_Price))

ames_metrics <- metric_set(rmse, rsq, mae)
ames_metrics(lm_test_res, truth = Sale_Price, estimate = .pred)
ames_metrics(lasso_test_res, truth = Sale_Price, estimate = .pred)

