# 
# This script generates most of the data objects used in the report.Rmd and presentation.Rmd
#
# set up libraries ---- 
library(pacman)
p_load(AmesHousing,car,tidyverse,DataExplorer,tidyquant,dlookr,reshape2,gridExtra,skimr,lubridate,ggthemes,faraway,mctest,here)

# make data object ----
ames_original <- make_ames()

# mutate and removals ---- 
## recode MS_Zoning factor levels and covert date objects
ames_original <- ames_original %>% mutate(
  MS_Zoning = dplyr::recode(MS_Zoning, A_agr = "Agriculture", I_all = "Industrial", C_all = "Commercial"),
  Year_Built = as.Date(Year_Built), 
  Year_Remod_Add = as.Date(Year_Remod_Add),
  Year_Sold = as.Date(Year_Sold),
  Mo_Sold = as.Date(Mo_Sold),
  Longitude = as.factor(Longitude),
  Latitude = as.factor(Latitude),
  Fireplaces = as.factor(Fireplaces)
) 
## Filter out nonresidential observations
ames1 <-  ames_original %>% filter(MS_Zoning %in% c("Residential_High_Density",
                               "Residential_Low_Density",
                               "Residential_Medium_Density")) 
## Remove 10 numeric
ames2 <- ames1 %>% 
  select(-BsmtFin_SF_2,-Enclosed_Porch,-Low_Qual_Fin_SF,
         -Misc_Val,-Pool_Area,-Screen_Porch,-Three_season_porch,
         -Bsmt_Half_Bath,-Kitchen_AbvGr, -Open_Porch_SF)

## Recode count numeric and Remove 2 numeric
ames3 <- 
  ames2 %>% mutate(Garage_Cars = as.factor(Garage_Cars),
                   Full_Bath = as.factor(Full_Bath), 
                   Bedroom_AbvGr = as.factor(Bedroom_AbvGr),
                   BsmtFin_SF_1 = as.factor(BsmtFin_SF_1),
                   Half_Bath = as.factor(Half_Bath),
                   TotRms_AbvGrd = as.factor(TotRms_AbvGrd)) %>% 
  select(-First_Flr_SF, -Second_Flr_SF)


##3 Remove 2 categorical 
ames4 <- ames3 %>%  select(-Longitude, -Latitude)
## Remove 12 categorical 
ames5 <- ames4 %>% select(-Utilities, -Street, -Alley, -Roof_Matl, -Land_Contour,
                          -Land_Slope, -Condition_1, -Condition_2, -Heating, -Functional,
                          -Pool_QC, -Bsmt_Cond, -Misc_Feature, -Central_Air, -Electrical,-Garage_Qual, -Garage_Cond, -Paved_Drive)

### equation writing function
model_equation <- function(model, ...) {
  format_args <- list(...)
  
  model_coeff <- model$coefficients
  format_args$x <- abs(model$coefficients)
  model_coeff_sign <- sign(model_coeff)
  model_coeff_prefix <- case_when(model_coeff_sign == -1 ~ " - ",
                                  model_coeff_sign == 1 ~ " + ",
                                  model_coeff_sign == 0 ~ " + ")
  model_eqn <- paste(strsplit(as.character(model$call$formula), "~")[[2]], # 'y'
                     "=",
                     paste(if_else(model_coeff[1]<0, "- ", ""),
                           do.call(format, format_args)[1],
                           paste(model_coeff_prefix[-1],
                                 do.call(format, format_args)[-1],
                                 " * ",
                                 names(model_coeff[-1]),
                                 sep = "", collapse = ""),
                           sep = ""))
  return(model_eqn)
}


# feature engineer ---- 
ames6 <- ames5 %>% mutate(
  Total_Area = Gr_Liv_Area + Total_Bsmt_SF,
  Price_Total_Per_Ft = Sale_Price / Total_Area,
  Price_Liv_Per_Ft = Sale_Price / Gr_Liv_Area,
  Remodeled = ifelse(Year_Remod_Add > Year_Built,1,0)) %>% 
  mutate_if(is.factor, as.character)

## dummify all categorical
source("./scripts/dummify.R")

## de-select  original categoricals 
ames7 <- ames6 %>% select(
  -MS_SubClass,-MS_Zoning,-Lot_Shape,-Lot_Config,-Neighborhood,
  -Bldg_Type,-House_Style,-Exterior_1st,-Exterior_2nd,-Mas_Vnr_Type,
  -Exter_Qual,-Exter_Cond,-Foundation,-Bsmt_Qual,-Roof_Style,-BsmtFin_Type_1,
  -BsmtFin_Type_2,-Heating_QC,-Kitchen_Qual,-Garage_Type,-Garage_Finish,
  -Sale_Type,-Sale_Condition, -Fence, -Fireplace_Qu,-Overall_Qual,-Overall_Cond,
  -Bsmt_Exposure,-BsmtFin_SF_1,-Full_Bath,-Half_Bath,-Bedroom_AbvGr,-TotRms_AbvGrd,
  -Fireplaces,-Garage_Cars)
  
# Import the model ---- 
fit <- readRDS(file = "my_precious.rds")


# Diagnostic ---- <- 
# ## Outlier ---- 
# p <- ncol(ames6)
# n <- nrow(ames6)
# ames_hats <- hatvalues(fit_632)
# which(ames_hats > 3 * (p+1)/n) -> leve_index
# ames_std <- rstandard(fit_632)
# which(abs(ames_std) > 3) -> outlier_index
# 
# index <- which(ames5$Gr_Liv_Area > 4000)
# 
# ### Remove outliers ---- 
# ames6 <- ames5[-c(1405,1660,1667,2046,2047),]
# 
# 
# ## Transforamtion ---- 
# 
# fit2 <- update(fit_632, .~ log(Gr_Liv_Area) + . , ames6)
# 
# fit3 <- update(fit2, .~log(Lot_Area) + . , ames6)
# 




