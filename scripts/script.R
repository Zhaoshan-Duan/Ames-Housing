# Main script
# read package script ----
source("./scripts/packages.R")
source("./scripts/functions.R")

# import data ----
AmesHousing::make_ames() -> raw_ames
# Preprocessing ---- 
## recode MS_Zoning factor levels and covert date objects
ames <- raw_ames %>%
  mutate(
    MS_Zoning = dplyr::recode(MS_Zoning,
      A_agr = "Agriculture",
      I_all = "Industrial",
      C_all = "Commercial"
    ),
    Year_Built = as.Date(Year_Built),
    Year_Remod_Add = as.Date(Year_Remod_Add),
    Year_Sold = as.Date(Year_Sold),
    Mo_Sold = as.Date(Mo_Sold),
    Fireplaces = as.factor(Fireplaces),
    Bsmt_Full_Bath = as.factor(Bsmt_Full_Bath)
  ) %>%
  filter(MS_Zoning %in% c(
    "Residential_High_Density",
    "Residential_Low_Density",
    "Residential_Medium_Density"
  )) %>%
  ## drop numerics with mostly 0 values
  select(
    -BsmtFin_SF_2, -Enclosed_Porch, -Low_Qual_Fin_SF, -Misc_Val,
    -Pool_Area, -Screen_Porch, -Three_season_porch, -Bsmt_Half_Bath,
    -Kitchen_AbvGr, -Open_Porch_SF, -Longitude, -Latitude
  ) %>%
  ## convert count data to factors
  mutate(
    Garage_Cars = as.factor(Garage_Cars),
    Full_Bath = as.factor(Full_Bath),
    Bedroom_AbvGr = as.factor(Bedroom_AbvGr),
    BsmtFin_SF_1 = as.factor(BsmtFin_SF_1),
    Half_Bath = as.factor(Half_Bath),
    TotRms_AbvGrd = as.factor(TotRms_AbvGrd)
  ) %>%
  select(-First_Flr_SF, -Second_Flr_SF) %>%
  ## Remove categoricals with values concentrated in one category
  select(
    -Utilities, -Street, -Alley, -Roof_Matl, -Land_Contour,
    -Land_Slope, -Condition_1, -Condition_2, -Heating, -Functional,
    -Pool_QC, -Bsmt_Cond, -Misc_Feature, -Central_Air, -Electrical,
    -Garage_Qual, -Garage_Cond, -Paved_Drive
  ) %>%
  ## engineer some features
  mutate(
    Total_Area = Gr_Liv_Area + Total_Bsmt_SF,
    Price_Total_Per_Ft = Sale_Price / Total_Area,
    Price_Liv_Per_Ft = Sale_Price / Gr_Liv_Area,
    Remodeled = ifelse(Year_Remod_Add > Year_Built, 1, 0)
  ) 

# Variable Selection ----
mod <- readRDS(file = "mod.rds")
fit <- update(mod, log(Sale_Price) ~. - Mas_Vnr_Area - MS_Zoning - Garage_Cars - 
                Fireplace_Qu - Neighborhood - Exterior_2nd-Price_Total_Per_Ft-Price_Liv_Per_Ft, data=ames)

## Outliers ---- 
which(hatvalues(fit) > 2 * mean(hatvalues(fit))) -> x
which(abs(rstandard(fit))> 3) -> y 
intersect(x,y) -> outlier
ames[-outlier,] -> ames1

fit2 <- update(fit, .~., ames1)

## Transformation on predictor ---- 
pred_tran <- powerTransform(
  cbind(Total_Area,Total_Bsmt_SF,
        Garage_Area,Lot_Frontage) + 1 ~ 1, data=ames1)

fit3 <- update(fit2, .~ +sqrt(Total_Area)-Total_Area, ames1)

## Final fit ----
source("./scripts/dummy.R")

fit_final <- lm(
  log(Sale_Price) ~ 
    Total_Bsmt_SF+Garage_Area+Overall_Qual_Good+Overall_Qual_Poor+
    TotRms_AbvGrd_15+Bsmt_Qual_Typical+Bsmt_Qual_Good+Bsmt_Qual_No_Basement+Year_Built+ sqrt(Total_Area), ames_final)
