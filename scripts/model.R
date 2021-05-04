# 
# Model selection 
# 
library(leaps)
library(car)

n <- nrow(ames7)
null_mod <- lm(Sale_Price ~ 1, data = ames7)
full_mod <- lm(Sale_Price ~., data = ames7)

## forward selection
foward_aic <- step(null_mod, scope=list(lower=null_mod, upper=full_mod), direction="forward", trace=0); 
foward_bic <- step(null_mod, scope=list(lower=null_mod, upper=full_mod), direction="forward", k=log(n),trace=0); 
anova(foward_bic, foward_aic) # prefer aic

## backward 
back_aic <- step(full_mod, direction="backward",trace=0);
back_bic <- step(full_mod, direction="backward", k=log(n),trace=0);
anova(back_bic,back_aic) #prefer aic

## stepwise 
step_aic <- step(null_mod, scope = list(lower = null_mod, upper=full_mod), trace = 0);
step_bic <- step(null_mod, scope = list(lower = null_mod, upper=full_mod),k=log(n),trace=0) 
anova(step_bic,step_aic) #prefer aic

## foward vs. 
anova(foward_aic,back_aic) #prefer foward
anova(foward_aic, step_aic) #same 

## backward vs. 
anova(step_aic,back_aic) #prefer back
anova(foward_aic,back_aic) #prefer step 

my_precious <- step_aic_log
saveRDS(my_precious, file = "my_precious.rds")


# Boruta_result <- lm(Sale_Price ~ 
#                       MS_SubClass + MS_Zoning+ Lot_Frontage + Lot_Area + Lot_Shape + Neighborhood + Bldg_Type + 
#                       House_Style + Overall_Qual + Overall_Cond + Roof_Style + Exterior_1st + Exterior_2nd + 
#                       Mas_Vnr_Type + Mas_Vnr_Area + Exter_Qual + Foundation + Bsmt_Qual + Bsmt_Exposure + 
#                       BsmtFin_Type_1 + BsmtFin_SF_1 + Heating_QC + Central_Air + Electrical + Full_Bath + Half_Bath + 
#                       Bedroom_AbvGr + Kitchen_Qual + TotRms_AbvGrd + Fireplaces + Fireplace_Qu + Garage_Cars + Garage_Type + 
#                       Garage_Finish + Garage_Area + Garage_Qual + Garage_Cond + Paved_Drive + Sale_Condition + 
#                       First_Flr_SF + Second_Flr_SF + Bsmt_Unf_SF + Total_Bsmt_SF + Bsmt_Full_Bath + Wood_Deck_SF + 
#                       Open_Porch_SF + Year_Built+Year_Remod_Add, ames)







