# 
# Variable selection using forward, back and stepwise
# 
library(leaps)
library(car)

n <- nrow(ames)
null_mod <- lm(Sale_Price ~ 1, data = ames)
full_mod <- lm(Sale_Price ~., data = ames)

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
anova(step_aic,foward_aic) #prefer step

## backward vs. 
anova(step_aic,back_aic) #prefer back
anova(foward_aic,back_aic) #prefer step 

my_precious <- step_aic
# save as RDS 
saveRDS(my_precious, file = "mod.rds")



