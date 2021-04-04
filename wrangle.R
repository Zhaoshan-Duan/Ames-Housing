# set up libraries
library(pacman)
p_load(tidyverse,AmesHousing, Boruta, faraway)

# access the data
ames <- make_ames()

nrow(ames)
summary(ames$Sale_Price)


fit <- lm(Sale_Price ~ ., ames)
summary(fit)


round(vif(fit),2) -> what

for (val in what){
  if (val >= 5) print(what[[val]])
}



