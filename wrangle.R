# set up libraries
library(pacman)
p_load(tidyverse,AmesHousing, Boruta)

# access the data
ames <- make_ames()

nrow(ames)
summary(ames$Sale_Price)



glimpse(ames)

str(ames)


Boruta(Sale_Price ~. , data=ames) -> Boruta.test

ames_schools_geo


ames_ord <- make_ordinal_ames()
ord_vars <- vapply(ames_ord, is.ordered, logical(1))
names(ord_vars)[ord_vars]