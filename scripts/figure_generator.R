#
# This script generates figures used in the report.Rmd
#
# property type table ----
property_type_summary <- raw_ames %>%
  group_by(MS_Zoning) %>%
  summarize(n = n())
# EDA: summary statistics of the response
# EDA: Response distribution ----
## response histogram
sale_summary <- ames %>% ggplot() +
  geom_histogram(
    mapping = aes(x = Sale_Price),
    col = "red"
  ) +
  scale_x_continuous(labels = addUnits) +
  labs(title = "Sale Price Distribution", x = "Sale Price", y = "Frequency") +
  theme_fivethirtyeight()
## response boxplot
sale_summary_boxplot <- ggplot(ames, aes(Sale_Price)) +
  geom_boxplot(varwidth = T, fill = "plum") +
  scale_x_continuous(labels = addUnits) +
  labs(
    title = "Box plot",
    subtitle = "Ames Housing - Sale Price Distribution",
    caption = "Source: AmesHousing",
    x = "Sale Price",
    y = "Count"
  ) +
  theme_fivethirtyeight()
## sale v. GrLivArea scatterplot
sale_vs_GrLivArea <- ggplot(data = ames) +
  geom_point(mapping = aes(x = Gr_Liv_Area, y = Sale_Price), col = "red") +
  scale_y_continuous(labels = addUnits) +
  theme_fivethirtyeight() +
  labs(
    title = "Sales vs. General Living Area",
    subtitle = "Ames Housing - Sale Price",
    caption = "Source: AmesHousing",
    y = "Sale Price",
    x = "Above grade (ground) living area square feet"
  )
## response normal qqplot
qq <- ames %>% ggplot(aes(sample = Sale_Price)) +
  stat_qq() +
  stat_qq_line() +
  theme_fivethirtyeight() +
  labs(
    title = "Normal QQ Plot",
    subtitle = "Ames Housing - Sale Price",
    caption = "Source: AmesHousing",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )
# EDA: Transformed Response distribution ----
## trasnformed resopnse histogram
sale_summary_transformed <- raw_ames %>% ggplot() +
  geom_histogram(
    mapping = aes(
      x = log(Sale_Price),
      col = "red"
    )
  ) +
  scale_x_continuous(labels = addUnits) +
  labs(title = "Sale Price Distribution", x = "Sale Price", y = "Frequency") +
  theme_fivethirtyeight()
## transformed qqplot
qq_transformed <- ames %>% ggplot(aes(sample = log(Sale_Price))) +
  stat_qq() +
  stat_qq_line() +
  theme_fivethirtyeight() +
  labs(
    title = "Normal QQ Plot",
    subtitle = "Ames Housing - Sale Price",
    caption = "Source: AmesHousing",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )
## transformed sales vs. gr_liv_area
sale_vs_GrLivArea_transformed <- ggplot(data = raw_ames) +
  geom_point(mapping = aes(x = Gr_Liv_Area, y = log(Sale_Price)), col = "red") +
  scale_y_continuous(labels = addUnits) +
  theme_fivethirtyeight() +
  labs(
    title = "Sales vs. General Living Area",
    subtitle = "Ames Housing - Sale Price",
    caption = "Source: AmesHousing",
    y = "Sale Price",
    x = "Above grade (ground) living area square feet"
  )
## gather all plots
stack_plot_response <- ggarrange(sale_vs_GrLivArea,
  ggarrange(sale_summary, sale_summary_boxplot, qq, ncol = 3, nrow = 1),
  ncol = 1, nrow = 2
)
## gather all transformed plots
stack_plot_transformed_response <- ggarrange(sale_summary_transformed, qq_transformed, sale_vs_GrLivArea_transformed, ncol = 2, nrow = 2)
# EDA: numerics distribution ----
numeric_plot <- raw_ames %>%
  select(-Longitude, -Latitude, -Gr_Liv_Area, -Lot_Frontage, -First_Flr_SF,-Second_Flr_SF,
         -Sale_Price, -Year_Built, -Total_Bsmt_SF,-Year_Sold,-Mo_Sold,-Bsmt_Unf_SF,
         -Year_Remod_Add, -Garage_Area) %>% 
  keep(is.numeric) %>%
  gather() %>%
  ggplot(aes(value)) +
  facet_wrap(~key, scales = "free") +
  geom_histogram() +
  theme_fivethirtyeight()

# EDA: correlation check ----
cc2 <- round(cor(ames %>% dplyr::select_if(is.numeric)), 2)
### Upper triangle
get_upper_tri <- function(cormat) {
  cormat[lower.tri(cormat)] <- NA
  return(cormat)
}
### reorder the correlation matrix according to the correlation coefficient
reorder_cormat <- function(cormat) {
  dd <- as.dist((1 - cormat) / 2)
  hc <- hclust(dd)
  cormat <- cormat[hc$order, hc$order]
}
cc3 <- reorder_cormat(cc2)
upper_tri <- get_upper_tri(cc3)
cc <- melt(upper_tri, na.rm = TRUE)
### correlation map of lower triangle matrix
gz <- ggplot(cc, mapping = aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1, size = 10)) +
  theme(text = element_text(size = 10)) +
  ggtitle("Heat Map for Ames Housing Data: Numeric Predictors") +
  ylab("") +
  xlab("") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Pearson\nCorrelation"
  )



# EDA: Multicolinearity & VIF ----
fit_test <- lm(Sale_Price ~ ., data = raw_ames %>% select_if(is.numeric) %>% select(
  -BsmtFin_SF_2, -Enclosed_Porch, -Low_Qual_Fin_SF, -Misc_Val,
  -Pool_Area, -Screen_Porch, -Three_season_porch, -Bsmt_Half_Bath,
  -Kitchen_AbvGr, -Open_Porch_SF, -Longitude, -Latitude
))
vif_summary <- fit_test %>% summary()
fit_test %>%
  vif() %>%
  round(2) -> vif_score

# EDA: categoricals ----
## removable cats plot
utilities_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Utilities)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  labs(title = "Utilities Type Bar Plot", x = "Sale Price", y = "Categories") +
  scale_x_continuous(labels = addUnits)

street_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Street)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  labs(title = "Street Type Bar Plot", x = "Sale Price", y = "Categories") +
  scale_x_continuous(labels = addUnits)

alley_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Alley)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  labs(title = "Alley Type Bar Plot", x = "Sale Price", y = "Categories") +
  scale_x_continuous(labels = addUnits)

landslope_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Land_Slope)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  labs(title = "Land Slope Type Bar Plot", x = "Sale Price", y = "Categories") +
  scale_x_continuous(labels = addUnits)

landcountour_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Land_Contour)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  labs(title = "Land Contour Type Bar Plot", x = "Sale Price", y = "Categories") +
  scale_x_continuous(labels = addUnits)

cond1_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Condition_1)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  labs(title = "Condition1 Type Bar Plot", x = "Sale Price", y = "Categories") +
  scale_x_continuous(labels = addUnits)

cond2_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Condition_2)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  labs(title = "Condition2 Bar Plot", x = "Sale Price", y = "Categories") +
  scale_x_continuous(labels = addUnits)

roof_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Roof_Matl)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  labs(title = "Roof Type Bar Plot", x = "Sale Price", y = "Categories") +
  scale_x_continuous(labels = addUnits)

heat_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Heating)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  labs(title = "Heating Type Bar Plot", x = "Sale Price", y = "Categories") +
  scale_x_continuous(labels = addUnits)

fun_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Functional)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  labs(title = "Function Type Bar Plot", x = "Sale Price", y = "Categories") +
  scale_x_continuous(labels = addUnits)

pool_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Pool_QC)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  labs(title = "Pool Type Bar Plot", x = "Sale Price", y = "Categories") +
  scale_x_continuous(labels = addUnits)

basement_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Bsmt_Cond)) +
  geom_bar(stat = "identity") +
  theme_fivethirtyeight() +
  labs(title = "Basecment Condition Type Bar Plot", x = "Sale Price", y = "Categories") +
  scale_x_continuous(labels = addUnits)

neightborhood_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Neighborhood, fill = Neighborhood)) +
  geom_boxplot(show.legend = FALSE) +
  theme_fivethirtyeight() +
  scale_x_continuous(labels = addUnits) +
  labs(title = "Sale Price by Neighborhood", x = "Sale Price")

ms_zoning_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = MS_Zoning, fill = MS_Zoning)) +
  geom_boxplot(show.legend = FALSE) +
  theme_fivethirtyeight() +
  scale_x_continuous(labels = addUnits) +
  labs(title = "Sale Price by Residental Density", x = "Sale Price")

building_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = Bldg_Type, fill = Bldg_Type)) +
  geom_boxplot(show.legend = FALSE) +
  theme_fivethirtyeight() +
  scale_x_continuous(labels = addUnits) +
  labs(title = "Sale Price by Building Type", x = "Sale Price")

housestyle_plot <- raw_ames %>%
  ggplot(aes(x = Sale_Price, y = House_Style, fill = House_Style)) +
  geom_boxplot(show.legend = FALSE) +
  theme_fivethirtyeight() +
  scale_x_continuous(labels = addUnits) +
  labs(title = "Sale Price by House Style", x = "Sale Price")

stack_cat_keep <- ggarrange(neightborhood_plot, 
                    ggarrange(building_plot,housestyle_plot, ncol=2, nrow=1),ncol=1,nrow=2)
  


### gather categoricals that can be removed
stack_plot_rm <- ggarrange(utilities_plot, street_plot, alley_plot, roof_plot,
  landslope_plot, landcountour_plot, cond1_plot, cond2_plot,
  heat_plot, fun_plot, pool_plot, basement_plot,
  ncol = 3, nrow = 4
)


