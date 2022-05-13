# Dummify all the categorical variables

ames_almost <- ames1 %>% 
  mutate(Year_Remod_Add = as.factor(Year_Remod_Add)) %>% 
  select(Overall_Qual,Total_Area,TotRms_AbvGrd,Bsmt_Qual,Fireplaces,Total_Bsmt_SF,MS_SubClass,Lot_Shape,
         Overall_Cond,Full_Bath,Half_Bath,Garage_Area,Bsmt_Exposure,Bedroom_AbvGr,Bsmt_Full_Bath,Lot_Frontage,
         Year_Built,Remodeled,Year_Remod_Add,Exter_Cond,Sale_Price)
ames_final <- ames_almost

for(level in unique(ames_final$Overall_Qual)){
  ames_final[paste("Overall_Qual", level, sep = "_")] <- ifelse(ames_final$Overall_Qual == level, 1, 0)
}

for(level in unique(ames_final$Overall_Cond)){
  ames_final[paste("Overall_Cond", level, sep = "_")] <- ifelse(ames_final$Overall_Cond == level, 1, 0)
}

for(level in unique(ames_final$TotRms_AbvGrd)){
  ames_final[paste("TotRms_AbvGrd", level, sep = "_")] <- ifelse(ames_final$TotRms_AbvGrd == level, 1, 0)
}

for(level in unique(ames_final$Bsmt_Qual)){
  ames_final[paste("Bsmt_Qual", level, sep = "_")] <- ifelse(ames_final$Bsmt_Qual == level, 1, 0)
}

for(level in unique(ames_final$Fireplaces)){
  ames_final[paste("Fireplaces", level, sep = "_")] <- ifelse(ames_final$Fireplaces == level, 1, 0)
}

for(level in unique(ames_final$MS_SubClass)){
  ames_final[paste("MS_SubClass", level, sep = "_")] <- ifelse(ames_final$MS_SubClass == level, 1, 0)
}

for(level in unique(ames_final$Lot_Shape)){
  ames_final[paste("Lot_Shape", level, sep = "_")] <- ifelse(ames_final$Lot_Shape == level, 1, 0)
}


for(level in unique(ames_final$Half_Bath)){
  ames_final[paste("Half_Bath", level, sep = "_")] <- ifelse(ames_final$Half_Bath == level, 1, 0)
}

for(level in unique(ames_final$Bsmt_Exposure)){
  ames_final[paste("Bsmt_Exposure", level, sep = "_")] <- ifelse(ames_final$Bsmt_Exposure == level, 1, 0)
}

for(level in unique(ames_final$Bedroom_AbvGr)){
  ames_final[paste("Bedroom_AbvGr", level, sep = "_")] <- ifelse(ames_final$Bedroom_AbvGr == level, 1, 0)
}

for(level in unique(ames_final$Bsmt_Full_Bath)){
  ames_final[paste("Bsmt_Full_Bath", level, sep = "_")] <- ifelse(ames_final$Bsmt_Full_Bath == level, 1, 0)
}
for(level in unique(ames_final$Year_Remod_Add)){
  ames_final[paste("Year_Remod_Add", level, sep = "_")] <- ifelse(ames_final$Year_Remod_Add == level, 1, 0)
}

ames_final <- ames_final %>% select_if( ~!is.factor(.))


