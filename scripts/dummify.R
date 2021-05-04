#
# Dummification of all categorical variables
#

for(level in unique(ames6$MS_Zoning)){
  ames6[paste("MS_Zoning", level, sep = "_")] <- ifelse(ames6$MS_Zoning == level, 1, 0)
}

for(level in unique(ames6$Lot_Shape)){
  ames6[paste("Lot_Shape", level, sep = "_")] <- ifelse(ames6$Lot_Shape == level, 1, 0)
}

for(level in unique(ames6$Lot_Config)){
  ames6[paste("Lot_Config", level, sep = "_")] <- ifelse(ames6$Lot_Config == level, 1, 0)
}

for(level in unique(ames6$Neighborhood)){
  ames6[paste("Neighborhood", level, sep = "_")] <- ifelse(ames6$Neighborhood == level, 1, 0)
}

for(level in unique(ames6$Bldg_Type)){
  ames6[paste("Bldg_Type", level, sep = "_")] <- ifelse(ames6$Bldg_Type == level, 1, 0)
}

for(level in unique(ames6$House_Style)){
  ames6[paste("House_Style", level, sep = "_")] <- ifelse(ames6$House_Style == level, 1, 0)
}

for(level in unique(ames6$Exterior_1st)){
  ames6[paste("Exterior_1st", level, sep = "_")] <- ifelse(ames6$Exterior_1st == level, 1, 0)
}

for(level in unique(ames6$Exterior_2nd)){
  ames6[paste("Exterior_2nd", level, sep = "_")] <- ifelse(ames6$Exterior_2nd == level, 1, 0)
}

for(level in unique(ames6$Mas_Vnr_Type)){
  ames6[paste("Mas_Vnr_Type", level, sep = "_")] <- ifelse(ames6$Mas_Vnr_Type == level, 1, 0)
}

for(level in unique(ames6$Exter_Qual)){
  ames6[paste("Exter_Qual", level, sep = "_")] <- ifelse(ames6$Exter_Qual == level, 1, 0)
}

for(level in unique(ames6$Exter_Cond)){
  ames6[paste("Exter_Cond", level, sep = "_")] <- ifelse(ames6$Exter_Cond == level, 1, 0)
}

for(level in unique(ames6$Foundation)){
  ames6[paste("Foundation", level, sep = "_")] <- ifelse(ames6$Foundation == level, 1, 0)
}

for(level in unique(ames6$Bsmt_Qual)){
  ames6[paste("Bsmt_Qual", level, sep = "_")] <- ifelse(ames6$Bsmt_Qual == level, 1, 0)
}


for(level in unique(ames6$Roof_Style)){
  ames6[paste("Roof_Style", level, sep = "_")] <- ifelse(ames6$Roof_Style == level, 1, 0)
}

for(level in unique(ames6$BsmtFin_Type_1)){
  ames6[paste("BsmtFin_Type_1", level, sep = "_")] <- ifelse(ames6$BsmtFin_Type_1 == level, 1, 0)
}


for(level in unique(ames6$BsmtFin_Type_2)){
  ames6[paste("BsmtFin_Type_2", level, sep = "_")] <- ifelse(ames6$BsmtFin_Type_2 == level, 1, 0)
}

for(level in unique(ames6$Heating_QC)){
  ames6[paste("Heating_QC", level, sep = "_")] <- ifelse(ames6$Heating_QC == level, 1, 0)
}


for(level in unique(ames6$Kitchen_Qual)){
  ames6[paste("Kitchen_Qual", level, sep = "_")] <- ifelse(ames6$Kitchen_Qual == level, 1, 0)
}


for(level in unique(ames6$Garage_Type)){
  ames6[paste("Garage_Type", level, sep = "_")] <- ifelse(ames6$Garage_Type == level, 1, 0)
}

for(level in unique(ames6$Garage_Finish)){
  ames6[paste("Garage_Finish", level, sep = "_")] <- ifelse(ames6$Garage_Finish == level, 1, 0)
}


for(level in unique(ames6$Sale_Type)){
  ames6[paste("Sale_Type", level, sep = "_")] <- ifelse(ames6$Sale_Type == level, 1, 0)
}

for(level in unique(ames6$Sale_Condition)){
  ames6[paste("Sale_Condition", level, sep = "_")] <- ifelse(ames6$Sale_Condition == level, 1, 0)
}

for(level in unique(ames6$Overall_Qual)){
  ames6[paste("Overall_Qual", level, sep = "_")] <- ifelse(ames6$Overall_Qual == level, 1, 0)
}

for(level in unique(ames6$Overall_Cond)){
  ames6[paste("Overall_Cond", level, sep = "_")] <- ifelse(ames6$Overall_Cond == level, 1, 0)
}

for(level in unique(ames6$Bsmt_Exposure)){
  ames6[paste("Bsmt_Exposure", level, sep = "_")] <- ifelse(ames6$Bsmt_Exposure == level, 1, 0)
}

for(level in unique(ames6$BsmtFin_SF_1)){
  ames6[paste("BsmtFin_SF_1", level, sep = "_")] <- ifelse(ames6$BsmtFin_SF_1 == level, 1, 0)
}


for(level in unique(ames6$Full_Bath)){
  ames6[paste("Full_Bath", level, sep = "_")] <- ifelse(ames6$Full_Bath == level, 1, 0)
}

for(level in unique(ames6$Half_Bath)){
  ames6[paste("Half_Bath", level, sep = "_")] <- ifelse(ames6$Half_Bath == level, 1, 0)
}

for(level in unique(ames6$Bedroom_AbvGr)){
  ames6[paste("Bedroom_AbvGr", level, sep = "_")] <- ifelse(ames6$Bedroom_AbvGr == level, 1, 0)
}

for(level in unique(ames6$TotRms_AbvGrd)){
  ames6[paste("TotRms_AbvGrd", level, sep = "_")] <- ifelse(ames6$TotRms_AbvGrd == level, 1, 0)
}

for(level in unique(ames6$Fireplaces)){
  ames6[paste("Fireplaces", level, sep = "_")] <- ifelse(ames6$Fireplaces == level, 1, 0)
}

for(level in unique(ames6$Garage_Cars)){
  ames6[paste("Garage_Cars", level, sep = "_")] <- ifelse(ames6$Garage_Cars == level, 1, 0)
}

