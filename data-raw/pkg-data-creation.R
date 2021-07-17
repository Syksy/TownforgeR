
infl.effects.ls <- TownforgeR::tf_influence_effects()

save(infl.effects.ls, file = "data/infl_effects_ls.rda")


building.names.contents <- 
  c("EMPTY"   ,  0, "Empty",
    "AGR"     ,  1, "Agricultural",
    "CRAFT"   ,  2, "Craft",
    "IND"     ,  3, "Industrial",
    "COM"     ,  4, "Commercial",
    "RES1"    ,  5, "Basic residential",
    "RES2"    ,  6, "Affluent residential",
    "RES3"    ,  7, "Luxury residential",
    "MIL"     ,  8, "Military",
    "CUL"     ,  9, "Cultural",
    "STO"     , 10, "Stonecutter",
    "SAW"     , 11, "Sawmill",
    "KILN"    , 12, "Kiln",
    "SME"     , 13, "Smelter",
    "WOR"     , 14, "Workforce",
    "ROAD"    , 15, "Road",
    "RESEARCH", 16, "Research")

building.names.df <- as.data.frame(matrix(building.names.contents, ncol = 3, byrow = TRUE))
colnames(building.names.df) <- c("abbrev", "id", "full.name")
building.names.df <- building.names.df[, c("id", "abbrev", "full.name")]

save(building.names.df, file = "data/building_names_df.rda")

building.names.v <- building.names.df$abbrev
names(building.names.v) <- building.names.df$full.name

save(building.names.v, file = "data/building_names_v.rda")


save(infl.effects.ls, building.names.df, building.names.v, file = "R/sysdata.rda")


# 1 = ag
# 2 = craft
# 5 = basic residential
# 10 = stonecutter
# 11 = sawmill
# 14 = workforce
# 15 = road
# ROLE_EMPTY = 0
# ROLE_AGRICULTURAL = 1
# ROLE_CRAFT = 2
# ROLE_RESIDENTIAL1 = 5
# ROLE_SAWMILL = 11
# ROLE_STONECUTTER = 10
# ROLE_WORKFORCE = 14
