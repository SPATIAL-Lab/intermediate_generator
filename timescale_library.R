####################################################################################################
##### Translate Phanerozoic sample ages between timescales
#####
##### Hard-coded full Phanerozoic stage tables for each timescale.
####################################################################################################

build_gts2020 <- function() {
  
  stage <- c(
    "Meghalayan", "Northgrippian", "Greenlandian",
    "Late Pleistocene", "Chibanian", "Calabrian", "Gelasian",
    "Piacenzian", "Zanclean",
    "Messinian", "Tortonian", "Serravallian", "Langhian", "Burdigalian", "Aquitanian",
    "Chattian", "Rupelian",
    "Priabonian", "Bartonian", "Lutetian", "Ypresian",
    "Thanetian", "Selandian", "Danian",
    "Maastrichtian", "Campanian", "Santonian", "Coniacian", "Turonian", "Cenomanian",
    "Albian", "Aptian", "Barremian", "Hauterivian", "Valanginian", "Berriasian",
    "Tithonian", "Kimmeridgian", "Oxfordian",
    "Callovian", "Bathonian", "Bajocian", "Aalenian",
    "Toarcian", "Pliensbachian", "Sinemurian", "Hettangian",
    "Rhaetian", "Norian", "Carnian",
    "Ladinian", "Anisian",
    "Olenekian", "Induan",
    "Changhsingian", "Wuchiapingian",
    "Capitanian", "Wordian", "Roadian",
    "Kungurian", "Artinskian", "Sakmarian", "Asselian",
    "Gzhelian", "Kasimovian", "Moscovian", "Bashkirian",
    "Serpukhovian", "Visean", "Tournaisian",
    "Famennian", "Frasnian",
    "Givetian", "Eifelian",
    "Emsian", "Pragian", "Lochkovian",
    "Pridoli",
    "Ludfordian", "Gorstian",
    "Homerian", "Sheinwoodian",
    "Telychian", "Aeronian", "Rhuddanian",
    "Hirnantian", "Katian", "Sandbian",
    "Darriwilian", "Dapingian",
    "Floian", "Tremadocian",
    "Age 10", "Jiangshanian", "Paibian",
    "Guzhangian", "Drumian", "Wuliuan",
    "Age 4", "Age 3", "Age 2", "Fortunian"
  )
  
  base_ma <- c(
    0.004250, 0.008236, 0.01170,
    0.129, 0.774, 1.80, 2.58,
    3.60, 5.335,
    7.25, 11.625, 13.82, 15.99, 20.45, 23.04,
    27.29, 33.90,
    37.71, 41.03, 48.07, 56.00,
    59.24, 61.66, 66.04,
    72.17, 83.65, 85.70, 89.39, 93.90, 100.50,
    113.20, 121.40, 126.50, 132.60, 137.70, 143.10,
    149.24, 154.80, 161.50,
    165.30, 168.20, 170.90, 174.70,
    184.20, 192.90, 199.46, 201.36,
    205.74, 227.30, 237.00,
    241.46, 246.70,
    249.88, 251.90,
    254.24, 259.55,
    264.34, 269.21, 274.37,
    283.30, 290.51, 293.52, 298.89,
    303.68, 307.02, 315.15, 323.40,
    330.34, 346.73, 359.30,
    371.10, 378.90,
    385.30, 394.30,
    410.50, 412.40, 419.00,
    422.73,
    425.01, 426.74,
    430.62, 432.93,
    438.59, 440.49, 443.07,
    445.21, 452.75, 458.18,
    469.42, 471.26,
    477.08, 486.85,
    491.00, 494.20, 497.00,
    500.50, 504.50, 509.00,
    514.50, 521.00, 529.00, 538.80
  )
  
  period <- c(
    rep("Quaternary", 7),
    rep("Neogene", 8),
    rep("Paleogene", 9),
    rep("Cretaceous", 12),
    rep("Jurassic", 11),
    rep("Triassic", 7),
    rep("Permian", 9),
    rep("Carboniferous", 7),
    rep("Devonian", 7),
    rep("Silurian", 8),
    rep("Ordovician", 7),
    rep("Cambrian", 10)
  )
  
  series_epoch <- c(
    "Holocene", "Holocene", "Holocene",
    "Pleistocene", "Pleistocene", "Pleistocene", "Pleistocene",
    
    "Pliocene", "Pliocene",
    "Miocene", "Miocene", "Miocene", "Miocene", "Miocene", "Miocene",
    
    "Oligocene", "Oligocene",
    "Eocene", "Eocene", "Eocene", "Eocene",
    "Paleocene", "Paleocene", "Paleocene",
    
    "Late Cretaceous", "Late Cretaceous", "Late Cretaceous", "Late Cretaceous", "Late Cretaceous", "Late Cretaceous",
    "Early Cretaceous", "Early Cretaceous", "Early Cretaceous", "Early Cretaceous", "Early Cretaceous", "Early Cretaceous",
    
    "Late Jurassic", "Late Jurassic", "Late Jurassic",
    "Middle Jurassic", "Middle Jurassic", "Middle Jurassic", "Middle Jurassic",
    "Early Jurassic", "Early Jurassic", "Early Jurassic", "Early Jurassic",
    
    "Late Triassic", "Late Triassic", "Late Triassic",
    "Middle Triassic", "Middle Triassic",
    "Early Triassic", "Early Triassic",
    
    "Lopingian", "Lopingian",
    "Guadalupian", "Guadalupian", "Guadalupian",
    "Cisuralian", "Cisuralian", "Cisuralian", "Cisuralian",
    
    "Pennsylvanian", "Pennsylvanian", "Pennsylvanian", "Pennsylvanian",
    "Mississippian", "Mississippian", "Mississippian",
    
    "Late Devonian", "Late Devonian",
    "Middle Devonian", "Middle Devonian",
    "Early Devonian", "Early Devonian", "Early Devonian",
    
    "Pridoli",
    "Ludlow", "Ludlow",
    "Wenlock", "Wenlock",
    "Llandovery", "Llandovery", "Llandovery",
    
    "Late Ordovician", "Late Ordovician", "Late Ordovician",
    "Middle Ordovician", "Middle Ordovician",
    "Early Ordovician", "Early Ordovician",
    
    "Furongian", "Furongian", "Furongian",
    "Miaolingian", "Miaolingian", "Miaolingian",
    "Series 2", "Series 2", "Series 2", "Terreneuvian"
  )
  
  out <- data.frame(
    period = period,
    series_epoch = series_epoch,
    stage = stage,
    base_ma = base_ma,
    stringsAsFactors = FALSE
  )
  
  out$top_ma <- c(0, head(out$base_ma, -1))
  
  out$boundary_id <- out$stage
  out$boundary_id[out$stage == "Greenlandian"]     <- "Holocene"
  out$boundary_id[out$stage == "Late Pleistocene"] <- "Late Pleistocene"
  
  out
}


build_gts2012 <- function() {
  
  stage <- c(
    "Holocene", "Upper Pleistocene", "Calabrian", "Gelasian",
    "Piacenzian", "Zanclean",
    "Messinian", "Tortonian", "Serravallian", "Langhian", "Burdigalian", "Aquitanian",
    "Chattian", "Rupelian",
    "Priabonian", "Bartonian", "Lutetian", "Ypresian",
    "Thanetian", "Selandian", "Danian",
    "Maastrichtian", "Campanian", "Santonian", "Coniacian", "Turonian", "Cenomanian",
    "Albian", "Aptian", "Barremian", "Hauterivian", "Valanginian", "Berriasian",
    "Tithonian", "Kimmeridgian", "Oxfordian",
    "Callovian", "Bathonian", "Bajocian", "Aalenian",
    "Toarcian", "Pliensbachian", "Sinemurian", "Hettangian",
    "Rhaetian", "Norian", "Carnian",
    "Ladinian", "Anisian", "Olenekian", "Induan",
    "Changhsingian", "Wuchiapingian",
    "Capitanian", "Wordian", "Roadian",
    "Kungurian", "Artinskian", "Sakmarian", "Asselian",
    "Gzhelian", "Kasimovian", "Moscovian", "Bashkirian",
    "Serpukhovian", "Visean", "Tournaisian",
    "Famennian", "Frasnian",
    "Givetian", "Eifelian",
    "Emsian", "Pragian", "Lochkovian",
    "Pridoli",
    "Ludfordian", "Gorstian",
    "Homerian", "Sheinwoodian",
    "Telychian", "Aeronian", "Rhuddanian",
    "Hirnantian", "Katian", "Sandbian",
    "Darriwilian", "Dapingian",
    "Floian", "Tremadocian",
    "Age 10", "Jiangshanian", "Paibian",
    "Guzhangian", "Drumian", "Wuliuan",
    "Age 4", "Age 3", "Age 2", "Fortunian"
  )
  
  base_ma <- c(
    0.0118, 0.126, 1.806, 2.59,
    3.60, 5.333,
    7.246, 11.63, 13.82, 15.97, 20.43, 23.03,
    28.09, 33.89,
    37.80, 41.20, 47.82, 55.96,
    59.24, 61.61, 66.04,
    72.05, 83.64, 86.26, 89.77, 93.90, 100.50,
    112.95, 126.30, 130.77, 133.88, 140.18, 145.01,
    152.06, 157.30, 163.50,
    166.10, 168.30, 170.30, 174.10,
    182.70, 190.80, 199.30, 201.30,
    209.46, 228.35, 237.00,
    241.50, 247.06, 250.01, 252.16,
    254.20, 259.81,
    265.14, 268.80, 272.30,
    279.33, 290.06, 295.53, 298.88,
    303.67, 306.99, 315.16, 323.23,
    330.92, 346.73, 358.94,
    372.24, 382.69,
    387.72, 393.25,
    407.57, 410.78, 419.20,
    422.96,
    425.57, 427.36,
    430.45, 433.35,
    438.49, 440.77, 443.83,
    445.16, 452.97, 458.36,
    467.25, 469.96,
    477.72, 485.37,
    489.50, 494.00, 497.00,
    500.50, 504.50, 509.00,
    514.00, 521.00, 529.00, 541.00
  )
  
  period <- c(
    rep("Quaternary", 4),
    rep("Neogene", 8),
    rep("Paleogene", 9),
    rep("Cretaceous", 12),
    rep("Jurassic", 11),
    rep("Triassic", 7),
    rep("Permian", 9),
    rep("Carboniferous", 7),
    rep("Devonian", 7),
    rep("Silurian", 8),
    rep("Ordovician", 7),
    rep("Cambrian", 10)
  )
  
  series_epoch <- c(
    "Holocene", "Pleistocene", "Pleistocene", "Pleistocene",
    
    "Pliocene", "Pliocene",
    "Miocene", "Miocene", "Miocene", "Miocene", "Miocene", "Miocene",
    
    "Oligocene", "Oligocene",
    "Eocene", "Eocene", "Eocene", "Eocene",
    "Paleocene", "Paleocene", "Paleocene",
    
    "Late Cretaceous", "Late Cretaceous", "Late Cretaceous", "Late Cretaceous", "Late Cretaceous", "Late Cretaceous",
    "Early Cretaceous", "Early Cretaceous", "Early Cretaceous", "Early Cretaceous", "Early Cretaceous", "Early Cretaceous",
    
    "Late Jurassic", "Late Jurassic", "Late Jurassic",
    "Middle Jurassic", "Middle Jurassic", "Middle Jurassic", "Middle Jurassic",
    "Early Jurassic", "Early Jurassic", "Early Jurassic", "Early Jurassic",
    
    "Late Triassic", "Late Triassic", "Late Triassic",
    "Middle Triassic", "Middle Triassic",
    "Early Triassic", "Early Triassic",
    
    "Lopingian", "Lopingian",
    "Guadalupian", "Guadalupian", "Guadalupian",
    "Cisuralian", "Cisuralian", "Cisuralian", "Cisuralian",
    
    "Pennsylvanian", "Pennsylvanian", "Pennsylvanian", "Pennsylvanian",
    "Mississippian", "Mississippian", "Mississippian",
    
    "Late Devonian", "Late Devonian",
    "Middle Devonian", "Middle Devonian",
    "Early Devonian", "Early Devonian", "Early Devonian",
    
    "Pridoli",
    "Ludlow", "Ludlow",
    "Wenlock", "Wenlock",
    "Llandovery", "Llandovery", "Llandovery",
    
    "Late Ordovician", "Late Ordovician", "Late Ordovician",
    "Middle Ordovician", "Middle Ordovician",
    "Early Ordovician", "Early Ordovician",
    
    "Furongian", "Furongian", "Furongian",
    "Miaolingian", "Miaolingian", "Miaolingian",
    "Series 2", "Series 2", "Series 2", "Terreneuvian"
  )
  
  out <- data.frame(
    period = period,
    series_epoch = series_epoch,
    stage = stage,
    base_ma = base_ma,
    stringsAsFactors = FALSE
  )
  
  out$top_ma <- c(0, head(out$base_ma, -1))
  
  out$boundary_id <- out$stage
  out$boundary_id[out$stage == "Holocene"]          <- "Holocene"
  out$boundary_id[out$stage == "Upper Pleistocene"] <- "Late Pleistocene"
  
  out
}


build_timescale_library <- function() {
  list(
    GTS2012 = build_gts2012(),
    GTS2020 = build_gts2020()
  )
}
