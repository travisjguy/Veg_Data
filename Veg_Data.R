veg <- read.csv("~/Dropbox/Bighorn Sheep Data/Data for R class/veg.csv")

head(veg)

library("dplyr")

library("stringr")

library("magrittr")


veg$Species_clean <- as.character(veg$Species)

clean_species <- function(sp) {
  sp <- as.character(sp)
  sp_split <- unlist(strsplit(sp, " "))
  if (tolower(sp_split[1]) != "unk") {
    substr(sp_split[1], 1, 1) <- toupper(substr(sp_split[1], 1, 1))
    substring(sp_split[1], 2) <- tolower(substring(sp_split[1], 2))
    if (length(sp_split) > 1) sp_split[2] <- tolower(sp_split[2])
  } else {
    sp_split <- tolower(sp_split)
  }
  sp_split <- paste(sp_split, collapse = " ")
  if (sp_split == "Unknown") sp_split <- "unk"  ##brute force for when you have 1 word
  if (sp_split == "Bae") sp_split <- "Bare ground"
  if (sp_split == "Bistorta bistortoides flower") sp_split <- "Bistorta bistortoides"
  if (sp_split == "Roas") sp_split <- "Road"
  if (sp_split == "Jun") sp_split <- "Juncus spp."
  if (sp_split == "Liter") sp_split <- "Litter"
  if (sp_split == "Cares") sp_split <- "Carex"
  if (sp_split == "Brock") sp_split <- "Rock"
  if (sp_split == "Erigeron") sp_split <- "Erigeron spp."
  if (sp_split == "Fragaria spp.") sp_split <- "Fragaria virginiana"
  if (sp_split == "Greas") sp_split <- "unk grass"
  if (sp_split == "Paronychia pulvinata ") sp_split <- "Paronychia pulvinata"
  if (sp_split == "Pn") sp_split <- "Poa spp."
  if (sp_split == "Potentilla unk") sp_split <- "Potentilla spp."
  if (sp_split == "Small 5 petal white unk") sp_split <- "unk white small 5 petal"
  if (sp_split == "Stp") sp_split <- "Stipa comata"
  if (sp_split == "Water/carex, juncus spp.") sp_split <- "Water/carex/juncus spp."
  if (sp_split == "Wild rose") sp_split <- "Rosa spp."
  if (sp_split == "Cerastium scariosum") sp_split <- "Cirsium scariosum"
  sp_split <- gsub("Achillia", "Achillea", sp_split)  ## brute force for when you have more than 1 word
  sp_split <- gsub("Anntenaria", "Antennaria", sp_split)
  sp_split <- gsub("Arrabis", "Arabis", sp_split)
  sp_split <- gsub("Artemesia", "Artemisia", sp_split)
  sp_split <- gsub("paryii", "parryi", sp_split)
  sp_split <- gsub("Erigonum", "Eriogonum", sp_split)
  sp_split <- gsub("Frageria", "Fragaria", sp_split)
  sp_split <- gsub("Hordium", "Hordeum", sp_split)
  sp_split <- gsub("Hymenoxia", "Hymenoxys", sp_split)
  sp_split <- gsub("Mulenberiga", "Muhlenbergia", sp_split)
  sp_split <- gsub("Muhlenberiga", "Muhlenbergia", sp_split)
  sp_split <- gsub("Pascopyron", "Pascopyrum", sp_split)
  sp_split <- gsub("Pascpyrum", "Pascopyrum", sp_split)
  sp_split <- gsub("smittii", "smithii", sp_split)
  sp_split <- gsub("flexilis.", "flexilis", sp_split)
  sp_split <- gsub("viscosissmum", "viscosissimum", sp_split)
  sp_split <- gsub("Ribres", "Ribes", sp_split)
  sp_split <- gsub("acquilis", "acaulis", sp_split)
  sp_split <- gsub("diveracarap", "divericarpa", sp_split)
  sp_split <- gsub("diveracarpa", "divericarpa", sp_split)
  sp_split <- gsub("divaricarpa", "divericarpa", sp_split)
  sp_split <- gsub("Tifolium", "Trifolium", sp_split)
  sp_split <- gsub("Trifollium", "Trifolium", sp_split)
  sp_split <- gsub("\\s{2,}", " ", sp_split)
  sp_split <- gsub("spp$", "spp.", sp_split)
  sp_split <- gsub("aqautic|aquatic", "", sp_split)
  sp_split <- gsub("/\\s+", "/", sp_split)
  sp_split <- gsub("(carex$|juncus$|draba$|eiger$)", "\\1 spp.", sp_split, ignore.case=TRUE)
  sp_split
}

veg$Species_clean <- sapply(veg$Species, clean_species)

length_veg <- sum(veg$Length)    ## finds the total length of all transects combined

total_length <- veg %>% group_by(Species_clean) %>%
  summarize(total_length = sum(Length),
            percent_length = sum(Length)/length_veg)

head(total_length)

write.csv(total_length, file="cleaned_total_length.csv", row.names=FALSE)

print(total_length, n= nrow(total_length))

str_trim(total_length$Species, side = "both")  ###one way to get rid of white spaces either side



