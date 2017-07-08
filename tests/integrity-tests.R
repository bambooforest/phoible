# Integrity tests on the PHOIBLE aggregated data
# Steven Moran <steven.moran@uzh.ch>
# setwd("/Users/stiv/Github/phoible/")

library(dplyr)

# TODO:
# library(testthat)

# Load data
## Global
load(url("https://github.com/phoible/dev/raw/master/data/phoible-by-phoneme.RData")) # final.data
## Local
load("data/phoible-by-phoneme.RData") # final.data
glimpse(final.data); dim(final.data) # 105092 46 with ER inventories

# Test if language codes (LanguageCode) in PHOIBLE are valid with ISO 639-3
phoible.unique.iso <- distinct(select(final.data, LanguageCode))
head(phoible.unique.iso); dim(phoible.unique.iso)
iso639.3 <- read.csv("http://www-01.sil.org/iso639-3/iso-639-3.tab", sep="\t")
unique.iso <- unique(iso639.3$Id)
phoible.unique.iso$in.iso <- phoible.unique.iso$LanguageCode %in% unique.iso
phoible.unique.iso %>% filter(!in.iso) # 14 records

# Tests if trump order is working correctly
trumped <- subset(final.data, Trump)
# trump contains all ISO codes that we have:
length(unique(final.data$LanguageCode)) == length(unique(trumped$LanguageCode)) # TRUE
# trump contains the same number of InventoryIDs as ISO codes
length(unique(trumped$LanguageCode)) == length(unique(trumped$InventoryID)) # TRUE
# trump has exactly 1 ISO code for each InventoryID
with(trumped, all(rowSums(table(LanguageCode, InventoryID) > 0) == 1)) # TRUE
# trump has exactly 1 InventoryID for each ISO code
with(trumped, all(colSums(table(LanguageCode, InventoryID) > 0) == 1)) # TRUE
length(unique(final.data$LanguageCode)) # 2055
length(unique(final.data$LanguageCode[final.data$Trump==TRUE])) # 2055

# There should be one Trump=TRUE Hawaiian inventory
final.data %>% filter(LanguageCode=="haw") %>% select(InventoryID, Source, Trump) %>% unique()
#    InventoryID Source Trump
# 1          352  upsid FALSE
# 14          43    spa  TRUE

# There should be one Trump=True [ell] inventory
final.data %>% filter(LanguageCode=="ell") %>% select(InventoryID, Source, Trump) %>% unique()
# 1 170 spa FALSE
# 27 2186 uz FALSE
# 68 2187 uz FALSE
# 91 2458 ea FALSE
# 120 318 upsid FALSE

# Some testing ideas:
## dump a unique list of segments after checking them
## basic stats on features, languages, etc.
## basic coverage of area, language family, etc.

