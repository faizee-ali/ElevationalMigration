
# load libraries
library(sf)
library(dplyr)
library(spThin)
library(readr)
# load the filtered ebird  dataset
ebd <- read.delim("results/ebd_SWG_filtered.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
#extract the all localities
LOCALITY.ID <- ebd$LOCALITY.ID
LATITUDE<- as.numeric(ebd$LATITUDE)
LONGITUDE<- ebd$LONGITUDE
loc <- subset.data.frame(ebd, select = c(LOCALITY.ID, LATITUDE, LONGITUDE))
# load sheet of all localties

loc_unique <- loc %>% distinct()

loc_unique$species <- "species"

# carry out spatial thinning with a minimum distance of 0.2km between records
rm(ebd,LOCALITY.ID,LATITUDE,LONGITUDE,loc)
thinned <- thin(loc_unique,lat.col="LATITUDE",long.col = "LONGITUDE",
                spec.col = "species",thin.par = 0.2,reps=10,
                write.files=T, 
                out.dir="data/",
                out.base="localities-thinned")

ebd_sam <- read.delim("ebd_sample_SWG_filtered.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))

local <- read.delim("data/localities-thinned_thin1.csv", sep = ",", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))


thinned_ebd_sam <- ebd_sam %>%
    filter(LONGITUDE %in% local$LONGITUDE, LATITUDE %in% local$LATITUDE)
seid <- unique(thinned_ebd_sam$SAMPLING.EVENT.IDENTIFIER)
thinned_ebd = ebd %>% filter(SAMPLING.EVENT.IDENTIFIER %in% seid)


print(nrow(thinned_ebd_sam)/65462)

write_tsv(
    thinned_ebd,
    "results/ebd_final_thinned_200m.txt",
    na = "NA",
    append = FALSE,
    col_names = TRUE,
    quote = "none",
    escape = c("double", "backslash", "none"),
    eol = "\n",
    num_threads = readr_threads(),
    progress = show_progress(),
)

write_tsv(
    thinned_ebd_sam,
    "results/ebd_final_thinned_sam_200m.txt",
    na = "NA",
    append = FALSE,
    col_names = TRUE,
    quote = "none",
    escape = c("double", "backslash", "none"),
    eol = "\n",
    num_threads = readr_threads(),
    progress = show_progress(),
)

