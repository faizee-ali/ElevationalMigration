library(dplyr)
library(readr)
#first splitting ebd into swg
ebd <- read.delim("ebd_SWG.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
sampl <- read.delim("ebd_sample_SWG.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
# filtering species from the ebird database
species <- read.csv("filtering sp. - final list 1.1.csv")
ebd_SWG_filtered = ebd %>% filter(COMMON.NAME %in% species$eBird.English.Name.2021)

seid <- unique(ebd_SWG_filtered$SAMPLING.EVENT.IDENTIFIER)
ebd_sam = sampl %>% filter(SAMPLING.EVENT.IDENTIFIER %in% seid)

temp_ebd_sam <- ebd_sam %>%
    filter(month %in% c(3,4,5,11,12,1))

seid <- unique(temp_ebd_sam$SAMPLING.EVENT.IDENTIFIER)
temp_ebd = ebd %>% filter(SAMPLING.EVENT.IDENTIFIER %in% seid)


write_tsv(
    temp_ebd,
    "ebd_SWG_filtered.txt",
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
    temp_ebd_sam,
    "ebd_sample_SWG_filtered.txt",
    na = "NA",
    append = FALSE,
    col_names = TRUE,
    quote = "none",
    escape = c("double", "backslash", "none"),
    eol = "\n",
    num_threads = readr_threads(),
    progress = show_progress(),
)

rm(ebd, sampl, species, seid)
