library(readr)
library(dplyr)
#extract the all localities

ebd_sam <- read.delim("ebd_sample_SWG_filtered.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))
ebd <- read.delim("ebd_SWG_filtered.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))



local <- read.delim("data/localities-thinned_thin2.csv", sep = ",", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))


thinned_ebd_sam <- ebd_sam %>%
    filter(LONGITUDE %in% local$LONGITUDE, LATITUDE %in% local$LATITUDE)
seid <- unique(thinned_ebd_sam$SAMPLING.EVENT.IDENTIFIER)
thinned_ebd = ebd %>% filter(SAMPLING.EVENT.IDENTIFIER %in% seid)


print(nrow(thinned_ebd_sam)/65462)

write_tsv(
    thinned_ebd,
    "ebd_thinned_200m.txt",
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
    "ebd_thinned_sam_200m.txt",
    na = "NA",
    append = FALSE,
    col_names = TRUE,
    quote = "none",
    escape = c("double", "backslash", "none"),
    eol = "\n",
    num_threads = readr_threads(),
    progress = show_progress(),
)
