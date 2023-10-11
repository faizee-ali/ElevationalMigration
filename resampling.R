 

library(readr)
library(dplyr)

dat <- read.delim("results/ebd_SWG_filtered.txt", sep = "\t", header = T, quote = "", stringsAsFactors = F, na.strings = c(""," ",NA))


## include only full species
dat <- dat %>%
    filter(CATEGORY == "species" | CATEGORY == "issf")


### Creating Resampled datasets

# Creating resampled Dataset for the centre, lower, and upper limit of a species' elevational distribution for 3 levels of sampling effort (number of checklists)- separately for east and west 



## Number of checklists in either season in each elevational band


Checklists <- dat[!duplicated(dat$group.id), ]
Checklists$elev_level <- cut(Checklists$elevation, breaks = c(-Inf, 400, 800, 1200, 1600, 2000, 2400, Inf), labels = 1:7)
Checklists.S <- subset(Checklists, month %in% 3:5) # breeding/summer
Checklists.W <- subset(Checklists, month %in% c(11, 1, 12)) # winter

summer <- Checklists.S %>% 
    group_by(elev_level) %>% 
    summarise(summer = n_distinct(group.id))

winter <- Checklists.W %>% 
    group_by(elev_level) %>% 
    summarise(winter = n_distinct(group.id))

## if you want to output a table the number of checklists at each elevation band and season
CL_ES <- left_join(summer, winter, by = "elev_level")
levels(CL_ES$elev_level) <- c("0-400","400-800","800-1200","1200-1600","1600-2000","2000-2400",">2400")
write_tsv(
    CL_ES,
    "results/thinned checklists_elevation unthinned.txt",
    na = "NA",
    append = FALSE,
    col_names = TRUE,
    quote = "none",
    escape = c("double", "backslash", "none"),
    eol = "\n",
    num_threads = readr_threads(),
    progress = show_progress(),
)
#write.csv(CL_ES, "ChecklistNo_SeasonElevation.csv", row.names = F)

## Sampling event IDs in each season and in each elevation band
ID.S <- lapply(1:7, function(x) Checklists.S$group.id[Checklists.S$elev_level == x])
ID.W <- lapply(1:7, function(x) Checklists.W$group.id[Checklists.W$elev_level == x])

## Get unique species list
uniSpe <- dat %>% filter(CATEGORY == "species" | CATEGORY == "issf")
uniSpe <- unique(uniSpe[, "COMMON.NAME"]) %>% data.frame()

## Get first second and third quartile of effort (number of checklists) across season and elevation
efforts.S <- summary(Checklists.S$elev_level)
efforts.W <- summary(Checklists.W$elev_level)
qEffort <- quantile(c(efforts.S, efforts.W), c(0.25,0.50,0.75))

effort.summ <- as.data.frame(cbind(c("0-400","400-800","800-1200","1200-1600","1600-2000","2000-2400",">2400"), efforts.S, efforts.W)) 
write_tsv(
    effort.summ,
    "results/thinned effort summary.txt",
    na = "NA",
    append = FALSE,
    col_names = TRUE,
    quote = "none",
    escape = c("double", "backslash", "none"),
    eol = "\n",
    num_threads = readr_threads(),
    progress = show_progress(),
)

# resample for the lower limit (5th percentile), center (median), and upper limit (95th percentile) for a species elevational distribution
for (qt in c(0.95)) {
    #resample for the levels of sampling effort
    for (i in 3) {
        
        ## sample an equal number of checklists (3 levels of effort) from each elevation band 
        set.seed(56789)
        sampleID.S <- lapply(1:100, function(y) {unlist(lapply(1:7, function(x) sample(ID.S[[x]], qEffort[i], replace=T)))})
        set.seed(56789)
        sampleID.W <- lapply(1:100, function(y) {unlist(lapply(1:7, function(x) sample(ID.W[[x]], qEffort[i], replace=T)))})
        
        # calculate the lower, median and upper elevation distribution for each bird species in the two seasons
        
        # This step takes awhile
        # We used parallel processing
        
        rs<- lapply(1:100, function(y) {
            m <- t(sapply(uniSpe[, 1], function(x){
                occs.S <- subset(dat, COMMON.NAME ==x & group.id %in% sampleID.S[[y]], select="elevation")
                occs.W <- subset(dat, COMMON.NAME==x & group.id %in% sampleID.W[[y]], select="elevation")
                B.n <- nrow(occs.S)
                W.n <- nrow(occs.W)
                B.elev <- if (B.n > 0) quantile(occs.S$elevation, qt) else NA
                W.elev <- if (W.n > 0) quantile(occs.W$elevation, qt) else NA
                return(c(B.elev = B.elev, B.n = B.n, W.elev = W.elev, W.n = W.n))
            }))
        })
        print(i)
        
        save(rs,file = paste0("eBird_final_resampled_", sprintf("%02d", qt*100),".q", i ,".RData"))
    }
    print(qt)
}


load("resample/eBird_final_resampled_95.q3.Rdata")
