#### for poster getting some stats ####
library(dplyr)
library(readr)
localities<-read.csv("data/localities-thinned_thin1.csv")
ebd<- read.delim("data/ebd_final_thinned_200m.txt", sep ="\t")
ebd_sam<- read.delim("data/ebd_final_thinned_sam_200m.txt", sep ="\t")
checklists <- unique(ebd$group.id)
rm(ebd_sam)

elev_check <- read.delim("results/checklists_elevation.txt", sep = "\t")
effort <- read.delim("results/checklists_elevation.txt", sep = "\t")

results95 <- read.csv("results/extent/final_birdlist_filtered95.q3.sam30.csv")
results05 <- read.csv("results/extent/final_birdlist_filtered05.q3.sam30.csv")
results50 <- read.csv("results/extent/final_birdlist_filtered50.q3.sam30.csv")
results <- left_join(results05,results50, by = "Species")
results <- left_join(results, results95, by = "Species")

# x = lower, y = median, _ = upper
# lower limit, median and upper limit moving up in winter
cmig <- results%>%
    filter((Group.x == 1 & Group.y == 1 & Group == 1) |
               (Group.x == 2 & Group == 2 & Group.y == 2))

cupmig <- results %>% 
    filter(Group.x == 1 & Group.y == 1 & Group == 1)

cdownmig <- results %>%
    filter(Group.x == 2 & Group == 2 & Group.y == 2)

nomig <- results %>% 
    filter(Group.x == 0, Group.y == 0, Group == 0)

## expand: upper boundary moving up and lower boundary moving down
# in winter

expand <- results %>%
    filter(Group.x == 2 & Group == 1)

# shrink: upper boundary moving down and lower boundary moving up
# in winter

Shrink <- results %>%
    filter(Group.x == 1 & Group == 2)

# simply median shift

median <- results %>%
    filter(Group.y == 1 |Group.y == 2)

#median shift  by 200m and more

median200 <- results %>%
    filter(Median_diff.y >200 | Median_diff.y < -200)

uplimit200 <- results %>%
    filter(Median_diff >200 | Median_diff < -200)


dnlimit200 <- results %>%
    filter(Median_diff.x >200 | Median_diff.x < -200)

# up and down migration but only limits up or down in winter

limits <- results %>% 
    filter(Group.x != 0 & Group != 0)

downlimits <- results %>% 
    filter(Group.x == 2 & Group == 2)
nomiglim <- results %>%
    filter(Group.x == 0 & Group == 0)

# shifting only upper limit
ulsd <- results %>% 
    filter(Group.y == 2) # shifting down

ulsu <- results %>% 
    filter(Group.y == 1) # shifting up

write_tsv(
    cdownmig,
    "stats/complete down mig data.txt",
    na = "NA",
    append = FALSE,
    col_names = TRUE,
    quote = "none",
    escape = c("double", "backslash", "none"),
    eol = "\n",
    num_threads = readr_threads(),
    progress = show_progress(),
)
