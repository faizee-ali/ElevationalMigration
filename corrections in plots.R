library(ggplot2)
library(dplyr)
ds <- c("05.q3", "50.q3", "95.q3")
minSample <- 60
group.05 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[1], ".sam", minSample, ".csv"))
group.50 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[2], ".sam", minSample, ".csv"))
group.95 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[3], ".sam", minSample, ".csv"))

# merge all shift measures into one data table
ds.elev1 <- as.data.frame(cbind(group.05$Species, group.05$LCI_diff ,group.05$Median_diff, group.05$UCI_diff,
                               group.50$LCI_diff , group.50$Median_diff ,group.50$UCI_diff ,
                               group.95$LCI_diff,group.95$Median_diff, group.95$UCI_diff,
                               group.05$Group, group.50$Group, group.95$Group, group.50$Median_S)) %>% na.omit()

colnames(ds.elev1) <- c("Species", "LCI_05", "Diff_05", "UCI_05", "LCI_50", "Diff_50", "UCI_50",
                       "LCI_95", "Diff_95", "UCI_95", "Group05", "Group50",
                       "Group95", "B50")

for (i in c("LCI_05", "Diff_05", "UCI_05", "LCI_50", "Diff_50", "UCI_50",
            "LCI_95", "Diff_95", "UCI_95", "B50")) {
    ds.elev1[[i]] <- as.numeric(ds.elev1[[i]])
}

# make species as factor (for the ordering in the plot)

# take only species which show significant shifts in upper and lower limits

ds.elev <- ds.elev1 %>%
    filter(ds.elev1$Group05 != 0 & ds.elev1$Group95 != 0)
ds.elev$Species <- factor(ds.elev$Species, levels = rev(ds.elev$Species[order(ds.elev$Diff_95)]))


# max and min y values of the plot
ds.elev$max <- as.numeric(apply(ds.elev[, 9:11], 1, max))
ds.elev$min <- as.numeric(apply(ds.elev[, 9:11], 1, min))

# bar width
if (minSample == 30) {
    if (substr(ds[[1]], 4, 5) == "q1") barwidth <- 4.4
    if (substr(ds[[1]], 4, 5) == "q2") barwidth <- 3.6
    if (substr(ds[[1]], 4, 5) == "q3") barwidth <- 3.0
} else barwidth <- 3.0

#setting color
barcol.u <- "#ff9999"
barcol.m <- "#aa60cc"
barcol.d <- "#9999ff"

#plot upper limit
p<-ggplot(data = ds.elev, aes(x = Species, y = Diff_95, ymin = min, ymax = max, family = "Calibri"))+
    geom_bar(stat="identity", fill = barcol.u, size = 0.5, color = "darkgreen", width = 1 ) + #barwidth
    geom_errorbar( aes(x=Species, ymin=LCI_95, ymax=UCI_95), width=0.3, colour="black")+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1, face = "italic", size = 16),
          axis.text.y = element_text(angle = 90, hjust = 0.5, size = 16),
          axis.title.y = element_text(size = 18),
          axis.title.x = element_text(size = 16)) +
    xlab(element_text("Species")) +
    ylab(element_text("Elevational Shift in Upper Distributional Limit (m)"))+
    geom_hline(yintercept = 100, linetype = "longdash", color = "#9999ff", size = 0.5)+
    geom_hline(yintercept = -100, linetype = "longdash", color = "#9999ff", size = 0.5)

ggsave(p, filename = paste0("Plots for ATBC/Upperlimit_", substr(ds[1], 4, 5), "n", minSample, "_sp", nrow(ds.elev), ".png"),
       width = 14, height = 9, units = "in", dpi = 600)


#plot lower limit
p<-ggplot(data = ds.elev, aes(x = Species, y = Diff_05, ymin = min, ymax = max, family = "Calibri"))+
    geom_bar(stat="identity", fill = barcol.d, size = 0.5, color = "darkgreen", width = 1 ) + #barwidth
    geom_errorbar( aes(x=Species, ymin=LCI_05, ymax=UCI_05), width=0.3, colour="black")+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1, face = "italic", size = 16),
          axis.text.y = element_text(angle = 90, hjust = 0.5, size = 16),
          axis.title.y = element_text(size = 18),
          axis.title.x = element_text(size = 16)) +
    xlab(element_text("Species")) +
    ylab(element_text("Elevational Shift in Lower Distributional Limit (m)"))+
    geom_hline(yintercept = -100, linetype = "longdash", color = "#ff9999", size = 0.5)+
    geom_hline(yintercept = 100, linetype = "longdash", color = "#ff9999", size = 0.5)

ggsave(p, filename = paste0("Plots for ATBC/Lowerlimit_", substr(ds[1], 4, 5), "n", minSample, "_sp", nrow(ds.elev), "1.png"),
       width = 14, height = 7, units = "in", dpi = 600)



#plot median shift
p<-ggplot(data = ds.elev, aes(x = Species, y = Diff_50, ymin = min, ymax = max, family = "Calibri"))+
    geom_bar(stat="identity", fill = barcol.m, size = 0.5, color = "darkgreen", width = 1 ) + #barwidth
    geom_errorbar( aes(x=Species, ymin=LCI_50, ymax=UCI_50), width=0.3, colour="black")+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic"),
          axis.text.y = element_text(angle = 90, hjust = 0.5, size = 18),
          axis.title.y = element_text(size = 18)) +
    xlab(element_text("Species")) +
    ylab(element_text("Elevational Shift in the Median of Distribution(m)"))

ggsave(p, filename = paste0("migration_plot/medianshift_", substr(ds[1], 4, 5), "n", minSample, "_sp", nrow(ds.elev), ".png"),
       width = 16, height = 9, units = "in", dpi = 600)