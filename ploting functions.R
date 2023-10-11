###### plot distribution range for each species ######
library(tidyverse)
library(ggplot2)
library(dplyr)
saveMigrationPlot <- function(ds, minSample) {
    
    # import data
    
    group.05 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[1], ".sam", minSample, ".csv"))
    group.50 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[2], ".sam", minSample, ".csv"))
    group.95 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[3], ".sam", minSample, ".csv"))
    
    # merge all shift measures into one data table
    ds.elev <- as.data.frame(cbind(group.05$Species, group.05$Median_S,group.05$Median_W, group.50$Median_S,
                                   group.50$Median_W, group.95$Median_S, group.95$Median_W, group.05$Group,
                                   group.50$Group, group.95$Group)) %>% na.omit()
    
    colnames(ds.elev) <- c("Species", "B05", "W05", "B50", "W50", "B95", "W95", "Group05", "Group50", "Group95")
    for (i in c("B05", "W05", "B50", "W50", "B95", "W95")) {
        ds.elev[[i]] <- as.numeric(ds.elev[[i]])
    }
    ## create data for plotting
    # grouping (up-migration, down-migration)
    ds.elev$Group50 <- as.factor(ds.elev$Group50)
    ds.elev$Group05 <- as.factor(ds.elev$Group05)
    ds.elev$Group95 <- as.factor(ds.elev$Group95)
    
    # y values of the bars
    ds.elev$e1 <- apply(ds.elev[, c("B05", "W05")], 1, min)
    ds.elev$e2 <- apply(ds.elev[, c("B05", "W05")], 1, max)
    ds.elev$e3 <- apply(ds.elev[, c("B95", "W95")], 1, min)
    ds.elev$e4 <- apply(ds.elev[, c("B95", "W95")], 1, max)
    
    # color and alpha
    ds.elev$col12 <- ifelse(ds.elev$B05 < ds.elev$W05, "3", "4")
    ds.elev$col34 <- ifelse(ds.elev$B95 > ds.elev$W95, "3", "4")
    ds.elev$colmed <- ifelse(ds.elev$B50 > ds.elev$W50, "1", "2")
    ds.elev$alphamed <- ifelse(ds.elev$Group50 == "0", "2", "1")
    
    # make species as factor (for the ordering in the plot)
    ds.elev$Species <- factor(ds.elev$Species, levels = ds.elev$Species[order(ds.elev$Species)])
    
    # max and min y values of the plot
    ds.elev$max <- apply(ds.elev[, 2:7], 1, max)
    ds.elev$min <- apply(ds.elev[, 2:7], 1, min)
    
    # bar width
    if (minSample == 30) {
        if (substr(ds[[1]], 4, 5) == "q1") barwidth <- 4.4
        if (substr(ds[[1]], 4, 5) == "q2") barwidth <- 3.6
        if (substr(ds[[1]], 4, 5) == "q3") barwidth <- 3.3
    } else barwidth <- 3.7
    
    
    # plot and save distribution range
    p <- ggplot(data = ds.elev, aes(x = Species, ymin = min, ymax = max, family = "Calibri")) +
        geom_linerange(aes(x = Species, ymin = e3, ymax = e4, color = col34), linewidth = barwidth) +
        geom_linerange(aes(x = Species, ymin = e1, ymax = e2, color = col12), linewidth = barwidth) +
        geom_linerange(aes(x = Species, ymin = e2, ymax = e3), color = "#aa60cc", linewidth = barwidth) +
        geom_segment(aes(x = Species, xend = Species, y = B50, yend = W50, color = colmed, alpha = alphamed), size = 0.8,
                     lineend = "round", linejoin = "round", arrow = arrow(length = unit(5, "pt"))) +
        scale_color_manual(values = c("white", "black", "#ff9999", "#9999ff")) +
        scale_alpha_manual(values = c(1, 0)) +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic", size = 11),
              axis.text.y = element_text(angle = 90, hjust = 0.5, size = 16),
              axis.title.y = element_text(size = 18),
              axis.title.x = element_text(size = 18),
              panel.grid.major.x = element_line(color = "gray")) +
        xlab(element_text("Species")) +
        ylab(element_text("Elevation (m)"))
    
    ggsave(p, filename = paste0("reorderedmigration_", substr(ds[1], 4, 5), "n", minSample, "_sp", nrow(ds.elev), " grid.png"),
           width = 16, height = 9, units = "in", dpi = 900)
}
#### filtering out species ####


saveMigrationPlot(c("05.q3", "50.q3", "95.q3"), 60) 




###### plot shifts in median upper and lower limits for each species ######
saveMedianPlot <- function(ds, minSample){
    group.05 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[1], ".sam", minSample, ".csv"))
    group.50 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[2], ".sam", minSample, ".csv"))
    group.95 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[3], ".sam", minSample, ".csv"))
    
    # merge all shift measures into one data table
    ds.elev <- as.data.frame(cbind(group.05$Species, group.05$LCI_diff ,group.05$Median_diff, group.05$UCI_diff,
                                   group.50$LCI_diff , group.50$Median_diff ,group.50$UCI_diff ,
                                   group.95$LCI_diff,group.95$Median_diff, group.95$UCI_diff,
                                   group.05$Group, group.50$Group, group.95$Group, group.50$Median_S)) %>% na.omit()
    
    colnames(ds.elev) <- c("Species", "LCI_05", "Diff_05", "UCI_05", "LCI_50", "Diff_50", "UCI_50",
                           "LCI_95", "Diff_95", "UCI_95", "Group05", "Group50",
                           "Group95", "B50")
    
    for (i in c("LCI_05", "Diff_05", "UCI_05", "LCI_50", "Diff_50", "UCI_50",
                "LCI_95", "Diff_95", "UCI_95", "B50")) {
        ds.elev[[i]] <- as.numeric(ds.elev[[i]])
    }
    
    # make species as factor (for the ordering in the plot)
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
    
    #plot lower limit
    p<-ggplot(data = ds.elev, aes(x = Species, y = Diff_05, ymin = min, ymax = max, family = "Calibri"))+
        geom_bar(stat="identity", fill = barcol.d, size = 0.5, color = "darkgreen", width = 1 ) + #barwidth
        geom_errorbar( aes(x=Species, ymin=LCI_05, ymax=UCI_05), width=0.3, colour="black")+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic"),
              axis.text.y = element_text(angle = 90, hjust = 0.5, size = 18),
              axis.title.y = element_text(size = 18)) +
        xlab(element_text("Species")) +
        ylab(element_text("Elevational Shift in Lower Distributional Limit (m)"))
    ggsave(p, filename = paste0("migration_plot/lowerlimit_", substr(ds[1], 4, 5), "n", minSample, "_sp", nrow(ds.elev), ".png"),
           width = 16, height = 9, units = "in", dpi = 600)
    #plot upper limit
    p<-ggplot(data = ds.elev, aes(x = Species, y = Diff_95, ymin = min, ymax = max, family = "Calibri"))+
        geom_bar(stat="identity", fill = barcol.u, size = 0.5, color = "darkgreen", width = 1 ) + #barwidth
        geom_errorbar( aes(x=Species, ymin=LCI_95, ymax=UCI_95), width=0.3, colour="black")+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic"),
              axis.text.y = element_text(angle = 90, hjust = 0.5, size = 18),
              axis.title.y = element_text(size = 18)) +
        xlab(element_text("Species")) +
        ylab(element_text("Elevational Shift in Upper Distributional Limit (m)"))
    
    ggsave(p, filename = paste0("migration_plot/Upperlimit_", substr(ds[1], 4, 5), "n", minSample, "_sp", nrow(ds.elev), ".png"),
           width = 16, height = 9, units = "in", dpi = 600)
    
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
    
}

saveMedianPlot(c("05.q3", "50.q3", "95.q3"), 30)



#####plotting PGLS output #####

model <- read.csv("compile_SWGmodels.csv")

##### function for plotting gls data #####
glsplotfunc <- function(minSample){
    model <- read.csv("compile_SWGmodels.csv")
    
    model.filtered <- model %>%
        filter(!Variable %in% "(Intercept)") %>%
        filter(substr(dataset, 10,11) == minSample)
    
    # useful labels = rownames(ds)[order(ds$x[1:(nrow(ds)/3)]
    model.filtered <- mutate(model.filtered, Variable = fct_recode(Variable, "Invertebrates" = "Dietinvertebrates", "Omnivore" = "Dietomnivore", "Seeds"="Dietseeds", "Vertebrates"="Dietvertebrates", "Body Mass" = "bodymass"))
    
    model.05 <- model.filtered%>% filter(substr(dataset,1,2) == "05")
    
    model.50 <- model.filtered%>% filter(substr(dataset,1,2) == "50")
    
    model.95 <- model.filtered%>% filter(substr(dataset,1,2) == "95")
    
    model.05$col <- ifelse(substr(model.05$dataset,4,5)=="q1","1",ifelse(substr(model.05$dataset,4,5)=="q2","2","3"))
    
    model.50$col <- ifelse(substr(model.50$dataset,4,5)=="q1","1",ifelse(substr(model.50$dataset,4,5)=="q2","2","3"))
    
    model.95$col <- ifelse(substr(model.95$dataset,4,5)=="q1","1",ifelse(substr(model.95$dataset,4,5)=="q2","2","3"))
    
    ### For upper limit
    model.95 <- model.95[order(model.95$Variable),]
    
    v <- c(0.9, 1, 1.1, 1.9, 2, 2.1, 2.9, 3, 3.1, 3.9, 4, 4.1, 4.9, 5, 5.1, 5.9, 6, 6.1, 6.9, 7, 7.1)
    
    index <- model.95$p.value < 0.05
    
    
    p <- ggplot(model.95, aes(Variable, Value), family = "Calibri") +
        geom_hline(yintercept = 0, linetype = "longdash", color = "red", size = 0.5)+
        geom_pointrange(
            aes(ymin = Value-Std.Error, ymax = Value+Std.Error+2, 
                color = col),
            size = 0.1,
            position = position_dodge(0.3)
        )+
        scale_color_manual(values = c("gray30", "gray40","gray50"))+
        annotate("text", x = v[index], y = model.95$Value[index] + 2, label = "*", size = 5, vjust = 0)+
        theme_bw()+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 6),
              axis.text.y = element_text(angle = 0, hjust = 0.5, size = 7),
              axis.title.y = element_text(size = 10),
              axis.title.x = element_text(size = 10),
              plot.title = element_text(color = "#1B6B93", size = 12),
              panel.grid = element_blank()) +
        xlab(element_text("Species Traits")) +
        ylab(element_text("Relative effect on seasonal shift (m)"))+
        labs(title = "Upper Limit")
    ggsave(p, filename = paste0("UL pgls for minsample",minSample,".png"),
           width = 4, height = 3, units = "in", dpi = 600)
    
    ### for median
    model.50 <- model.50[order(model.50$Variable),]
    
    v <- c(0.9, 1, 1.1, 1.9, 2, 2.1, 2.9, 3, 3.1, 3.9, 4, 4.1, 4.9, 5, 5.1, 5.9, 6, 6.1, 6.9, 7, 7.1)
    
    index <- model.50$p.value < 0.05
    
    
    p <- ggplot(model.50, aes(Variable, Value), family = "Calibri") +
        geom_hline(yintercept = 0, linetype = "longdash", color = "red", size = 0.5)+
        geom_pointrange(
            aes(ymin = Value-Std.Error, ymax = Value+Std.Error+2, 
                color = col),
            size = 0.1,
            position = position_dodge(0.3)
        )+
        scale_color_manual(values = c("gray30", "gray40","gray50"))+
        annotate("text", x = v[index], y = model.50$Value[index] + 2, label = "*", size = 5, vjust = 0)+
        theme_bw()+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 6),
              axis.text.y = element_text(angle = 0, hjust = 0.5, size = 7),
              axis.title.y = element_text(size = 10),
              axis.title.x = element_text(size = 10),
              plot.title = element_text(color = "#1B6B93", size = 12),
              panel.grid = element_blank()) +
        xlab(element_text("Species Traits")) +
        ylab(element_text("Relative effect on seasonal shift (m)"))+
        labs(title = "Median")
    ggsave(p, filename = paste0("Median pgls for minsample",minSample,".png"),
           width = 4, height = 3, units = "in", dpi = 600)
    
    ### for lower limit
    
    model.05 <- model.05[order(model.05$Variable),]
    
    v <- c(0.9, 1, 1.1, 1.9, 2, 2.1, 2.9, 3, 3.1, 3.9, 4, 4.1, 4.9, 5, 5.1, 5.9, 6, 6.1, 6.9, 7, 7.1)
    
    index <- model.05$p.value < 0.05
    
    
    p <- ggplot(model.05, aes(Variable, Value), family = "Calibri") +
        geom_hline(yintercept = 0, linetype = "longdash", color = "red", size = 0.5)+
        geom_pointrange(
            aes(ymin = Value-Std.Error, ymax = Value+Std.Error+2, 
                color = col),
            size = 0.1,
            position = position_dodge(0.3)
        )+
        scale_color_manual(values = c("gray30", "gray40","gray50"))+
        annotate("text", x = v[index], y = model.05$Value[index] + 2, label = "*", size = 5, vjust = 0)+
        theme_bw()+
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 6),
              axis.text.y = element_text(angle = 0, hjust = 0.5, size = 7),
              axis.title.y = element_text(size = 10),
              axis.title.x = element_text(size = 10),
              plot.title = element_text(color = "#1B6B93", size = 12),
              panel.grid = element_blank()) +
        xlab(element_text("Species Traits")) +
        ylab(element_text("Relative effect on seasonal shift (m)"))+
        labs(title = "Lower Limit")
    ggsave(p, filename = paste0("LL pgls for minsample",minSample,".png"),
           width = 4, height = 3, units = "in", dpi = 600)
    
}
glsplotfunc(30)


##### plotting temperature regime vs elevational shift ####
library(hrbrthemes)
data.95 <- read.csv("results/pgls/SWG_ordered95.q3.sam30.csv")
data.05 <- read.csv("results/pgls/SWG_ordered05.q3.sam30.csv")
data.50 <- read.csv("results/pgls/SWG_ordered50.q3.sam30.csv")

data<- as.data.frame(cbind(data.05$Species.x, data.05$maxTemp_Dec_mean, data.05$minTemp_Dec_mean,
                           data.05$maxTemp_May_mean, data.05$minTemp_May_mean,
                           data.50$maxTemp_Dec_mean, data.50$minTemp_Dec_mean,
                           data.50$maxTemp_May_mean, data.50$minTemp_May_mean,
                           data.95$maxTemp_Dec_mean, data.95$minTemp_Dec_mean,
                           data.95$maxTemp_May_mean, data.95$minTemp_May_mean,
                           data.05$ThermalRegime, data.50$ThermalRegime, data.95$ThermalRegime,
                           data.05$Median_diff, data.50$Median_diff, data.95$Median_diff
))


colnames(data) <- c("data.05$Species.x", "data.05$maxTemp_Dec_mean", "data.05$minTemp_Dec_mean",
                    "data.05$maxTemp_May_mean", "data.05$minTemp_May_mean",
                    "data.50$maxTemp_Dec_mean", "data.50$minTemp_Dec_mean",
                    "data.50$maxTemp_May_mean", "data.50$minTemp_May_mean",
                    "data.95$maxTemp_Dec_mean", "data.95$minTemp_Dec_mean",
                    "data.95$maxTemp_May_mean", "data.95$minTemp_May_mean",
                    "data.05$ThermalRegime", "data.50$ThermalRegime", "data.95$ThermalRegime",
                    "data.05$Median_diff", "data.50$Median_diff", "data.95$Median_diff")


columns_to_convert <- c("data.05$maxTemp_Dec_mean", "data.05$minTemp_Dec_mean",
                        "data.05$maxTemp_May_mean", "data.05$minTemp_May_mean",
                        "data.50$maxTemp_Dec_mean", "data.50$minTemp_Dec_mean",
                        "data.50$maxTemp_May_mean", "data.50$minTemp_May_mean",
                        "data.95$maxTemp_Dec_mean", "data.95$minTemp_Dec_mean",
                        "data.95$maxTemp_May_mean", "data.95$minTemp_May_mean",
                        "data.05$ThermalRegime", "data.50$ThermalRegime", "data.95$ThermalRegime",
                        "data.05$Median_diff", "data.50$Median_diff", "data.95$Median_diff")

# Loop over the columns and convert them to numeric
for (col in columns_to_convert) {
    data[[col]] <- as.numeric(data[[col]])
}

tempmax <- apply(data[,2:13],1,max)
tempmin <- apply(data[,2:13],1,min)
TR<- tempmax-tempmin
data$SpeciesTR <- TR
tokeep <- c(1,17,18,19,20)
data1 <- data[,tokeep]
library(reshape2)
library(tidyverse)

df <- melt(data1 ,  id.vars = 'SpeciesTR', variable.name = 'series', measure.vars = c("data.05$Median_diff", "data.50$Median_diff", "data.95$Median_diff"))

ds.elev$colmed <- ifelse(ds.elev$B50 > ds.elev$W50, "1", "2")
df$series <- fct_recode(df$series, "1" = "data.05$Median_diff", "2" = "data.50$Median_diff", "3" = "data.95$Median_diff")
df$series2 <- fct_recode(df$series, "#9999ff" = "1", "#aa60cc" = "2", "#ff9999" = "3")



p <- ggplot(df, aes(SpeciesTR, value), family = "Calibri") +
    geom_point(aes(color = series, shape = series), size = 0.5)+
    geom_smooth(method=lm, aes(color = series, fill = series), linetype = "longdash", size = 0.5)+
    theme_bw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 6),
          axis.text.y = element_text(angle = 0, hjust = 0.5, size = 6),
          axis.title.y = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          panel.grid = element_blank()) +
    scale_color_manual(values = c("#9999ff", "#aa60cc", "#ff9999"))+
    xlab(element_text(paste("Thermal Regime", "\u00B0C"))) +
    ylab(element_text("Elevational shift(m)"))

ggsave(p, filename = paste0("mergerdplot for shiftand TR",".png"),
       width = 4, height = 3, units = "in", dpi = 600)    


p1 <- ggplot(data, aes(x=SpeciesTR, y=`data.95$Median_diff`), family = "Calibri") +
    geom_point(aes(color = "#ff9999"), size = 1) +
    geom_smooth(method=lm, color = "#ff9999",se=TRUE,linetype = "longdash", size = 1) +
    theme_ipsum()+
    theme_bw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10),
          axis.text.y = element_text(angle = 0, hjust = 0.5, size = 10),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          plot.title = element_text(color = "#1B6B93", size = 16),
          panel.grid = element_blank()) +
    xlab(element_text(paste("Thermal Regime", "\u00B0C"))) +
    ylab(element_text("Elevational shift(m)"))+
    labs(title = "Upper Limit")
p1

ggsave(p1, filename = paste0("Upperlimit shiftand TR",".png"),
       width = 4, height = 3, units = "in", dpi = 600) 


p2 <- ggplot(data, aes(x=SpeciesTR, y=`data.05$Median_diff`), family = "Calibri") +
    geom_point(color = "#9999ff", size = 1) +
    geom_smooth(method=lm, color = "#9999ff",se=TRUE,linetype = "longdash", size = 1) +
    theme_ipsum()+
    theme_bw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 10),
          axis.text.y = element_text(angle = 0, hjust = 0.5, size = 10),
          axis.title.y = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          plot.title = element_text(color = "#1B6B93", size = 16),
          panel.grid = element_blank()) +
    xlab(element_text(paste("Thermal Regime", "\u00B0C"))) +
    ylab(element_text("Elevational shift(m)"))+
    labs(title = "Lower Limit")
p2

ggsave(p2, filename = paste0("Lower shift and TR",".png"),
       width = 4, height = 3, units = "in", dpi = 600)    

#### zoomed in ####
saveMigrationPlot <- function(ds, minSample) {
    
    # import data
    
    group.05 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[1], ".sam", minSample, ".csv"))
    group.50 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[2], ".sam", minSample, ".csv"))
    group.95 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[3], ".sam", minSample, ".csv"))
    
    # merge all shift measures into one data table
    ds.elev <- as.data.frame(cbind(group.05$Species, group.05$Median_S,group.05$Median_W, group.50$Median_S,
                                   group.50$Median_W, group.95$Median_S, group.95$Median_W, group.05$Group,
                                   group.50$Group, group.95$Group)) %>% na.omit()
    
    colnames(ds.elev) <- c("Species", "B05", "W05", "B50", "W50", "B95", "W95", "Group05", "Group50", "Group95")
    for (i in c("B05", "W05", "B50", "W50", "B95", "W95")) {
        ds.elev[[i]] <- as.numeric(ds.elev[[i]])
    }
    
    ds.elev <- ds.elev %>%
        filter(Group95 != 0 & Group05 != 0)
    ## create data for plotting
    # grouping (up-migration, down-migration)
    ds.elev$Group50 <- as.factor(ds.elev$Group50)
    ds.elev$Group05 <- as.factor(ds.elev$Group05)
    ds.elev$Group95 <- as.factor(ds.elev$Group95)
    
    # y values of the bars
    ds.elev$e1 <- apply(ds.elev[, c("B05", "W05")], 1, min)
    ds.elev$e2 <- apply(ds.elev[, c("B05", "W05")], 1, max)
    ds.elev$e3 <- apply(ds.elev[, c("B95", "W95")], 1, min)
    ds.elev$e4 <- apply(ds.elev[, c("B95", "W95")], 1, max)
    
    # color and alpha
    ds.elev$col12 <- ifelse(ds.elev$B05 < ds.elev$W05, "3", "4")
    ds.elev$col34 <- ifelse(ds.elev$B95 > ds.elev$W95, "3", "4")
    ds.elev$colmed <- ifelse(ds.elev$B50 > ds.elev$W50, "1", "2")
    ds.elev$alphamed <- ifelse(ds.elev$Group50 == "0", "2", "1")
    
    # make species as factor (for the ordering in the plot)
    ds.elev$Species <- factor(ds.elev$Species, levels = rev(ds.elev$Species[order(ds.elev$B50)]))
    
    # max and min y values of the plot
    ds.elev$max <- apply(ds.elev[, 2:7], 1, max)
    ds.elev$min <- apply(ds.elev[, 2:7], 1, min)
    
    # bar width
    if (minSample == 30) {
        if (substr(ds[[1]], 4, 5) == "q1") barwidth <- 4.4
        if (substr(ds[[1]], 4, 5) == "q2") barwidth <- 3.6
        if (substr(ds[[1]], 4, 5) == "q3") barwidth <- 3.3
    } else barwidth <- 15.0
    
    
    # plot and save distribution range
    p <- ggplot(data = ds.elev, aes(x = Species, ymin = min, ymax = max, family = "Calibri")) +
        geom_linerange(aes(x = Species, ymin = e3, ymax = e4, color = col34), linewidth = barwidth) +
        geom_linerange(aes(x = Species, ymin = e1, ymax = e2, color = col12), linewidth = barwidth) +
        geom_linerange(aes(x = Species, ymin = e2, ymax = e3), color = "#aa60cc", linewidth = barwidth) +
        geom_segment(aes(x = Species, xend = Species, y = B50, yend = W50, color = colmed, alpha = alphamed), size = 2,
                     lineend = "round", linejoin = "round", arrow = arrow(length = unit(15, "pt"))) +
        scale_color_manual(values = c("white", "black", "#ff9999", "#9999ff")) +
        scale_alpha_manual(values = c(1, 0)) +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 65, hjust = 1, vjust = 1, face = "italic", size = 22),
              axis.text.y = element_text(angle = 90, hjust = 0.5, size = 18),
              axis.title.y = element_text(size = 26),
              axis.title.x = element_text(size = 20),
              panel.grid.major.x = element_line(color = "gray")) +
        ylab(element_text("Elevation (m)"))+
        xlab(element_text(""))
    
    ggsave(p, filename = paste0("zoomed migration_", substr(ds[1], 4, 5), "n", minSample, "_sp", nrow(ds.elev), " grid.png"),
           width = 17, height = 11, units = "in", dpi = 900)
}

saveMigrationPlot(c("05.q3", "50.q3", "95.q3"), 60) 

# import data
ds <- c("05.q3", "50.q3", "95.q3")
minSample <- 60

group.05 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[1], ".sam", minSample, ".csv"))
group.50 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[2], ".sam", minSample, ".csv"))
group.95 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[3], ".sam", minSample, ".csv"))

# merge all shift measures into one data table
ds.elev <- as.data.frame(cbind(group.05$Species, group.05$Median_S,group.05$Median_W, group.50$Median_S,
                               group.50$Median_W, group.95$Median_S, group.95$Median_W, group.05$Group,
                               group.50$Group, group.95$Group)) %>% na.omit()

colnames(ds.elev) <- c("Species", "B05", "W05", "B50", "W50", "B95", "W95", "Group05", "Group50", "Group95")
for (i in c("B05", "W05", "B50", "W50", "B95", "W95")) {
    ds.elev[[i]] <- as.numeric(ds.elev[[i]])
}

ds.elev <- ds.elev %>%
    filter(Group95 != "0" & Group05 != "0")
## create data for plotting
# grouping (up-migration, down-migration)
ds.elev$Group50 <- as.factor(ds.elev$Group50)
ds.elev$Group05 <- as.factor(ds.elev$Group05)
ds.elev$Group95 <- as.factor(ds.elev$Group95)

# y values of the bars
ds.elev$e1 <- apply(ds.elev[, c("B05", "W05")], 1, min)
ds.elev$e2 <- apply(ds.elev[, c("B05", "W05")], 1, max)
ds.elev$e3 <- apply(ds.elev[, c("B95", "W95")], 1, min)
ds.elev$e4 <- apply(ds.elev[, c("B95", "W95")], 1, max)

# color and alpha
ds.elev$col12 <- ifelse(ds.elev$B05 < ds.elev$W05, "3", "4")
ds.elev$col34 <- ifelse(ds.elev$B95 > ds.elev$W95, "3", "4")
ds.elev$colmed <- ifelse(ds.elev$B50 > ds.elev$W50, "1", "2")
ds.elev$alphamed <- ifelse(ds.elev$Group50 == "0", "2", "1")

# make species as factor (for the ordering in the plot)
ds.elev$Species <- factor(ds.elev$Species, levels = rev(ds.elev$Species[order(ds.elev$B50)]))

# max and min y values of the plot
ds.elev$max <- apply(ds.elev[, 2:7], 1, max)
ds.elev$min <- apply(ds.elev[, 2:7], 1, min)

# bar width
if (minSample == 30) {
    if (substr(ds[[1]], 4, 5) == "q1") barwidth <- 4.4
    if (substr(ds[[1]], 4, 5) == "q2") barwidth <- 3.6
    if (substr(ds[[1]], 4, 5) == "q3") barwidth <- 3.3
} else barwidth <- 3.7


# plot and save distribution range
p <- ggplot(data = ds.elev, aes(x = Species, ymin = min, ymax = max, family = "Calibri")) +
    geom_linerange(aes(x = Species, ymin = e3, ymax = e4, color = col34), linewidth = barwidth) +
    geom_linerange(aes(x = Species, ymin = e1, ymax = e2, color = col12), linewidth = barwidth) +
    geom_linerange(aes(x = Species, ymin = e2, ymax = e3), color = "#aa60cc", linewidth = barwidth) +
    geom_segment(aes(x = Species, xend = Species, y = B50, yend = W50, color = colmed, alpha = alphamed), size = 0.8,
                 lineend = "round", linejoin = "round", arrow = arrow(length = unit(25, "pt"))) +
    scale_color_manual(values = c("white", "black", "#ff9999", "#9999ff")) +
    scale_alpha_manual(values = c(1, 0)) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "italic", size = 11),
          axis.text.y = element_text(angle = 90, hjust = 0.5, size = 16),
          axis.title.y = element_text(size = 18),
          axis.title.x = element_text(size = 18),
          panel.grid.major.x = element_line(color = "gray")) +
    xlab(element_text("Species")) +
    ylab(element_text("Elevation (m)"))

ggsave(p, filename = paste0("zoomed migration_", substr(ds[1], 4, 5), "n", minSample, "_sp", nrow(ds.elev), " grid.png"),
       width = 16, height = 9, units = "in", dpi = 900)



saveMigrationPlot <- function(ds, minSample) {
    
    # import data
    
    group.05 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[1], ".sam", minSample, ".csv"))
    group.50 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[2], ".sam", minSample, ".csv"))
    group.95 <- read.csv(paste0("results/extent/final_birdlist_filtered", ds[3], ".sam", minSample, ".csv"))
    
    # merge all shift measures into one data table
    ds.elev <- as.data.frame(cbind(group.05$Species, group.05$Median_S,group.05$Median_W, group.50$Median_S,
                                   group.50$Median_W, group.95$Median_S, group.95$Median_W, group.05$Group,
                                   group.50$Group, group.95$Group)) %>% na.omit()
    
    colnames(ds.elev) <- c("Species", "B05", "W05", "B50", "W50", "B95", "W95", "Group05", "Group50", "Group95")
    for (i in c("B05", "W05", "B50", "W50", "B95", "W95")) {
        ds.elev[[i]] <- as.numeric(ds.elev[[i]])
    }
    cand <- read.csv("data/elevational shifts in species - Candidate sp..csv")
    ds.elev <- ds.elev %>% filter(Species %in% cand$Species)
    ## create data for plotting
    # grouping (up-migration, down-migration)
    ds.elev$Group50 <- as.factor(ds.elev$Group50)
    ds.elev$Group05 <- as.factor(ds.elev$Group05)
    ds.elev$Group95 <- as.factor(ds.elev$Group95)
    
    # y values of the bars
    ds.elev$e1 <- apply(ds.elev[, c("B05", "W05")], 1, min)
    ds.elev$e2 <- apply(ds.elev[, c("B05", "W05")], 1, max)
    ds.elev$e3 <- apply(ds.elev[, c("B95", "W95")], 1, min)
    ds.elev$e4 <- apply(ds.elev[, c("B95", "W95")], 1, max)
    
    # color and alpha
    ds.elev$col12 <- ifelse(ds.elev$B05 < ds.elev$W05, "3", "4")
    ds.elev$col34 <- ifelse(ds.elev$B95 > ds.elev$W95, "3", "4")
    ds.elev$colmed <- ifelse(ds.elev$B50 > ds.elev$W50, "1", "2")
    ds.elev$alphamed <- ifelse(ds.elev$Group50 == "0", "2", "1")
    
    # make species as factor (for the ordering in the plot)
    ds.elev$Species <- factor(ds.elev$Species, levels = ds.elev$Species[rev(order(ds.elev$B50))])
    
    # max and min y values of the plot
    ds.elev$max <- apply(ds.elev[, 2:7], 1, max)
    ds.elev$min <- apply(ds.elev[, 2:7], 1, min)
    
    # bar width
    barwidth <- 9
    
    
    # plot and save distribution range
    p <- ggplot(data = ds.elev, aes(x = Species, ymin = min, ymax = max, family = "Calibri")) +
        geom_linerange(aes(x = Species, ymin = e3, ymax = e4, color = col34), linewidth = barwidth) +
        geom_linerange(aes(x = Species, ymin = e1, ymax = e2, color = col12), linewidth = barwidth) +
        geom_linerange(aes(x = Species, ymin = e2, ymax = e3), color = "#aa60cc", linewidth = barwidth) +
        geom_segment(aes(x = Species, xend = Species, y = B50, yend = W50, color = colmed, alpha = alphamed), size = 0.8,
                     lineend = "round", linejoin = "round", arrow = arrow(length = unit(9, "pt"))) +
        scale_color_manual(values = c("white", "black", "#ff9999", "#9999ff")) +
        scale_alpha_manual(values = c(1, 0)) +
        theme_bw() +
        theme(legend.position = "none",
              axis.text.x = element_text(angle = 75, hjust = 1, vjust = 1, face = "italic", size = 14),
              axis.text.y = element_text(angle = 90, hjust = 0.5, size = 16),
              axis.title.y = element_text(size = 18),
              axis.title.x = element_text(size = 18),
              panel.grid.major.x = element_line(color = "gray")) +
        xlab(element_text("Species")) +
        ylab(element_text("Elevation (m)"))
    
    ggsave(p, filename = paste0("filtered_dmigration_", substr(ds[1], 4, 5), "n", minSample, "_sp", nrow(ds.elev), " grid.png"),
           width = 16, height = 9, units = "in", dpi = 900)
}
#### filtering out species ####
saveMigrationPlot(c("05.q3", "50.q3", "95.q3"), 60) 

