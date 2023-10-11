library(dplyr)
library(phytools)
library(phangorn)
library(tidyverse)
library(ape)
library(geiger)
library(nlme)
library(phytools)
# load necessary data for running statistical models 
east_100 <-read.csv("data/env_mean_vars_100.csv") #temperature data
east_100_W <- east_100 %>% select(-contains("Dec")) %>% rename(elev_roundW = elev_round) #winter
east_100_S <- east_100 %>% select(-contains("May"))%>% rename(elev_roundS = elev_round) #summer
jetzname <-read.csv("data/matched ebird names - jets-ebird tax.csv") #taxonomy used by birdtree.org
sheard_trait <- read.csv("data/HWI DATA - speciesdata.csv") %>% #trait data set
    select(HWI,Diet,Tree.name,Body.mass..log.)
sheard_trait$bodymass <- 10^(sheard_trait$Body.mass..log.)
sheard_trait$Species.y <- sub("_", " ", sheard_trait$Tree.name)
sheard_trait1 <- sheard_trait %>% drop_na(Species.y)
tree <-read.nexus("data/birdtree.nex")
tree <- mcc(tree) ## maximum clade credibility tree


compile_eastmodels<-setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("Variable", "Value", "Std.Error", "t-value", "p-value", "ds"))


for (ds in c("05.q1.sam30", "05.q1.sam60", "05.q2.sam30","05.q2.sam60", "05.q3.sam30","05.q3.sam60", "95.q1.sam30","95.q1.sam60", "95.q2.sam30","95.q2.sam60" ,"95.q3.sam30","95.q3.sam60" ,"50.q1.sam30","50.q1.sam60" ,"50.q2.sam30","50.q2.sam60" ,"50.q3.sam30", "50.q3.sam60")) {
    
    a<-read.csv(paste("results/extent/final_birdlist_filtered", ds, ".csv", sep = ""))
    a$eBird.English.Name.2021 <- a$Species
    #use a by function to left join!!!!
    x<-left_join(a,jetzname, by = "eBird.English.Name.2021")
    x<-left_join(x, sheard_trait1, by = "Species.y")
    x <- mutate(x, Diet = fct_recode(Diet, "omnivore" = "nectar", "omnivore" = "scav", "seeds"="plants"))
    x$elev_roundS<-round(x$Median_S,-2)
    x$elev_roundW<-round(x$Median_W,-2)
    x<-left_join(x,east_100_S, by = "elev_roundS")
    x<-left_join(x,east_100_W, by = "elev_roundW")
    x$ThermalRegime<-x$maxTemp_May_mean-x$minTemp_Dec_mean
    x <- x[complete.cases(x),]
    glsdata<-x[match(tree$tip.label, x$Tree.name),]
    glsdata <- glsdata[complete.cases(glsdata), ]
    pgls<-gls(Median_diff~ThermalRegime+HWI+Diet+bodymass,correlation=corBrownian(1,phy = tree),data=glsdata)
    k<-summary(pgls)
    outputk<-as.data.frame(k$tTable)
    outputk$dataset<-ds[1]
    outputk$Variable<-rownames(outputk)
    compile_eastmodels<-rbind(compile_eastmodels,outputk)
    
    write.csv(glsdata, file = paste("SWG_ordered",ds,".csv", sep = ""), row.names = F)
    write.csv(compile_eastmodels, "compile_SWGmodels.csv", row.names = F)
}



