

# library(dplyr) 
# library(multcomp)
# library(scales)
# library(tidyr)
# library(ggplot2)
# 
# library(lme4)
# library(emmeans)  
# library(lmerTest)

library(pollinatorAttractiveness)
library(tidyverse)

# emmeans emmeans
# lme4 glmer
# stats anova confint





# scales::hue_pal()(3)[2]
# show_col(hue_pal()(3))

# path <- "//lar-file-srv/Data/Jason/pollinatorAttractiveness/datasets/csvsUsedInAnalysis/"

# # Analysis parts I & II.  
# source("//lar-file-srv/Data/Jason/pollinatorAttractiveness/R/anoDev.R")
# source("//lar-file-srv/Data/Jason/pollinatorAttractiveness/R/getPoissonMeans.R")
# source("//lar-file-srv/Data/Jason/pollinatorAttractiveness/R/makeTallBars.R")
# source("//lar-file-srv/Data/Jason/pollinatorAttractiveness/R/stackedBars.R")
# 
# # Analysis part III.  
# source("//lar-file-srv/Data/Jason/pollinatorAttractiveness/R/fitFT.R")
# source("//lar-file-srv/Data/Jason/pollinatorAttractiveness/R/makeModelStrings.R")
# source("//lar-file-srv/Data/Jason/pollinatorAttractiveness/R/cleanUpFormula.R")
# source("//lar-file-srv/Data/Jason/pollinatorAttractiveness/R/getKappa.R")
# source("//lar-file-srv/Data/Jason/pollinatorAttractiveness/R/fits.R")
# source("//lar-file-srv/Data/Jason/pollinatorAttractiveness/R/lettered.R")
# source("//lar-file-srv/Data/Jason/pollinatorAttractiveness/R/table1plot.R")

textsize <- 16    # Text size of all output, except...
textsize2 <- 11   # ... plots entitled Visit per Sampling Period, Visit per Sampling Period by Insect, Visits per Sampling Period within Flower Type

# Make sure this folder already exists.  
out <- "L:/Jason/pollinatorAttractiveness/Output/September2019/"

dir.create(file.path(out,"2017 Perennials"),showWarnings=FALSE)
dir.create(file.path(out,"2017 Annuals"),showWarnings=FALSE)
dir.create(file.path(out,"2017 Standards"),showWarnings=FALSE)
dir.create(file.path(out,"2017 StandardsMums"),showWarnings=FALSE)
dir.create(file.path(out,"2018 Perennials"),showWarnings=FALSE)
dir.create(file.path(out,"2018 Annuals"),showWarnings=FALSE)
dir.create(file.path(out,"2018 Standards"),showWarnings=FALSE)
dir.create(file.path(out,"2018 StandardsMums"),showWarnings=FALSE)

path2017 <- "L:/Jason/pollinatorAttractiveness/Project/Reformatted Data/2017/To Be Analyzed - 2017"
X2017 <- lapply(paste0(path2017,"/",dir(path2017,pattern=".csv")),function(x) utils::read.csv(x,stringsAsFactors=FALSE))
names(X2017) <- paste0("y",substr(dir(path2017,pattern=".csv"),1,nchar(dir(path2017,pattern=".csv")) - 4))

path2018 <- "L:/Jason/pollinatorAttractiveness/Project/Reformatted Data/2018/To Be Analyzed - 2018"
X2018 <- lapply(paste0(path2018,"/",dir(path2018,pattern=".csv")),function(x) utils::read.csv(x,stringsAsFactors=FALSE))
names(X2018) <- paste0("y",substr(dir(path2018,pattern=".csv"),1,nchar(dir(path2018,pattern=".csv")) - 4))







#' For the perennials, we would like a graph of the means with error bars, but
#' we also like the analysis of deviance (that's the one with the pairwise
#' ratios, right). All of these figures should be separated by year. Lastly,
#' there are 15 chrysanthemum cultivars in 2018, is that too many for a pairwise
#' analysis?

# Daylily       2018
# Dianthus 2017 2018
# Heuchera 2017 2018
# Hosta         2018
# Mum      2017

#   ---- Combine all data into one spreadsheet.  Block variable not coded.  
totalperennial <- dplyr::bind_rows(readr::read_csv(paste0(path2017,"/2017DianthusR.csv")),
                                   readr::read_csv(paste0(path2017,"/2017ChrysanthemumR.csv")),
                                   readr::read_csv(paste0(path2017,"/2017HeucheraR.csv")))

#   ---- Analysis of deviance for each FlowerType.  
p1 <- readr::read_csv(paste0(path2017,"/2017DianthusR.csv")) %>% 
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>% 
  anoDev("Dianthus",annuals=FALSE,blocks=FALSE)

p2 <- readr::read_csv(paste0(path2017,"/2017ChrysanthemumR.csv")) %>% 
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>% 
  anoDev("Chrysanthemum",annuals=FALSE,blocks=FALSE)

p3 <- readr::read_csv(paste0(path2017,"/2017HeucheraR.csv")) %>% 
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>% 
  anoDev("Heuchera",annuals=FALSE,blocks=FALSE)

#   ---- Compile Tukey letters.
tukeyLetters <- rbind(p1$c,p2$c,p3$c)
utils::write.csv(tukeyLetters,paste0(out,"2017 Perennials/tukeyLettersPerennials2017.csv"),row.names=FALSE)

#   ---- Compile p-values from analyses of deviance.  
allpDeviances <- rbind(p1$a,p2$a,p3$a)
allpDeviances <- allpDeviances[!is.na(allpDeviances$Df),]
rownames(allpDeviances) <- NULL
utils::write.csv(allpDeviances,paste0(out,"2017 Perennials/allpDeviancesPerennials2017.csv"),row.names=FALSE)

#   ---- Compile Tukey-type stats.  Reuse names expected for makeTallBars.  Rename in the function. 
allptDiffs <- as.data.frame(rbind(p1$i,p2$i,p3$i))
colnames(allptDiffs) <- c("Cultivar","FlowerType","b0","lo2.5","hi97.5","calpha","pvalue")
allptDiffs$b0 <- ifelse(is.infinite(allptDiffs$hi97.5),0,allptDiffs$b0)
allptDiffs$hi97.5 <- ifelse(is.infinite(allptDiffs$hi97.5),0,allptDiffs$hi97.5)
allptDiffs <- allptDiffs[!(allptDiffs$lo2.5 == allptDiffs$hi97.5),]
allptDiffs$Cultivar <- gsub("-","/",allptDiffs$Cultivar)
rownames(allptDiffs) <- NULL
allptDiffs <- allptDiffs %>% dplyr::arrange(FlowerType,desc(b0))
utils::write.csv(allptDiffs,paste0(out,"/2017 Perennials/allptDiffsPerennials2017.csv"),row.names=FALSE)

#   ---- Individual Cultivar within FlowerType graphical comparisons.  
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Dianthus",],"2017 Dianthus -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Perennials/"),12,6,TRUE,scales::hue_pal()(3)[1],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Chrysanthemum",],"2017 Chrysanthemum -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Perennials/"),12,10,TRUE,scales::hue_pal()(3)[2],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Heuchera",],"2017 Heuchera -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Perennials/"),12,10,TRUE,scales::hue_pal()(3)[3],text=textsize)

#   ---- Get Cultivar within Flower Type means.  
perennialMeans <- getPoissonMeans(totalperennial)
utils::write.csv(perennialMeans,paste0(out,"2017 Perennials/perennialsMeans2017.csv"),row.names=FALSE)

#   ---- Make graphs with all output together.  Group by FlowerType, then sort by mean estimate.
perennialMeans2 <- perennialMeans %>% 
  dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
  dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) 
makeTallBars(perennialMeans2,"2017 Perennials -- Visits per Sampling Period within Flower Type","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2017 Perennials/"),6,8,text=textsize2)
stackedBars(perennialMeans2,"2017 Perennials -- Visits per Sampling Period by Insect Status","","Visits / Sampling Period","Flower Type - Cultivar",paste0(out,"2017 Perennials/"),6,8,text=textsize2)

#   ---- Same as above, but Chrysanthemum alone and then all others alone.  
makeTallBars(perennialMeans2[perennialMeans2$FlowerType == "Chrysanthemum",],"2017 Perennials -- Chrysanthemum Only -- Visits per Sampling Period within Flower Type","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2017 Perennials/"),6,8,text=9)
stackedBars(perennialMeans2[perennialMeans2$FlowerType == "Chrysanthemum",],"2017 Perennials -- Chrysanthemum Only -- Visits per Sampling Period by Insect Status","","Visits / Sampling Period","Flower Type - Cultivar",paste0(out,"2017 Perennials/"),6,8,text=9)
makeTallBars(perennialMeans2[perennialMeans2$FlowerType != "Chrysanthemum",],"2017 Perennials -- All But Chrysanthemum -- Visits per Sampling Period within Flower Type","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2017 Perennials/"),6,8,text=textsize2)
stackedBars(perennialMeans2[perennialMeans2$FlowerType != "Chrysanthemum",],"2017 Perennials -- All But Chrysanthemum -- Visits per Sampling Period by Insect Status","","Visits / Sampling Period","Flower Type - Cultivar",paste0(out,"2017 Perennials/"),6,8,text=textsize2)


#   ---- Make graphs with all output together.  Order over all mean estimates.  
perennialMeans3 <- perennialMeans %>% 
  dplyr::arrange(dplyr::desc(b0))
makeTallBars(perennialMeans3,"2017 Perennials -- Visits per Sampling Period","Visits / Sampling Period with 95% Confidence Interval","","Cultivar",paste0(out,"2017 Perennials/"),6,8,text=textsize)

#   ---- Same as above, but Chrysanthemum alone and then all others alone.  
makeTallBars(perennialMeans3[perennialMeans3$FlowerType == "Chrysanthemum",],"2017 Perennials -- Chrysanthemum Only -- Visits per Sampling Period","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2017 Perennials/"),6,8,text=textsize)
makeTallBars(perennialMeans3[perennialMeans3$FlowerType != "Chrysanthemum",],"2017 Perennials -- All But Chrysanthemum -- Visits per Sampling Period","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2017 Perennials/"),6,8,text=textsize)

#   ---- Prepare dataframe, group by Flower Type, then largest to smallest.  
# ggdf <- allptDiffs %>% 
#   dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
#   dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) 
# makeTallBars(ggdf,"2017 Perennials -- Tukey Ratios -- Ordered within Flower Type","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Perennials/"),12,12,TRUE)

#   ---- Prepare dataframe, order over all mean ratios.  
ggdf2 <- allptDiffs %>% 
  dplyr::arrange(dplyr::desc(b0)) %>%
  dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) 
makeTallBars(ggdf2,"2017 Perennials -- Tukey Ratios -- Ordered Over All","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Perennials/"),12,12,TRUE,text=textsize)

#   ---- Same as above, but Chrysanthemum alone and then all others alone.  
makeTallBars(ggdf2[ggdf2$FlowerType == "Chrysanthemum",],"2017 Perennials -- Chrysanthemum Only -- Tukey Ratios -- Ordered Over All","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Perennials/"),12,12,TRUE,text=textsize)
makeTallBars(ggdf2[ggdf2$FlowerType != "Chrysanthemum",],"2017 Perennials -- All But Chrysanthemum -- Tukey Ratios -- Ordered Over All","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Perennials/"),12,12,TRUE,text=textsize)

#   ---- Insects.
insects <- c("Syrphidae","ApisMellifera","BombusImpatiens","OtherDiptera","OtherBees","Wasps")
insectsClean <- c("Syrphidae","Apis Mellifera","Bombus Impatiens","Other Diptera","Other Bees","Wasps") 
for(insect in insects){
  
  insectClean <- insectsClean[match(insect,insects)]
  
  #   ---- Ordered Over All 
  totalperennial %>% 
    dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>% 
    dplyr::rename(TotalPollinatorVisits = !!sym(paste0("Total",insect))) %>% 
    dplyr::mutate(VisitsPerMin = TotalPollinatorVisits / SamplingPeriods) %>% 
    getPoissonMeans() %>%
    dplyr::arrange(dplyr::desc(b0)) %>%
    dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
    makeTallBars(paste0("2017 Perennials -- ",insectClean," -- Ordered Over All"),"","Visits / Sampling Period with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Perennials/"),12,12,text=textsize)
  
  # #   ---- Ordered Within Flower Type 
  # totalperennial %>% 
  #   dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>% 
  #   dplyr::rename(TotalPollinatorVisits = !!sym(paste0("Total",insect))) %>% 
  #   dplyr::mutate(VisitsPerMin = TotalPollinatorVisits / SamplingPeriods) %>% 
  #   getPoissonMeans() %>%
  #   dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
  #   dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
  #   makeTallBars(paste0("2017 Perennials -- ",insectClean," -- Ordered within Flower Type"),"","Visits / Sampling Period with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Perennials/"),12,12,text=textsize)
  
  if(insect %in% c("Syrphidae","ApisMellifera","BombusImpatiens","OtherDiptera","OtherBees","Wasps")){
    
    for(ft in unique(totalperennial$FlowerType)){
      
      #   ---- Restricted to both insect and flower type.  
      totalperennial %>% 
        dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>% 
        dplyr::rename(TotalPollinatorVisits = !!sym(paste0("Total",insect))) %>% 
        dplyr::mutate(VisitsPerMin = TotalPollinatorVisits / SamplingPeriods) %>% 
        getPoissonMeans() %>%
        dplyr::arrange(dplyr::desc(b0)) %>%
        dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
        dplyr::filter(FlowerType == ft) %>%
        makeTallBars(paste0("2017 Perennials -- ",insectClean," -- ",ft),"","Visits / Sampling Period with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Perennials/"),12,12,FALSE,text=textsize)
    }
  }
}


# Investigate relationships with cutpoints.  
dianthus2017 <- readr::read_csv(paste0(path2017,"/2017DianthusR.csv")) %>% dplyr::filter(Cultivar != "Firestar") %>% fitFT(2017,"SamplingPeriods")
chrysanthemum2017 <- readr::read_csv(paste0(path2017,"/2017ChrysanthemumR.csv")) %>% fitFT(2017,"SamplingPeriods")
heuchera2017 <- readr::read_csv(paste0(path2017,"/2017HeucheraR.csv")) %>% fitFT(2017,"SamplingPeriods")

summary(dianthus2017$poisson$goodL[[dianthus2017$poisson$top[dianthus2017$poisson$top$formula == "z0_1z",]$goodN]])
summary(chrysanthemum2017$poisson$goodL[[chrysanthemum2017$poisson$top[chrysanthemum2017$poisson$top$formula == "z0_1z",]$goodN]])
summary(heuchera2017$poisson$goodL[[heuchera2017$poisson$top[heuchera2017$poisson$top$formula == "z0_1z",]$goodN]])

perennialModels2017 <- rbind(dianthus2017$poisson$top,
                             chrysanthemum2017$poisson$top,
                             heuchera2017$poisson$top)

utils::write.csv(perennialModels2017,paste0(out,"2017 Perennials/perennialModels2017.csv"))


# Tukey letter plots.  
lettered(totalperennial,"Perennials","2017 Dianthus -- Pollinator Means & Tukey Lettering","",
         "Cultivar Comparisons","Visits / Sampling Period","Dianthus",out,2017,12,12,TRUE,FALSE,text=textsize)

lettered(totalperennial,"Perennials","2017 Chrysanthemum -- Pollinator Means & Confidence Bounds","",
         "Cultivar Comparisons","Visits / Sampling Period","Chrysanthemum",out,2017,12,12,FALSE,TRUE,text=textsize)

lettered(totalperennial,"Perennials","2017 Chrysanthemum -- Pollinator Means & Tukey Lettering","",
         "Cultivar Comparisons","Visits / Sampling Period","Chrysanthemum",out,2017,12,12,TRUE,FALSE,text=textsize)

lettered(totalperennial,"Perennials","2017 Heuchera -- Pollinator Means & Tukey Lettering","",
         "Cultivar Comparisons","Visits / Sampling Period","Heuchera",out,2017,12,12,TRUE,FALSE,text=textsize)

rm(totalperennial,p1,p2,p3,tukeyLetters,allpDeviances,allptDiffs,perennialMeans,perennialMeans2,perennialMeans3,ggdf2,insectClean,insects,insectsClean,insect,
   dianthus2017,chrysanthemum2017,heuchera2017,perennialModels2017)










#   ---- Combine all data into one spreadsheet.  Block variable not coded.  
totalperennial <- dplyr::bind_rows(readr::read_csv(paste0(path2018,"/2018DianthusR.csv")),
                                   readr::read_csv(paste0(path2018,"/2018ChrysanthemumR.csv")),
                                   readr::read_csv(paste0(path2018,"/2018HeucheraR.csv")),
                                   readr::read_csv(paste0(path2018,"/2018HostaR.csv")),
                                   readr::read_csv(paste0(path2018,"/2018DaylilyR.csv")))

#   ---- Analysis of deviance for each FlowerType.  
p1 <- readr::read_csv(paste0(path2018,"/2018DianthusR.csv")) %>% 
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>% 
  anoDev("Dianthus",annuals=FALSE,blocks=FALSE)

p2 <- readr::read_csv(paste0(path2018,"/2018ChrysanthemumR.csv")) %>% 
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>% 
  anoDev("Chrysanthemum",annuals=FALSE,blocks=FALSE)

p3 <- readr::read_csv(paste0(path2018,"/2018HeucheraR.csv")) %>% 
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>% 
  anoDev("Heuchera",annuals=FALSE,blocks=FALSE)

p4 <- readr::read_csv(paste0(path2018,"/2018HostaR.csv")) %>% 
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>% 
  anoDev("Hosta",annuals=FALSE,blocks=FALSE)

p5 <- readr::read_csv(paste0(path2018,"/2018DaylilyR.csv")) %>% 
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>% 
  anoDev("Daylily",annuals=FALSE,blocks=FALSE)

#   ---- Compile Tukey letters.
tukeyLetters <- rbind(p1$c,p2$c,p3$c,p4$c,p5$c)
utils::write.csv(tukeyLetters,paste0(out,"2018 Perennials/tukeyLettersPerennials2018.csv"),row.names=FALSE)

#   ---- Compile p-values from analyses of deviance.  
allpDeviances <- rbind(p1$a,p2$a,p3$a)
allpDeviances <- allpDeviances[!is.na(allpDeviances$Df),]
rownames(allpDeviances) <- NULL
utils::write.csv(allpDeviances,paste0(out,"2018 Perennials/allpDeviancesPerennials2018.csv"),row.names=FALSE)

#   ---- Compile Tukey-type stats.  Reuse names expected for makeTallBars.  Rename in the function. 
allptDiffs <- as.data.frame(rbind(p1$i,p2$i,p3$i,p4$i,p5$i))
colnames(allptDiffs) <- c("Cultivar","FlowerType","b0","lo2.5","hi97.5","calpha","pvalue")
allptDiffs$b0 <- ifelse(is.infinite(allptDiffs$hi97.5),0,allptDiffs$b0)
allptDiffs$hi97.5 <- ifelse(is.infinite(allptDiffs$hi97.5),0,allptDiffs$hi97.5)
allptDiffs <- allptDiffs[!(allptDiffs$lo2.5 == allptDiffs$hi97.5),]
allptDiffs$Cultivar <- gsub("-","/",allptDiffs$Cultivar)
rownames(allptDiffs) <- NULL
allptDiffs <- allptDiffs %>% dplyr::arrange(FlowerType,desc(b0))
utils::write.csv(allptDiffs,paste0(out,"2018 Perennials/allptDiffsPerennials2018.csv"),row.names=FALSE)

#   ---- Individual Cultivar within FlowerType graphical comparisons.  
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Dianthus",],"2018 Dianthus -- Tukey Ratios","Ratios with 95% Confidence Interval","","Cultivar Comparisons",paste0(out,"2018 Perennials/"),12,6,TRUE,scales::hue_pal()(3)[1],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Chrysanthemum",],"2018 Chrysanthemum -- Tukey Ratios","Ratios with 95% Confidence Interval","","Cultivar Comparisons",paste0(out,"2018 Perennials/"),12,10,TRUE,scales::hue_pal()(3)[2],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Heuchera",],"2018 Heuchera -- Tukey Ratios","Ratios with 95% Confidence Interval","","Cultivar Comparisons",paste0(out,"2018 Perennials/"),12,10,TRUE,scales::hue_pal()(3)[3],text=textsize)

#   ---- Get Cultivar within Flower Type means.  
perennialMeans <- getPoissonMeans(totalperennial)
utils::write.csv(perennialMeans,paste0(out,"2018 Perennials/perennialsMeans2018.csv"),row.names=FALSE)

#   ---- Make graphs with all output together.  Group by FlowerType, then sort by mean estimate.
perennialMeans2 <- perennialMeans %>% 
  dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
  dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) 
makeTallBars(perennialMeans2,"2018 Perennials -- Visits per Sampling Period within Flower Type","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2018 Perennials/"),6,8,text=textsize2)
stackedBars(perennialMeans2,"2018 Perennials -- Visits per Sampling Period by Insect Status","","Visits / Sampling Period","Flower Type - Cultivar",paste0(out,"2018 Perennials/"),6,8,text=textsize2)
makeTallBars(perennialMeans2[perennialMeans2$FlowerType == "Chrysanthemum",],"2018 Chrysanthemums -- Visits per Sampling Period","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2018 Perennials/"),6,8,text=textsize2)

#   ---- Same as above, but Chrysanthemum alone and then all others alone.  
makeTallBars(perennialMeans2[perennialMeans2$FlowerType == "Chrysanthemum",],"2018 Perennials -- Chrysanthemum Only -- Visits per Sampling Period within Flower Type","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2018 Perennials/"),6,8,text=9)
stackedBars(perennialMeans2[perennialMeans2$FlowerType == "Chrysanthemum",],"2018 Perennials -- Chrysanthemum Only -- Visits per Sampling Period by Insect Status","","Visits / Sampling Period","Flower Type - Cultivar",paste0(out,"2018 Perennials/"),6,8,text=9)
makeTallBars(perennialMeans2[perennialMeans2$FlowerType != "Chrysanthemum",],"2018 Perennials -- All But Chrysanthemum -- Visits per Sampling Period within Flower Type","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2018 Perennials/"),6,8,text=textsize2)
stackedBars(perennialMeans2[perennialMeans2$FlowerType != "Chrysanthemum",],"2018 Perennials -- All But Chrysanthemum -- Visits per Sampling Period by Insect Status","","Visits / Sampling Period","Flower Type - Cultivar",paste0(out,"2018 Perennials/"),6,8,text=textsize2)


#   ---- Make graphs with all output together.  Order over all mean estimates.  
perennialMeans3 <- perennialMeans %>% 
  dplyr::arrange(dplyr::desc(b0))
makeTallBars(perennialMeans3,"2018 Perennials -- Visits per Sampling Period","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2018 Perennials/"),6,8,text=textsize)

#   ---- Same as above, but Chrysanthemum alone and then all others alone.  
makeTallBars(perennialMeans3[perennialMeans3$FlowerType == "Chrysanthemum",],"2018 Perennials -- Chrysanthemum Only -- Visits per Sampling Period","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2018 Perennials/"),6,8,text=textsize)
makeTallBars(perennialMeans3[perennialMeans3$FlowerType != "Chrysanthemum",],"2018 Perennials -- All But Chrysanthemum -- Visits per Sampling Period","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2018 Perennials/"),6,8,text=textsize)

#   ---- Prepare dataframe, group by Flower Type, then largest to smallest.  
# ggdf <- allptDiffs %>% 
#   dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
#   dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) 
# makeTallBars(ggdf,"2018 Perennials -- Tukey Ratios -- Ordered within Flower Type","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Perennials/"),12,12,TRUE)

#   ---- Prepare dataframe, order over all mean ratios.  
ggdf2 <- allptDiffs %>% 
  dplyr::arrange(dplyr::desc(b0)) %>%
  dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) 
makeTallBars(ggdf2,"2018 Perennials -- Tukey Ratios -- Ordered Over All","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Perennials/"),12,12,TRUE,text=textsize)

#   ---- Same as above, but Chrysanthemum alone and then all others alone.  
makeTallBars(ggdf2[ggdf2$FlowerType == "Chrysanthemum",],"2018 Perennials -- Chrysanthemum Only -- Tukey Ratios -- Ordered Over All","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Perennials/"),12,12,TRUE,text=textsize)
makeTallBars(ggdf2[ggdf2$FlowerType != "Chrysanthemum",],"2018 Perennials -- All But Chrysanthemum -- Tukey Ratios -- Ordered Over All","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Perennials/"),12,12,TRUE,text=textsize)


#   ---- Insects.
insects <- c("Syrphidae","ApisMellifera","BombusImpatiens","OtherDiptera","OtherBees","Wasps")
insectsClean <- c("Syrphidae","Apis Mellifera","Bombus Impatiens","Other Diptera","Other Bees","Wasps") 
for(insect in insects){
  
  insectClean <- insectsClean[match(insect,insects)]
  
  #   ---- Ordered Over All 
  totalperennial %>% 
    dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>% 
    dplyr::rename(TotalPollinatorVisits = !!sym(paste0("Total",insect))) %>% 
    dplyr::mutate(VisitsPerMin = TotalPollinatorVisits / SamplingPeriods) %>% 
    getPoissonMeans() %>%
    dplyr::arrange(dplyr::desc(b0)) %>%
    dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
    makeTallBars(paste0("2018 Perennials -- ",insectClean," -- Ordered Over All"),"","Visits / Sampling Period with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Perennials/"),12,12,text=textsize)
  
  # #   ---- Ordered Within Flower Type 
  # totalperennial %>% 
  #   dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>% 
  #   dplyr::rename(TotalPollinatorVisits = !!sym(paste0("Total",insect))) %>% 
  #   dplyr::mutate(VisitsPerMin = TotalPollinatorVisits / SamplingPeriods) %>% 
  #   getPoissonMeans() %>%
  #   dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
  #   dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
  #   makeTallBars(paste0("2018 Perennials -- ",insectClean," -- Ordered within Flower Type"),"","Visits / Sampling Period with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Perennials/"),12,12,text=textsize)
  
  if(insect %in% c("Syrphidae","ApisMellifera","BombusImpatiens","OtherDiptera","OtherBees","Wasps")){
    
    for(ft in unique(totalperennial$FlowerType)){
      
      #   ---- Restricted to both insect and flower type.  
      totalperennial %>% 
        dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>% 
        dplyr::rename(TotalPollinatorVisits = !!sym(paste0("Total",insect))) %>% 
        dplyr::mutate(VisitsPerMin = TotalPollinatorVisits / SamplingPeriods) %>% 
        getPoissonMeans() %>%
        dplyr::arrange(dplyr::desc(b0)) %>%
        dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
        dplyr::filter(FlowerType == ft) %>%
        makeTallBars(paste0("2018 Perennials -- ",insectClean," -- ",ft),"","Visits / Sampling Period with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Perennials/"),12,12,FALSE,text=textsize)
    }
  }
}


# Investigate relationships with color, flower area, and spiders.  
dianthus2018 <- readr::read_csv(paste0(path2018,"/2018DianthusR.csv")) %>% dplyr::filter(Cultivar != "Firestar") %>% fitFT(2018,"SamplingPeriods")
chrysanthemum2018 <- readr::read_csv(paste0(path2018,"/2018ChrysanthemumR.csv")) %>% fitFT(2018,"SamplingPeriods")
daylily2018 <- readr::read_csv(paste0(path2018,"/2018DaylilyR.csv")) %>% fitFT(2018,"SamplingPeriods")
heuchera2018 <- readr::read_csv(paste0(path2018,"/2018HeucheraR.csv")) %>% fitFT(2018,"SamplingPeriods")
hosta2018 <- readr::read_csv(paste0(path2018,"/2018HostaR.csv")) %>% fitFT(2018,"SamplingPeriods")

summary(dianthus2018$poisson$goodL[[dianthus2018$poisson$top[dianthus2018$poisson$top$formula == "z0_1z",]$goodN]])
summary(chrysanthemum2018$poisson$goodL[[chrysanthemum2018$poisson$top[chrysanthemum2018$poisson$top$formula == "z0_1z",]$goodN]])
summary(daylily2018$poisson$goodL[[daylily2018$poisson$top[daylily2018$poisson$top$formula == "z0_1z",]$goodN]])
summary(heuchera2018$poisson$goodL[[heuchera2018$poisson$top[heuchera2018$poisson$top$formula == "z0_1z",]$goodN]])
summary(hosta2018$poisson$goodL[[hosta2018$poisson$top[hosta2018$poisson$top$formula == "z0_1z",]$goodN]])

perennialModels2018 <- rbind(dianthus2018$poisson$top,
                             chrysanthemum2018$poisson$top,
                             daylily2018$poisson$top,
                             heuchera2018$poisson$top,
                             hosta2018$poisson$top)

utils::write.csv(perennialModels2018,paste0(out,"2018 Perennials/perennialModels2018.csv"))

# Tukey letter plots.  
lettered(totalperennial,"Perennials","2018 Dianthus -- Pollinator Means & Tukey Lettering","",
         "Cultivar Comparisons","Visits / Sampling Period","Dianthus",out,2018,12,12,TRUE,FALSE,text=textsize)

lettered(totalperennial,"Perennials","2018 Chrysanthemum -- Pollinator Means & Confidence Bounds","",
         "Cultivar Comparisons","Visits / Sampling Period","Chrysanthemum",out,2018,12,12,FALSE,TRUE,text=textsize)

lettered(totalperennial,"Perennials","2018 Heuchera -- Pollinator Means & Tukey Lettering","",
         "Cultivar Comparisons","Visits / Sampling Period","Heuchera",out,2018,12,12,TRUE,FALSE,text=textsize)

lettered(totalperennial,"Perennials","2018 Hosta -- Pollinator Means & Tukey Lettering","",
         "Cultivar Comparisons","Visits / Sampling Period","Hosta",out,2018,12,12,TRUE,FALSE,text=textsize)

lettered(totalperennial,"Perennials","2018 Daylily -- Pollinator Means & Tukey Lettering","",
         "Cultivar Comparisons","Visits / Sampling Period","Daylily",out,2018,12,12,TRUE,FALSE,text=textsize)

rm(totalperennial,p1,p2,p3,p4,p5,tukeyLetters,allpDeviances,allptDiffs,perennialMeans,perennialMeans2,perennialMeans3,ggdf2,insectClean,insects,insectsClean,insect,
   dianthus2018,chrysanthemum2018,daylily2018,heuchera2018,hosta2018,perennialModels2018)
















# Standards. 


totalstandarddates <- readr::read_csv(paste0(path2017,"/2017StandardAnnualDatesR.csv"))

#   ---- Get Cultivar within Flower Type means.  
totalStandardMeans <- getPoissonMeans(totalstandarddates)
utils::write.csv(totalStandardMeans,paste0(out,"2017 Standards/standardsMeans2017.csv"),row.names=FALSE)

lettered(totalstandarddates,"Standards","2017 Standard Annual Dates -- Pollinator Means & Confidence Bounds","",
         "Cultivar Comparisons","Visits / Sampling Period",c("Marigolds","Zinnia","Echinacea","Nepeta"),out,2017,12,12,FALSE,TRUE,text=textsize)



totalstandardmumdates <- readr::read_csv(paste0(path2017,"/2017StandardMumDatesR.csv"))

#   ---- Get Cultivar within Flower Type means.  
totalStandardMumMeans <- getPoissonMeans(totalstandardmumdates)
utils::write.csv(totalStandardMumMeans,paste0(out,"2017 StandardsMums/standardsMumsMeans2017.csv"),row.names=FALSE)

lettered(totalstandardmumdates,"StandardsMums","2017 Standard Mum Dates -- Pollinator Means & Confidence Bounds","",
         "Cultivar Comparisons","Visits / Sampling Period",c("Marigolds","Zinnia","Echinacea","Nepeta"),out,2017,12,12,FALSE,TRUE,text=textsize)

rm(totalstandarddates,totalStandardMeans,totalstandardmumdates,totalStandardMumMeans)




totalstandarddates <- readr::read_csv(paste0(path2018,"/2018StandardAnnualDatesR.csv"))

#   ---- Get Cultivar within Flower Type means.  
totalStandardMeans <- getPoissonMeans(totalstandarddates)
utils::write.csv(totalStandardMeans,paste0(out,"2018 Standards/standardsMeans2018.csv"),row.names=FALSE)

lettered(totalstandarddates,"Standards","2018 Standard Annual Dates -- Pollinator Means & Confidence Bounds","",
         "Cultivar Comparisons","Visits / Sampling Period",c("Marigolds","Zinnia","Echinacea","Nepeta"),out,2018,12,12,FALSE,TRUE,text=textsize)



totalstandardmumdates <- readr::read_csv(paste0(path2018,"/2018StandardMumDatesR.csv"))

#   ---- Get Cultivar within Flower Type means.  
totalStandardMumMeans <- getPoissonMeans(totalstandardmumdates)
utils::write.csv(totalStandardMumMeans,paste0(out,"2018 StandardsMums/standardsMumsMeans2018.csv"),row.names=FALSE)

lettered(totalstandardmumdates,"StandardsMums","2018 Standard Mum Dates -- Pollinator Means & Confidence Bounds","",
         "Cultivar Comparisons","Visits / Sampling Period",c("Marigolds","Zinnia","Echinacea","Nepeta"),out,2018,12,12,FALSE,TRUE,text=textsize)

rm(totalstandarddates,totalStandardMeans,totalstandardmumdates,totalStandardMumMeans)




















#   ---- Combine all data into one spreadsheet.
totalannual <- dplyr::bind_rows(readr::read_csv(paste0(path2017,"/2017AnnualsR.csv"))) %>% 
  dplyr::mutate(Cultivar=factor(Cultivar)) %>% 
  dplyr::mutate(Block=factor(rep(seq(1,6),25))) %>% 
  dplyr::mutate(FlowerType=factor(FlowerType))

a1 <- totalannual %>%
  anoDev(ft="Begonia",annuals=TRUE,blocks=FALSE)

a2 <- totalannual %>%
  anoDev(ft="Geranium",annuals=TRUE,blocks=FALSE)

a3 <- totalannual %>%
  anoDev(ft="NGImpatiens",annuals=TRUE,blocks=FALSE)

a4 <- totalannual %>%
  anoDev(ft="Impatiens",annuals=TRUE,blocks=FALSE)

a5 <- totalannual %>%
  anoDev(ft="Pansy",annuals=TRUE,blocks=FALSE)

a6 <- totalannual %>%
  anoDev(ft="Petunia",annuals=TRUE,blocks=FALSE)



# To get confidence intervals for each Cultivar, independently of the others. 






#   ---- Call Flower Type the same thing so we can use it in anoDev.  
totalannualAll <- totalannual %>% 
  dplyr::mutate(Cultivar = FlowerType) %>%  
  dplyr::mutate(FlowerType = "All Together")

aa <- totalannualAll %>% 
  anoDev(ft="All Together",annuals=TRUE,blocks=FALSE)

aa2 <- totalannual %>% 
  anoDev(ft=c("Begonia","Geranium","Impatiens","NGImpatiens","Pansy","Petunia"),annuals=TRUE,blocks=FALSE)

#   ---- Compile Tukey letters.
tukeyLetters <- aa2$c
utils::write.csv(tukeyLetters,paste0(out,"2017 Annuals/tukeyLettersAnnuals2017.csv"),row.names=FALSE)



#   ---- Compile p-values from analyses of deviance.  
allaDeviances <- rbind(a1$a,a2$a,a3$a,a4$a,a5$a,a6$a,aa$a)
allaDeviances <- allaDeviances[!is.na(allaDeviances$Df),]
rownames(allaDeviances) <- NULL
write.csv(allaDeviances,paste0(out,"2017 Annuals/allpDeviancesAnnuals2017.csv"),row.names=FALSE)

#   ---- Compile Tukey-type stats.  Reuse names expected for makeTallBars.  Rename in the function. 
allptDiffs <- as.data.frame(rbind(a1$i,a2$i,a3$i,a4$i,a5$i,a6$i,aa$i))
colnames(allptDiffs) <- c("Cultivar","FlowerType","b0","lo2.5","hi97.5","calpha","pvalue")
allptDiffs$b0 <- ifelse(is.infinite(allptDiffs$hi97.5),0,allptDiffs$b0)
allptDiffs$hi97.5 <- ifelse(is.infinite(allptDiffs$hi97.5),0,allptDiffs$hi97.5)
allptDiffs <- allptDiffs[!(allptDiffs$lo2.5 == allptDiffs$hi97.5),]
allptDiffs$Cultivar <- gsub("-","/",allptDiffs$Cultivar)
rownames(allptDiffs) <- NULL
allptDiffs <- allptDiffs %>% dplyr::arrange(FlowerType,desc(b0))
write.csv(allptDiffs,paste0(out,"2017 Annuals/allptDiffsAnnuals2017.csv"),row.names=FALSE)

#   ---- Individual Cultivar within FlowerType graphical comparisons.  
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Begonia",],"2017 Begonia -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Annuals/"),12,6,TRUE,scales::hue_pal()(3)[1],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Geranium",],"2017 Geranium -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Annuals/"),12,10,TRUE,scales::hue_pal()(3)[2],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Impatiens",],"2017 Impatiens -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Annuals/"),12,10,TRUE,scales::hue_pal()(3)[3],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "NGImpatiens",],"2017 NGImpatiens -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Annuals/"),12,6,TRUE,scales::hue_pal()(3)[1],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Pansy",],"2017 Pansy -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Annuals/"),12,10,TRUE,scales::hue_pal()(3)[2],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Petunia",],"2017 Petunia -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Annuals/"),12,10,TRUE,scales::hue_pal()(3)[3],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "All Together",],"2017 All Together -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Annuals/"),12,10,TRUE,scales::hue_pal()(3)[3],text=textsize)

#   ---- Get Cultivar within Flower Type means.  
annualMeans <- getPoissonMeans(totalannual)
utils::write.csv(annualMeans,paste0(out,"2017 Annuals/annualsMeans2017.csv"),row.names=FALSE)

annualMeansAll <- getPoissonMeans(totalannualAll)
utils::write.csv(annualMeansAll,paste0(out,"2017 Annuals/annualsMeansAll2017.csv"),row.names=FALSE)

#   ---- Make graphs with all output together.  Group by FlowerType, then sort by mean estimate.
annualMeans2 <- annualMeans %>% 
  dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
  dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) 
makeTallBars(annualMeans2,"2017 Annuals -- Visits per Sampling Period within Flower Type","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2017 Annuals/"),6,8,text=textsize2)
stackedBars(annualMeans2,"2017 Annuals -- Visits per Sampling Period by Insect Status","","Visits / Sampling Period","Flower Type - Cultivar",paste0(out,"2017 Annuals/"),6,8,text=textsize2)

#   ---- Get means and errors per Cultivar, with each Cultivar alone.  
t1p <- table1plot(totalannual,"Cultivar","FlowerType")
utils::write.csv(t1p,paste0(out,"2017 Annuals/annualsMeans2017EachCultivarAlone.csv"),row.names=FALSE)
makeTallBars(t1p,"2017 Annuals -- All Means and Errors","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2017 Annuals/"),6,8,text=textsize2)

# annualMeans[1,]
# x <- totalannual[totalannual$Cultivar == "Cocktail Brandy",]
# xbar <- mean(x$TotalPollinatorVisits)/23
# xbar - qt(0.975,5)*sqrt(var(x$TotalPollinatorVisits/23)/6)*sqrt(1.189176)
# xbar + qt(0.975,5)*sqrt(var(x$TotalPollinatorVisits/23)/6)*sqrt(1.189176)
# 
# exp(confint(glm(x$TotalPollinatorVisits ~ 1 + offset(log(x$SamplingPeriods)),family=quasipoisson)))


annualMeansAll2 <- annualMeansAll %>% 
  dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
  dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) 
makeTallBars(annualMeansAll2,"2017 Annuals -- All Together -- Visits per Sampling Period within Flower Type","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2017 Annuals/"),6,8,text=textsize2)
stackedBars(annualMeansAll2,"2017 Annuals -- All Together -- Visits per Sampling Period by Insect Status","","Visits / Sampling Period","Flower Type - Cultivar",paste0(out,"2017 Annuals/"),6,8,text=textsize2)


#   ---- Make graphs with all output together.  Order over all mean estimates.  
annualMeans3 <- annualMeans %>% 
  dplyr::arrange(dplyr::desc(b0))
makeTallBars(annualMeans3,"2017 Annuals -- Visits per Sampling Period","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2017 Annuals/"),6,8,text=textsize2)

annualMeansAll3 <- annualMeansAll %>% 
  dplyr::arrange(dplyr::desc(b0))
makeTallBars(annualMeansAll3,"2017 Annuals -- All Together -- Visits per Sampling Period","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2017 Annuals/"),6,8,text=textsize2)

#   ---- Prepare dataframe, group by Flower Type, then largest to smallest.  
# ggdf <- allptDiffs %>% 
#   dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
#   dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) 
# makeTallBars(ggdf,"2017 Annuals -- Tukey Ratios -- Ordered within Flower Type","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Annuals/"),12,12,TRUE)

#   ---- Prepare dataframe, order over all mean ratios.  
ggdf2 <- allptDiffs %>% 
  dplyr::arrange(dplyr::desc(b0)) %>%
  dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
  dplyr::filter(FlowerType != "All Together")
makeTallBars(ggdf2,"2017 Annuals -- Tukey Ratios -- Ordered Over All","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Annuals/"),12,12,TRUE,text=textsize)

ggdfAll2 <- allptDiffs %>% 
  dplyr::arrange(dplyr::desc(b0)) %>%
  dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
  dplyr::filter(FlowerType == "All Together") 
makeTallBars(ggdfAll2,"2017 Annuals -- All Together -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Annuals/"),12,12,TRUE,text=textsize)


#   ---- Insects.
insects <- c("Syrphidae","ApisMellifera","BombusImpatiens","OtherDiptera","OtherBees","Wasps")
insectsClean <- c("Syrphidae","Apis Mellifera","Bombus Impatiens","Other Diptera","Other Bees","Wasps") 
for(insect in insects){
  
  insectClean <- insectsClean[match(insect,insects)]
  
  #   ---- Ordered Over All 
  totalannual %>% 
    dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>%  
    dplyr::rename(TotalPollinatorVisits = !!sym(paste0("Total",insect))) %>% 
    dplyr::mutate(VisitsPerMin = TotalPollinatorVisits / SamplingPeriods) %>% 
    getPoissonMeans() %>%
    dplyr::arrange(dplyr::desc(b0)) %>%
    dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
    makeTallBars(paste0("2017 Annuals -- ",insectClean," -- Ordered Over All"),"Visits / Sampling Period with 95% Confidence Interval","","Cultivar Comparisons",paste0(out,"2017 Annuals/"),12,12,text=textsize)
  
  #   ---- Ordered Within Flower Type 
  # totalannual %>%
  #   dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>%
  #   dplyr::rename(TotalPollinatorVisits = !!sym(paste0("Total",insect))) %>%
  #   dplyr::mutate(VisitsPerMin = TotalPollinatorVisits / SamplingPeriods) %>%
  #   getPoissonMeans() %>%
  #   dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
  #   dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
  #   makeTallBars(paste0("2017 Annuals -- ",insectClean," -- Ordered within Flower Type"),"","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2017 Annuals/"),12,12,text=textsize)
  
  #   ---- Ordered Over All:  Flower Type as Cultivar
  totalannualAll %>% 
    dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>%  
    dplyr::rename(TotalPollinatorVisits = !!sym(paste0("Total",insect))) %>% 
    dplyr::mutate(VisitsPerMin = TotalPollinatorVisits / SamplingPeriods) %>% 
    getPoissonMeans() %>%
    dplyr::arrange(dplyr::desc(b0)) %>%
    dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
    makeTallBars(paste0("2017 Annuals -- All Together -- ",insectClean," -- Ordered Over All"),"","Visits / Sampling Period with 95% Confidence Interval","Flower Type Comparisons",paste0(out,"2017 Annuals/"),12,12,text=textsize)
}

# Investigate relationships with cutpoints only.  
begonia2017 <- readr::read_csv(paste0(path2017,"/2017AnnualsR.csv")) %>% dplyr::filter(FlowerType == "Begonia") %>% fitFT(2017,"SamplingPeriods")
geranium2017 <- readr::read_csv(paste0(path2017,"/2017AnnualsR.csv")) %>% dplyr::filter(FlowerType == "Geranium") %>% fitFT(2017,"SamplingPeriods")
ngimpatiens2017 <- readr::read_csv(paste0(path2017,"/2017AnnualsR.csv")) %>% dplyr::filter(FlowerType == "NGImpatiens") %>% fitFT(2017,"SamplingPeriods")
impatiens2017 <- readr::read_csv(paste0(path2017,"/2017AnnualsR.csv")) %>% dplyr::filter(FlowerType == "Impatiens") %>% fitFT(2017,"SamplingPeriods")
pansy2017 <- readr::read_csv(paste0(path2017,"/2017AnnualsR.csv")) %>% dplyr::filter(FlowerType == "Pansy") %>% fitFT(2017,"SamplingPeriods")
petunia2017 <- readr::read_csv(paste0(path2017,"/2017AnnualsR.csv")) %>% dplyr::filter(FlowerType == "Petunia") %>% fitFT(2017,"SamplingPeriods")

# summary(begonia2017$poisson$goodL[[begonia2017$poisson$top[begonia2017$poisson$top$formula == "z0_1z",]$goodN]])
# summary(geranium2017$poisson$goodL[[geranium2017$poisson$top[geranium2017$poisson$top$formula == "z0_1z",]$goodN]])
# summary(ngimpatiens2017$poisson$goodL[[ngimpatiens2017$poisson$top[ngimpatiens2017$poisson$top$formula == "z0_1z",]$goodN]])
# summary(impatiens2017$poisson$goodL[[impatiens2017$poisson$top[impatiens2017$poisson$top$formula == "z0_1z",]$goodN]])
# summary(pansy2017$poisson$goodL[[pansy2017$poisson$top[pansy2017$poisson$top$formula == "z0_1z",]$goodN]])
# summary(petunia2017$poisson$goodL[[petunia2017$poisson$top[petunia2017$poisson$top$formula == "z0_1z",]$goodN]])

annualModels2017 <- rbind(begonia2017$poisson$top,
                          geranium2017$poisson$top,
                          ngimpatiens2017$poisson$top,
                          impatiens2017$poisson$top,
                          pansy2017$poisson$top,
                          petunia2017$poisson$top)

utils::write.csv(annualModels2017,paste0(out,"2017 Annuals/annualModels2017.csv"))


# Compare annuals by FlowerType.
lettered(totalannual,"Annuals","2017 Annuals -- Flower Type Pollinator Means & Tukey Lettering","",
         "Flower Type Comparisons","Visits / Sampling Period",c("Begonia","Geranium","Impatiens","NGImpatiens","Pansy","Petunia"),out,2017,12,12,TRUE,TRUE,text=textsize)



rm(totalannual,totalannualAll,a1,a2,a3,a4,a5,a6,aa,aa2,allaDeviances,allptDiffs,annualMeans,annualMeans2,annualMeans3,annualMeansAll,annualMeansAll2,annualMeansAll3,t1p,ggdf2,ggdfAll2,insectClean,insects,insectsClean,insect,begonia2017,geranium2017,impatiens2017,ngimpatiens2017,pansy2017,petunia2017,annualModels2017)
























#   ---- Combine all data into one spreadsheet.
totalannual <- dplyr::bind_rows(readr::read_csv(paste0(path2018,"/2018AnnualsR.csv"))) %>% 
  dplyr::mutate(Cultivar=factor(Cultivar)) %>% 
  dplyr::mutate(Block=factor(rep(seq(1,6),25))) %>% 
  dplyr::mutate(FlowerType=factor(FlowerType))

#   ---- Analysis of deviance for each FlowerType.
a1 <- totalannual %>%
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>%
  anoDev("Begonia",annuals=TRUE,blocks=FALSE)

a2 <- totalannual %>%
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>%
  anoDev("Geranium",annuals=TRUE,blocks=FALSE)

a3 <- totalannual %>%
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>%
  anoDev("Impatiens",annuals=TRUE,blocks=FALSE)

a4 <- totalannual %>%
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>%
  anoDev("NGImpatiens",annuals=TRUE,blocks=FALSE)

a5 <- totalannual %>%
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>%
  anoDev("Pansy",annuals=TRUE,blocks=FALSE)

a6 <- totalannual %>%
  dplyr::mutate(Cultivar=as.factor(Cultivar)) %>%
  anoDev("Petunia",annuals=TRUE,blocks=FALSE)

#   ---- Call Flower Type the same thing so we can use it in anoDev.  
totalannualAll <- totalannual %>% 
  dplyr::mutate(Cultivar = FlowerType) %>%  
  dplyr::mutate(FlowerType = "All Together")

aa <- totalannualAll %>% 
  anoDev(ft="All Together",annuals=TRUE,blocks=FALSE)

aa2 <- totalannual %>% 
  anoDev(ft=c("Begonia","Geranium","Impatiens","NGImpatiens","Pansy","Petunia"),annuals=TRUE,blocks=FALSE)

#   ---- Compile Tukey letters.
tukeyLetters <- aa2$c
utils::write.csv(tukeyLetters,paste0(out,"2018 Annuals/tukeyLettersAnnuals2018.csv"),row.names=FALSE)


#   ---- Compile p-values from analyses of deviance.  
allaDeviances <- rbind(a1$a,a2$a,a3$a,a4$a,a5$a,a6$a,aa$a)
allaDeviances <- allaDeviances[!is.na(allaDeviances$Df),]
rownames(allaDeviances) <- NULL
write.csv(allaDeviances,paste0(out,"2018 Annuals/allpDeviancesAnnuals2018.csv"),row.names=FALSE)

#   ---- Compile Tukey-type stats.  Reuse names expected for makeTallBars.  Rename in the function. 
allptDiffs <- as.data.frame(rbind(a1$i,a2$i,a3$i,a4$i,a5$i,a6$i,aa$i))
colnames(allptDiffs) <- c("Cultivar","FlowerType","b0","lo2.5","hi97.5","calpha","pvalue")
allptDiffs$b0 <- ifelse(is.infinite(allptDiffs$hi97.5),0,allptDiffs$b0)
allptDiffs$hi97.5 <- ifelse(is.infinite(allptDiffs$hi97.5),0,allptDiffs$hi97.5)
allptDiffs <- allptDiffs[!(allptDiffs$lo2.5 == allptDiffs$hi97.5),]
allptDiffs$Cultivar <- gsub("-","/",allptDiffs$Cultivar)
rownames(allptDiffs) <- NULL
allptDiffs <- allptDiffs %>% dplyr::arrange(FlowerType,desc(b0))
write.csv(allptDiffs,paste0(out,"2018 Annuals/allptDiffsAnnuals2018.csv"),row.names=FALSE)

#   ---- Individual Cultivar within FlowerType graphical comparisons.  
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Begonia",],"2018 Begonia -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Annuals/"),12,6,TRUE,scales::hue_pal()(3)[1],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Geranium",],"2018 Geranium -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Annuals/"),12,10,TRUE,scales::hue_pal()(3)[2],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Impatiens",],"2018 Impatiens -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Annuals/"),12,10,TRUE,scales::hue_pal()(3)[3],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "NGImpatiens",],"2018 NGImpatiens -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Annuals/"),12,6,TRUE,scales::hue_pal()(3)[1],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Pansy",],"2018 Pansy -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Annuals/"),12,10,TRUE,scales::hue_pal()(3)[2],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "Petunia",],"2018 Petunia -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Annuals/"),12,10,TRUE,scales::hue_pal()(3)[3],text=textsize)
makeTallBars(allptDiffs[allptDiffs$FlowerType == "All Together",],"2018 All Together -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Annuals/"),12,10,TRUE,scales::hue_pal()(3)[3],text=textsize)

#   ---- Get Cultivar within Flower Type means.  
annualMeans <- getPoissonMeans(totalannual)
utils::write.csv(annualMeans,paste0(out,"2018 Annuals/annualsMeans2018.csv"),row.names=FALSE)

annualMeansAll <- getPoissonMeans(totalannualAll)
utils::write.csv(annualMeansAll,paste0(out,"2018 Annuals/annualsMeansAll2018.csv"),row.names=FALSE)


#   ---- Make graphs with all output together.  Group by FlowerType, then sort by mean estimate.
annualMeans2 <- annualMeans %>% 
  dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
  dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) 
makeTallBars(annualMeans2,"2018 Annuals -- Visits per Sampling Period within Flower Type","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2018 Annuals/"),6,8,text=textsize)
stackedBars(annualMeans2,"2018 Annuals -- Visits per Sampling Period by Insect Status","","Visits / Sampling Period","Flower Type - Cultivar",paste0(out,"2018 Annuals/"),6,8,text=textsize)

#   ---- Get means and errors per Cultivar, with each Cultivar alone.  
t1p <- table1plot(totalannual,"Cultivar","FlowerType")
utils::write.csv(t1p,paste0(out,"2018 Annuals/annualsMeans2018EachCultivarAlone.csv"),row.names=FALSE)
makeTallBars(t1p,"2018 Annuals -- All Means and Errors","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2018 Annuals/"),6,8,text=textsize2)


annualMeansAll2 <- annualMeansAll %>% 
  dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
  dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) 
makeTallBars(annualMeansAll2,"2018 Annuals -- All Together -- Visits per Sampling Period within Flower Type","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2018 Annuals/"),6,8,text=textsize)
stackedBars(annualMeansAll2,"2018 Annuals -- All Together -- Visits per Sampling Period by Insect Status","","Visits / Sampling Period","Flower Type - Cultivar",paste0(out,"2018 Annuals/"),6,8,text=textsize)


#   ---- Make graphs with all output together.  Order over all mean estimates.  
annualMeans3 <- annualMeans %>% 
  dplyr::arrange(dplyr::desc(b0))
makeTallBars(annualMeans3,"2018 Annuals -- Visits per Sampling Period","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2018 Annuals/"),6,8,text=textsize)

annualMeansAll3 <- annualMeansAll %>% 
  dplyr::arrange(dplyr::desc(b0))
makeTallBars(annualMeansAll3,"2018 Annuals -- All Together -- Visits per Sampling Period","","Visits / Sampling Period with 95% Confidence Interval","Cultivar",paste0(out,"2018 Annuals/"),6,8,text=textsize)


#   ---- Prepare dataframe, group by Flower Type, then largest to smallest.  
ggdf <- allptDiffs %>% 
  dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
  dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
  dplyr::filter(FlowerType != "All Together")
makeTallBars(ggdf,"2018 Annuals -- Tukey Ratios -- Ordered within Flower Type","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Annuals/"),12,12,TRUE,text=textsize)

ggdfAll <- allptDiffs %>% 
  dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
  dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
  dplyr::filter(FlowerType == "All Together") 
makeTallBars(ggdfAll,"2018 Annuals -- All Together -- Tukey Ratios","","Ratios with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Annuals/"),12,12,TRUE,text=textsize)

#   ---- Prepare dataframe, order over all mean ratios.  
# ggdf2 <- allptDiffs %>% 
#   dplyr::arrange(dplyr::desc(b0)) %>%
#   dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) 
# makeTallBars(ggdf2,"2018 Annuals -- Tukey Ratios -- Ordered Over All","Ratios with 95% Confidence Interval","Cultivar Comparisons","L:/Jason/pollinatorAttractiveness/inst/Output/2018 Annuals/",12,12,TRUE)


#   ---- Insects.
insects <- c("Syrphidae","ApisMellifera","BombusImpatiens","OtherDiptera","OtherBees","Wasps")
insectsClean <- c("Syrphidae","Apis Mellifera","Bombus Impatiens","Other Diptera","Other Bees","Wasps") 
for(insect in insects){
  
  insectClean <- insectsClean[match(insect,insects)]
  
  #   ---- Ordered Over All 
  totalannual %>% 
    dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>% 
    dplyr::rename(TotalPollinatorVisits = !!sym(paste0("Total",insect))) %>% 
    dplyr::mutate(VisitsPerMin = TotalPollinatorVisits / SamplingPeriods) %>% 
    getPoissonMeans() %>%
    dplyr::arrange(dplyr::desc(b0)) %>%
    dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
    makeTallBars(paste0("2018 Annuals -- ",insectClean," -- Ordered Over All"),"","Visits / Sampling Period with 95% Confidence Interval","Cultivar Comparisons",paste0(out,"2018 Annuals/"),12,12,text=textsize)
  
  #   ---- Ordered Within Flower Type 
  # totalannual %>% 
  #   dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>% 
  #   dplyr::rename(PollinatorVisits = !!sym(paste0("Total",insect))) %>% 
  #   dplyr::mutate(VisitsPerMin = PollinatorVisits / SamplingPeriods) %>% 
  #   getPoissonMeans() %>%
  #   dplyr::arrange(FlowerType,dplyr::desc(b0)) %>%
  #   dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
  #   makeTallBars(paste0("2018 Annuals -- ",insectClean," -- Ordered within Flower Type"),"","Ratios with 95% Confidence Interval","Cultivar Comparisons","L:/Jason/pollinatorAttractiveness/inst/Output/2018 Annuals/",12,12,text=textsize)
  
  #   ---- Ordered Over All:  Flower Type as Cultivar
  totalannualAll %>% 
    dplyr::select(FlowerType,Cultivar,SamplingPeriods,paste0("Total",insect),paste0("Collected",insect),paste0("Observed",insect)) %>%  
    dplyr::rename(TotalPollinatorVisits = !!sym(paste0("Total",insect))) %>% 
    dplyr::mutate(VisitsPerMin = TotalPollinatorVisits / SamplingPeriods) %>% 
    getPoissonMeans() %>%
    dplyr::arrange(dplyr::desc(b0)) %>%
    dplyr::mutate(Cultivar=factor(Cultivar,levels=Cultivar)) %>%
    makeTallBars(paste0("2018 Annuals -- All Together -- ",insectClean," -- Ordered Over All"),"","Visits / Sampling Period with 95% Confidence Interval","Flower Type Comparisons",paste0(out,"2018 Annuals/"),12,12,text=textsize)
}


# Investigate relationships with color, flower area, and spiders.  
begonia2018 <- readr::read_csv(paste0(path2018,"/2018AnnualsR.csv")) %>% dplyr::filter(FlowerType == "Begonia") %>% fitFT(2018,"SamplingPeriods")
geranium2018 <- readr::read_csv(paste0(path2018,"/2018AnnualsR.csv")) %>% dplyr::filter(FlowerType == "Geranium") %>% fitFT(2018,"SamplingPeriods")
ngimpatiens2018 <- readr::read_csv(paste0(path2018,"/2018AnnualsR.csv")) %>% dplyr::filter(FlowerType == "NGImpatiens") %>% fitFT(2018,"SamplingPeriods")
impatiens2018 <- readr::read_csv(paste0(path2018,"/2018AnnualsR.csv")) %>% dplyr::filter(FlowerType == "Impatiens") %>% fitFT(2018,"SamplingPeriods")
pansy2018 <- readr::read_csv(paste0(path2018,"/2018AnnualsR.csv")) %>% dplyr::filter(FlowerType == "Pansy") %>% fitFT(2018,"SamplingPeriods")
petunia2018 <- readr::read_csv(paste0(path2018,"/2018AnnualsR.csv")) %>% dplyr::filter(FlowerType == "Petunia") %>% fitFT(2018,"SamplingPeriods")

summary(begonia2018$poisson$goodL[[begonia2018$poisson$top[begonia2018$poisson$top$formula == "z0_1z",]$goodN]])
summary(geranium2018$poisson$goodL[[geranium2018$poisson$top[geranium2018$poisson$top$formula == "z0_1z",]$goodN]])
summary(ngimpatiens2018$poisson$goodL[[ngimpatiens2018$poisson$top[ngimpatiens2018$poisson$top$formula == "z0_1z",]$goodN]])
summary(impatiens2018$poisson$goodL[[impatiens2018$poisson$top[impatiens2018$poisson$top$formula == "z0_1z",]$goodN]])
summary(pansy2018$poisson$goodL[[pansy2018$poisson$top[pansy2018$poisson$top$formula == "z0_1z",]$goodN]])
summary(petunia2018$poisson$goodL[[petunia2018$poisson$top[petunia2018$poisson$top$formula == "z0_1z",]$goodN]])

annualModels2018 <- rbind(begonia2018$poisson$top,
                          geranium2018$poisson$top,
                          ngimpatiens2018$poisson$top,
                          impatiens2018$poisson$top,
                          pansy2018$poisson$top,
                          petunia2018$poisson$top)

utils::write.csv(annualModels2018,paste0(out,"2018 Annuals/annualModels2018.csv"))


# Compare annuals by FlowerType.
lettered(totalannual,"Annuals","2018 Annuals -- Flower Type Pollinator Means & Tukey Lettering","",
         "Flower Type Comparisons","Visits / Sampling Period",c("Begonia","Geranium","Impatiens","NGImpatiens","Pansy","Petunia"),out,2018,12,12,TRUE,TRUE,text=textsize)


rm(totalannual,totalannualAll,a1,a2,a3,a4,a5,a6,aa,aa2,allaDeviances,allptDiffs,annualMeans,annualMeans2,annualMeans3,annualMeansAll,annualMeansAll2,annualMeansAll3,t1p,ggdf,ggdfAll,insectClean,insects,insectsClean,insect,
   begonia2018,geranium2018,ngimpatiens2018,impatiens2018,pansy2018,petunia2018,annualModels2018)




