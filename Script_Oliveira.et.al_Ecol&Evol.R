# Oliveira, I.F., Baccaro, F.B., Werneck, F.P., Haugaasen, T. Seasonal flooding decreases fruit-feeding butterfly species dominance and increases spatial turnover in floodplain forests of central Amazonia. Manuscript submitted to Ecology and Evolution (2022).

#
library(ade4)
library(vegan)
library(ggplot2)
library(reshape)
library(cowplot)

#Fruit-feeding butterfly richness and abundance
#Combined traps (understory and canopy as sampling units)
#Rarefaction curves
library(iNEXT)

####All (combined values from the three forests types)
df_traps<-read.table("df_traps.txt", h=T)

# Sample-based
df_traps_cheia <- decostand(subset(df_traps,Season=="Flooded")[,-c(1:6)], "pa")
df_traps_cheia <- t(df_traps_cheia[,colSums(df_traps_cheia)>0])

df_traps_seca <- decostand(subset(df_traps,Season=="Dry")[,-c(1:6)], "pa")
df_traps_seca <- t(df_traps_seca[,colSums(df_traps_seca)>0])

all_inci<-lapply(list(But.flooded = df_traps_cheia, But.dry= df_traps_seca), as.incfreq)

rar_sb_all <- iNEXT (all_inci, endpoint = 180, datatype = "incidence_freq")
rar_sb_all.1 <- ggiNEXT (rar_sb_all, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Sample-based frequency") + ylab ("Number of species") + theme_bw (base_size = 18)+ xlim(0,180)
rar_sb_all.1
rar_sb_all.1.2 <- rar_sb_all.1 + scale_colour_manual(values=c("gray45", "dodgerblue1"))
rar_sb_all.1.3<- rar_sb_all.1.2 + scale_fill_manual(values=c("gray45", "dodgerblue1"))
rar_sb_all.1.3
rar_sb_all.1.4<- rar_sb_all.1.3 + theme (legend.position = "right") + annotate ("text", x=0, y=68, label="All", size =6)
rar_sb_all.1.4

#qD=0
rar_all_q0 <- iNEXT (all_inci, q=0, endpoint = 300, datatype = "abundance")
rar_all_q0.1 <- ggiNEXT (rar_all_q0,type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Number of individuals") + ylab ("Number of species") + theme_bw (base_size = 18) + ylim(0,90)
rar_all_q0.1.2 <- rar_all_q0.1 + scale_colour_manual(values=c("gray45", "dodgerblue1"))
rar_all_q0.1.3<- rar_all_q0.1.2 + scale_fill_manual(values=c("gray45", "dodgerblue1"))
rar_all_q0.1.4<- rar_all_q0.1.3 + theme (legend.position = "right") + annotate ("text", x=5, y=90, label="All", size =6)
rar_all_q0.1.4

#qD=1
rar_all_q1 <- iNEXT (all_inci, q=1, endpoint = 300, datatype = "abundance")
rar_all_q1.1 <- ggiNEXT (rar_all_q1, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Number of individuals") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_all_q1.1.2 <- rar_all_q1.1 + scale_colour_manual(values=c("gray45", "dodgerblue1"))
rar_all_q1.1.3<- rar_all_q1.1.2 + scale_fill_manual(values=c("gray45", "dodgerblue1"))
rar_all_q1.1.4<- rar_all_q1.1.3 + theme (legend.position = "right") + annotate ("text", x=5, y=90, label="All", size =6)
rar_all_q1.1.4

#qD=2
rar_all_q2 <- iNEXT (all_inci, q=2, endpoint = 300, datatype = "abundance")
rar_all_q2.1 <- ggiNEXT (rar_all_q2, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Number of individuals") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_all_q2.1.2 <- rar_all_q2.1 + scale_colour_manual(values=c("gray45", "dodgerblue1"))
rar_all_q2.1.3<- rar_all_q2.1.2 + scale_fill_manual(values=c("gray45", "dodgerblue1"))
rar_all_q2.1.4<- rar_all_q2.1.3 + theme (legend.position = "right") + annotate ("text", x=5, y=90, label="All", size =6)
rar_all_q2.1.4

#Completeness
rar_all_compl <- iNEXT (all_inci, datatype = "abundance")
rar_all_compl.1 <- ggiNEXT (rar_all_compl, type = 3, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Sample completeness") + ylab ("Number of species") + theme_bw (base_size = 18) + xlim(0,1)
rar_all_compl.1.2 <- rar_all_compl.1  + scale_colour_manual(values=c("gray45", "dodgerblue1"))
rar_all_compl.1.3<- rar_all_compl.1.2+ scale_fill_manual(values=c("gray45", "dodgerblue1"))
rar_all_compl.1.4<- rar_all_compl.1.3 + theme (legend.position = "left") + annotate ("text", x=0.01, y=90, label="All", size =6)
rar_all_compl.1.4

####Terra firme
#Sample-based
df_traps_cheia.tf<- decostand(subset(df_traps, 
                                          Season=="Flooded" & Ambiente == "Terra_Firme")[,-c(1:8)], "pa")
df_traps_cheia.tf <- t(df_traps_cheia.tf[,colSums(df_traps_cheia.tf)>0])

df_traps_seca.tf<- decostand(subset(df_traps, 
                                         Season=="Dry" & Ambiente == "Terra_Firme")[,-c(1:8)], "pa")
df_traps_seca.tf <- t(df_traps_seca.tf[,colSums(df_traps_seca.tf)>0])

tf_inci<-lapply(list(tf.flooded = df_traps_cheia.tf, tf.dry= df_traps_seca.tf), as.incfreq)

rar_sb_tf <- iNEXT (tf_inci, datatype = "incidence_freq")
rar_sb_tf.1 <- ggiNEXT (rar_sb_tf, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Sample-based frequency") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_sb_tf.1.2 <- rar_sb_tf.1 + scale_colour_manual(values=c("darkgreen","lightgreen"))
rar_sb_tf.1.3<- rar_sb_tf.1.2 + scale_fill_manual(values=c("darkgreen","lightgreen"))
rar_sb_tf.1.4<- rar_sb_tf.1.3 + theme (legend.position = "right") + annotate ("text", x=7.5, y=45, label="Terra firme", size =6)
rar_sb_tf.1.4

#qD=0
rar_tf_q0 <- iNEXT (tf_inci, q=0,endpoint = 100, datatype = "abundance")
rar_tf_q0.1 <- ggiNEXT (rar_tf_q0, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Number of individuals") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_tf_q0.1.2 <- rar_tf_q0.1 + scale_colour_manual(values=c("darkgreen","lightgreen"))
rar_tf_q0.1.3<- rar_tf_q0.1.2 + scale_fill_manual(values=c("darkgreen","lightgreen"))
rar_tf_q0.1.4<- rar_tf_q0.1.3 + theme (legend.position = "right") + annotate ("text", x=10, y=40, label="Terra firme", size = 6)
rar_tf_q0.1.4

#qD=1
rar_tf_q1 <- iNEXT (tf_inci, q=1,endpoint = 100, datatype = "abundance")
rar_tf_q1.1 <- ggiNEXT (rar_tf_q1, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Number of individuals") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_tf_q1.1.2 <- rar_tf_q1.1 + scale_colour_manual(values=c("darkgreen","lightgreen"))
rar_tf_q1.1.3<- rar_tf_q1.1.2 + scale_fill_manual(values=c("darkgreen","lightgreen"))
rar_tf_q1.1.4<- rar_tf_q1.1.3 + theme (legend.position = "right") + annotate ("text", x=10, y=40, label="Terra firme", size = 6)
rar_tf_q1.1.4

#qD=2
rar_tf_q2 <- iNEXT (tf_inci, q=2,endpoint = 100, datatype = "abundance")
rar_tf_q2.1 <- ggiNEXT (rar_tf_q2, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Number of individuals") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_tf_q2.1.2 <- rar_tf_q2.1 + scale_colour_manual(values=c("darkgreen","lightgreen"))
rar_tf_q2.1.3<- rar_tf_q2.1.2 + scale_fill_manual(values=c("darkgreen","lightgreen"))
rar_tf_q2.1.4<- rar_tf_q2.1.3 + theme (legend.position = "right") + annotate ("text", x=10, y=40, label="Terra firme", size = 6)
rar_tf_q2.1.4

#Completeness
rar_tf_c <- iNEXT (tf_inci, datatype = "abundance")
rar_tf_c.1 <- ggiNEXT (rar_tf_c, type = 3, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Sample completeness") + ylab ("Number of species") + theme_bw (base_size = 18) + xlim(0,1)
rar_tf_c.1.2 <- rar_tf_c.1 + scale_colour_manual(values=c("darkgreen","lightgreen"))
rar_tf_c.1.3<- rar_tf_c.1.2 + scale_fill_manual(values=c("darkgreen","lightgreen"))
rar_tf_c.1.4<- rar_tf_c.1.3 + theme (legend.position = "left") + annotate ("text", x=0.13, y=60, label="Terra firme", size =6)
rar_tf_c.1.4

####Várzea
#Sample-based
df_traps_cheia.vz<- decostand(subset(df_traps, 
                                          Season=="Flooded" & Ambiente == "Varzea")[,-c(1:8)], "pa")
df_traps_cheia.vz <- t(df_traps_cheia.vz[,colSums(df_traps_cheia.vz)>0])

df_traps_seca.vz<- decostand(subset(df_traps, 
                                         Season=="Dry" & Ambiente == "Varzea")[,-c(1:8)], "pa")
df_traps_seca.vz <- t(df_traps_seca.vz[,colSums(df_traps_seca.vz)>0])

vz_inci<-lapply(list(vz.flooded = df_traps_cheia.vz, vz.dry= df_traps_seca.vz), as.incfreq)

rar_sb_vz <- iNEXT (vz_inci, datatype = "incidence_freq")
rar_sb_vz.1 <- ggiNEXT (rar_sb_vz, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Sample-based frequency") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_sb_vz.1.2 <- rar_sb_vz.1 + scale_colour_manual(values=c("tan4","orange"))
rar_sb_vz.1.3<- rar_sb_vz.1.2 + scale_fill_manual(values=c("tan4","orange"))
rar_sb_vz.1.4<- rar_sb_vz.1.3 + theme (legend.position = "right") + annotate ("text", x=4.5, y=45, label="Várzea", size =6)
rar_sb_vz.1.4

#qD=0
rar_vz_q0 <- iNEXT (vz_inci, q=0,endpoint = 150, datatype = "abundance")
rar_vz_q0.1 <- ggiNEXT (rar_vz_q0, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Number of individuals") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_vz_q0.1.2 <- rar_vz_q0.1 + scale_colour_manual(values=c("tan4","orange"))
rar_vz_q0.1.3<- rar_vz_q0.1.2 + scale_fill_manual(values=c("tan4","orange"))
rar_vz_q0.1.4<- rar_vz_q0.1.3 + theme (legend.position = "right") + annotate ("text", x=15, y=40, label=" Várzea", size =6)
rar_vz_q0.1.4

#qD=1
rar_vz_q1 <- iNEXT (vz_inci, q=1,endpoint = 150, datatype = "abundance")
rar_vz_q1.1 <- ggiNEXT (rar_vz_q1, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Number of individuals") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_vz_q1.2 <- rar_vz_q1.1 + scale_colour_manual(values=c("tan4","orange"))
rar_vz_q1.3<- rar_vz_q1.2 + scale_fill_manual(values=c("tan4","orange"))
rar_vz_q1.4<- rar_vz_q1.3 + theme (legend.position = "right") + annotate ("text", x=15, y=40, label=" Várzea", size =6)
rar_vz_q1.4

#qD=2
rar_vz_q2 <- iNEXT (vz_inci, q=2,endpoint = 150, datatype = "abundance")
rar_vz_q2.1 <- ggiNEXT (rar_vz_q2, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Number of individuals") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_vz_q2.2 <- rar_vz_q2.1 + scale_colour_manual(values=c("tan4","orange"))
rar_vz_q2.3<- rar_vz_q2.2 + scale_fill_manual(values=c("tan4","orange"))
rar_vz_q2.4<- rar_vz_q2.3 + theme (legend.position = "right") + annotate ("text", x=15, y=40, label= "Várzea", size =6)
rar_vz_q2.4

#Completeness
rar_vz_c <- iNEXT (vz_inci, datatype = "abundance")
rar_vz_c.1 <- ggiNEXT (rar_vz_c, type = 3, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Sample completeness") + ylab ("Number of species") + theme_bw (base_size = 18) + xlim(0,1)
rar_vz_c.1.2 <- rar_vz_c.1  + scale_colour_manual(values=c("tan4","orange"))
rar_vz_c.1.3<- rar_vz_c.1.2 + scale_fill_manual(values=c("tan4","orange"))
rar_vz_c.1.4<- rar_vz_c.1.3 + theme (legend.position = "right") + annotate ("text", x=0.1, y=60, label=" Várzea", size =6)
rar_vz_c.1.4

####Igapó
#Sample-based
df_traps_cheia.ig<- decostand(subset(df_traps, 
                                          Season=="Flooded" & Ambiente == "Igapo")[,-c(1:8)], "pa")
df_traps_cheia.ig <- t(df_traps_cheia.ig[,colSums(df_traps_cheia.ig)>0])

df_traps_seca.ig<- decostand(subset(df_traps, 
                                         Season=="Dry" & Ambiente == "Igapo")[,-c(1:8)], "pa")
df_traps_seca.ig <- t(df_traps_seca.ig[,colSums(df_traps_seca.ig)>0])

ig_inci<-lapply(list(Ig.flooded = df_traps_cheia.ig, Ig.dry= df_traps_seca.ig), as.incfreq)

rar_sb_ig <- iNEXT (ig_inci, datatype = "incidence_freq")
rar_sb_ig.1 <- ggiNEXT (rar_sb_ig, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Sample-based frequency") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_sb_ig.1.2 <- rar_sb_ig.1 + scale_colour_manual(values=c("black","gray50"))
rar_sb_ig.1.3<- rar_sb_ig.1.2 + scale_fill_manual(values=c("black","gray50"))
rar_sb_ig.1.4<- rar_sb_ig.1.3 + theme (legend.position = "right") + annotate ("text", x=4, y=45, label="Igapó", size =6)
rar_sb_ig.1.4

#qD=0
rar_ig_q0 <- iNEXT (ig_inci, q=0,endpoint = 100, datatype = "abundance")
rar_ig_q0.1 <- ggiNEXT (rar_ig_q0, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Number of individuals") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_ig_q0.1.2 <- rar_ig_q0.1 + scale_colour_manual(values=c("black","gray50"))
rar_ig_q0.1.3<- rar_ig_q0.1.2 + scale_fill_manual(values=c("black","gray50"))
rar_ig_q0.1.4<- rar_ig_q0.1.3 + theme (legend.position = "right") + annotate ("text", x=5, y=40, label="Igapó", size =6)
rar_ig_q0.1.4

#qD=1
rar_ig_q1 <- iNEXT (ig_inci, q=1,endpoint = 100, datatype = "abundance")
rar_ig_q1.1 <- ggiNEXT (rar_ig_q1, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Number of individuals") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_ig_q1.2 <- rar_ig_q1.1 + scale_colour_manual(values=c("black","gray50"))
rar_ig_q1.3<- rar_ig_q1.2+ scale_fill_manual(values=c("black","gray50"))
rar_ig_q1.4<- rar_ig_q1.3 + theme (legend.position = "right") + annotate ("text", x=5, y=40, label="Igapó", size =6)
rar_ig_q1.4

#qD=2
rar_ig_q2 <- iNEXT (ig_inci, q=2,endpoint = 100, datatype = "abundance")
rar_ig_q2.1 <- ggiNEXT (rar_ig_q2, type = 1, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Number of individuals") + ylab ("Number of species") + theme_bw (base_size = 18)
rar_ig_q2.2 <- rar_ig_q2.1 + scale_colour_manual(values=c("black","gray50"))
rar_ig_q2.3<- rar_ig_q2.2 + scale_fill_manual(values=c("black","gray50"))
rar_ig_q2.4<- rar_ig_q2.3 + theme (legend.position = "right") + annotate ("text", x=5, y=40, label="Igapó", size =6)
rar_ig_q2.4

#Completness
rar_ig_c <- iNEXT (ig_inci, datatype = "abundance")
rar_ig_c.1 <- ggiNEXT (rar_ig_c, type = 3, se = TRUE, facet.var= "none", color.var = "site", grey=F) + xlab ("Sample completeness") + ylab ("Number of species") + theme_bw (base_size = 18) + xlim(0,1)
rar_ig_c.1.2 <- rar_ig_c.1  + scale_colour_manual(values=c("black","gray50"))
rar_ig_c.1.3<- rar_ig_c.1.2 + scale_fill_manual(values=c("black","gray50"))
rar_ig_c.1.4<- rar_ig_c.1.3 + theme (legend.position = "left") + annotate ("text", x=0.1, y=60, label="Igapó", size =6)
rar_ig_c.1.4

#######################################
#Species rank 
#Terra firme
tf_abu<-read.table("tf_abu.txt", h=T)
tf.floo<- colSums(tf_abu[1:3,])
tf.dry<- colSums(tf_abu[4:6,])

flooded<- data.frame(abudance = tf.floo[order(tf.floo, decreasing = TRUE)], rank = seq(1:length(tf.floo)))
dry<- data.frame(abudance = tf.dry[order(tf.dry, decreasing = TRUE)], rank = seq(1:length(tf.dry)))

plot(dry[dry[,1]!=0,][,1] ~ dry[dry[,1]!=0,][,2], type="n", xlab="Species rank", main="Terra firme", ylim=c(1,5), las=1, ylab="Abundance", bty="l", cex.axis=1.3, cex.lab=1.3)
lines(dry[dry[,1]!=0,][,1] ~ dry[dry[,1]!=0,][,2], col="darkgreen")
points(dry[dry[,1]!=0,][,1] ~ dry[dry[,1]!=0,][,2], pch=16, cex=1.5, col="darkgreen")
lines(flooded[flooded[,1]!=0,][,1] ~ flooded[flooded[,1]!=0,][,2], col="lightgreen")
points(flooded[flooded[,1]!=0,][,1] ~ flooded[flooded[,1]!=0,][,2], pch=18, cex=1.7, col="lightgreen")

#Várzea
vz_abu<-read.table("vz_abu.txt", h=T)
vz.floo<- colSums(vz_abu[1:3,])
vz.dry<- colSums(vz_abu[4:6,])

flooded<- data.frame(abudance = vz.floo[order(vz.floo, decreasing = TRUE)], rank = seq(1:length(vz.floo)))
dry<- data.frame(abudance = vz.dry[order(vz.dry, decreasing = TRUE)], rank = seq(1:length(vz.dry)))

plot(dry[dry[,1]!=0,][,1] ~ dry[dry[,1]!=0,][,2], type="n", xlab="Species rank", main="Várzea", ylim=c(1,40), las=1, ylab="Abundance", bty="l", cex.axis=1.3, cex.lab=1.3)
lines(dry[dry[,1]!=0,][,1] ~ dry[dry[,1]!=0,][,2], col="tan4")
points(dry[dry[,1]!=0,][,1] ~ dry[dry[,1]!=0,][,2], pch=16, cex=1.5, col="tan4")
lines(flooded[flooded[,1]!=0,][,1] ~ flooded[flooded[,1]!=0,][,2], col="orange")
points(flooded[flooded[,1]!=0,][,1] ~ flooded[flooded[,1]!=0,][,2], pch=18, cex=1.7, col="orange")

#Igapó
ig_abu<-read.table("ig_abu.txt", h=T)
ig.floo<- colSums(ig_abu[1:3,])
ig.dry<- colSums(ig_abu[4:6,])

flooded<- data.frame(abudance = ig.floo[order(ig.floo, decreasing = TRUE)], rank = seq(1:length(ig.floo)))
dry<- data.frame(abudance = ig.dry[order(ig.dry, decreasing = TRUE)], rank = seq(1:length(ig.dry)))

plot(dry[dry[,1]!=0,][,1] ~ dry[dry[,1]!=0,][,2], type="n", xlab="Species rank", main="Igapó", ylim=c(1,7), las=1, ylab="Abundance", bty="l", cex.axis=1.3, cex.lab=1.3)
lines(dry[dry[,1]!=0,][,1] ~ dry[dry[,1]!=0,][,2], col="black")
points(dry[dry[,1]!=0,][,1] ~ dry[dry[,1]!=0,][,2], pch=16, cex=1.5, col="black")
lines(flooded[flooded[,1]!=0,][,1] ~ flooded[flooded[,1]!=0,][,2], col="gray50")
points(flooded[flooded[,1]!=0,][,1] ~ flooded[flooded[,1]!=0,][,2], pch=18, cex=1.7, col="gray50")

########################################
#PERMANOVA
#Fruit-feeding butterfly composition across seasons and forest strata
#Separate forest types - transects as sampling units
df_transect<-read.table("df_transect_season_strata.txt", header=T)

#Terra firme
tf_strata<-(subset(df_transect, Habitat=="Terra_Firme"))
tf_strata.1<- (tf_strata[rowSums(tf_strata[,-c(1:6)])!=0,-c(1:6)])
tf_strata.2<- data.frame(tf_strata[rowSums(tf_strata[,-c(1:6)])!=0,c(1:6)], tf_strata.1)

tf_perm<-adonis2(tf_strata.2[,-c(1:6)] ~ Strata+Season, data = tf_strata.2, perm=999)
tf_perm

#Várzea
vz_strata<-(subset(df_transect, Habitat=="Varzea"))
vz_strata.1<- (vz_strata[rowSums(vz_strata[,-c(1:6)])!=0,-c(1:6)])
vz_strata.2<- data.frame(vz_strata[rowSums(vz_strata[,-c(1:6)])!=0,c(1:6)], vz_strata.1)

vz_perm<-adonis2(vz_strata.2[,-c(1:6)] ~ Strata+Season, data = vz_strata.2, perm=999)
vz_perm

#Igapó
ig_strata<-(subset(df_transect, Habitat=="Igapo"))
ig_strata.1<- (ig_strata[rowSums(ig_strata[,-c(1:6)])!=0,-c(1:6)])
ig_strata.2<- data.frame(vz_strata[rowSums(ig_strata[,-c(1:6)])!=0,c(1:6)], ig_strata.1)

ig_perm<-adonis2(ig_strata.2[,-c(1:6)] ~ Strata+Season, data = ig_strata.2, perm=999)
ig_perm

########################################
# Spatial 𝜷-diversity of fruit-feeding butterflies per season
# preparing the data
df_traps<-read.table("df_traps.txt", h=T)

# Terra firme
beta_tf <- subset(df_traps, Habitat == "Terra_Firme")
beta_tf_cheia<- subset(df_traps, Habitat == "Terra_Firme" & Season =="Flooded")
beta_tf_seca <- subset(df_traps, Habitat == "Terra_Firme" & Season =="Dry")

# Várzea
beta_vz <- subset(df_traps, Habitat == "Varzea")
beta_vz_cheia <- subset(df_traps, Habitat == "Varzea" & Season =="Flooded")
beta_vz_seca  <- subset(df_traps, Habitat == "Varzea" & Season =="Dry")

# Igapó
beta_ig <- subset(df_traps, Habitat == "Igapo")
beta_ig_cheia <- subset(df_traps, Habitat == "Igapo" & Season =="Flooded")
beta_ig_seca  <- subset(df_traps, Habitat == "Igapo" & Season =="Dry")

#Legendre's function
source("Beta diversidade com abundancia_FBB.R")
# Terra firme
# High-water season
beta.D <- as.matrix (beta_tf_cheia[rowSums(beta_tf_cheia[,-c(1:6)])!=0,-c(1:6)])
beta.tf.high <- beta.div.comp (beta.D, coef="BS",quant=TRUE) # beta diversity partition (turnover matrix; nestedness matrix; dissimilarity matrix; multisite beta-div = beta-div value, turnover value, nestedness value, %turnover, %nestedness)
beta.tf.high
# Low-water season
beta.D <- as.matrix (beta_tf_seca[rowSums(beta_tf_seca[,-c(1:6)])!=0,-c(1:6)])
beta.tf.low <- beta.div.comp (beta.D, coef="BS",quant=TRUE) # beta diversity partition (turnover matrix; nestedness matrix; dissimilarity matrix; multisite beta-div = beta-div value, turnover value, nestedness value, %turnover, %nestedness)
beta.tf.low

# Varzea
# High-water season
beta.D <- as.matrix (beta_vz_cheia[rowSums(beta_vz_cheia[,-c(1:6)])!=0,-c(1:6)])
beta.vz.high <- beta.div.comp (beta.D, coef="BS",quant=TRUE) # beta diversity partition (turnover matrix; nestedness matrix; dissimilarity matrix; multisite beta-div = beta-div value, turnover value, nestedness value, %turnover, %nestedness)
beta.vz.high
# Low-water season
beta.D <- as.matrix (beta_vz_seca[rowSums(beta_vz_seca[,-c(1:6)])!=0,-c(1:6)])
beta.vz.low <- beta.div.comp (beta.D, coef="BS",quant=TRUE) # beta diversity partition (turnover matrix; nestedness matrix; dissimilarity matrix; multisite beta-div = beta-div value, turnover value, nestedness value, %turnover, %nestedness)
beta.vz.low

# Igapó
# High-water season
beta.D <- as.matrix (beta_ig_cheia[rowSums(beta_ig_cheia[,-c(1:6)])!=0,-c(1:6)])
beta.ig.high <- beta.div.comp (beta.D, coef="BS",quant=TRUE) # beta diversity partition (turnover matrix; nestedness matrix; dissimilarity matrix; multisite beta-div = beta-div value, turnover value, nestedness value, %turnover, %nestedness)
beta.ig.high
# Low-water season
beta.D <- as.matrix (beta_ig_seca[rowSums(beta_ig_seca[,-c(1:6)])!=0,-c(1:6)])
beta.ig.low <- beta.div.comp (beta.D, coef="BS",quant=TRUE) # beta diversity partition (turnover matrix; nestedness matrix; dissimilarity matrix; multisite beta-div = beta-div value, turnover value, nestedness value, %turnover, %nestedness)
beta.ig.low

####Boostrap####
### function to permute the subsamples of communities ####
boot.div.comp<- function (x, n){
  temp <- sample (rownames (x), size = n, replace=F)
  beta.div.comp(as.matrix(x[temp,]), coef="BS", quant=TRUE)$turn_perc
}

boot.div.comp<- function (x, n){
  temp <- sample (rownames (x), size = n, replace=F)
  beta.div.comp(as.matrix(x[temp,]), coef="BS", quant=TRUE)$nest_perc
}

boot.div.comp<- function (x, n){
  temp <- sample (rownames (x), size = n, replace=F)
  beta.div.comp(as.matrix(x[temp,]), coef="BS", quant=TRUE)$part[1]
}
### Bootstrap Terra firme ####
beta_tf_cheia_zero <- as.matrix (beta_tf_cheia[rowSums(beta_tf_cheia[,-c(1:6)])!=0,-c(1:6)])
beta_tf_seca_zero  <- as.matrix (beta_tf_seca[rowSums(beta_tf_seca[,-c(1:6)])!=0,-c(1:6)])

boot.tf.high <- replicate(99, boot.div.comp(x = beta_tf_cheia_zero,
                                            n = round(nrow(beta_tf_cheia_zero)/2, 0)))

boot.tf.low  <- replicate(99, boot.div.comp(x = beta_tf_seca_zero,
                                            n = round(nrow(beta_tf_seca_zero)/2, 0)))

### Bootstrap várzea ####
beta_vz_cheia_zero <- as.matrix (beta_vz_cheia[rowSums(beta_vz_cheia[,-c(1:6)])!=0,-c(1:6)])
beta_vz_seca_zero  <- as.matrix (beta_vz_seca[rowSums(beta_vz_seca[,-c(1:6)])!=0,-c(1:6)])

boot.vz.high <- replicate(99, boot.div.comp(x = beta_vz_cheia_zero,
                                            n = round(nrow(beta_vz_cheia_zero)/2, 0)))

boot.vz.low  <- replicate(99, boot.div.comp(x = beta_vz_seca_zero,
                                            n = round(nrow(beta_vz_seca_zero)/2, 0)))

## Bootstrap igapó ####
beta_ig_cheia_zero <- as.matrix (beta_ig_cheia[rowSums(beta_ig_cheia[,-c(1:6)])!=0,-c(1:6)])
beta_ig_seca_zero  <- as.matrix (beta_ig_seca[rowSums(beta_ig_seca[,-c(1:6)])!=0,-c(1:6)])

boot.ig.high <- replicate(99, boot.div.comp(x = beta_ig_cheia_zero,
                                            n = round(nrow(beta_ig_cheia_zero)/2, 0)))

boot.ig.low  <- replicate(99, boot.div.comp(x = beta_ig_seca_zero,
                                            n = round(nrow(beta_ig_seca_zero)/2, 0)))
### Plots ####
# Table
#Turnover
tab.gg <- data.frame(
  Habitat = rep(c("Terra firme", "Várzea", "Igapó"), each=2),
  Season = rep(c("High-water", "Low-water"), 3),
  obs = c(beta.tf.high$turn_perc, beta.tf.low$turn_perc, beta.vz.high$turn_perc,
          beta.vz.low$turn_perc, beta.ig.high$turn_perc, beta.ig.low$turn_perc),
  std.boot = c(sd(boot.tf.high), sd(boot.tf.low), sd(boot.vz.high), sd(boot.vz.low),
               sd(boot.ig.high), sd(boot.ig.low)))

tab.gg$Habitat <- factor(tab.gg$Habitat, levels = c("Terra firme", "Várzea", "Igapó"))

ggplot(data=tab.gg, aes(x=Season, y=obs, fill=Habitat)) +
  geom_col(position = position_dodge(width=0.9)) + 
  geom_errorbar(aes(ymin = obs-(2*std.boot), 
                    ymax = ifelse(obs+(2*std.boot)>1,1,obs+(2*std.boot))), width = 0.1, 
                position = position_dodge(width=0.9)) +
  theme_cowplot(font_size = 18) +
  scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values=c("forestgreen","tan2","grey30"))+
  ylab("Turnover") +
  theme(legend.position = "none") +
  facet_wrap(~Habitat)
ggsave(filename = "bootstrap_isa.png", width = 10, height = 6)

#Nestedness
tab.gg <- data.frame(
  Habitat = rep(c("Terra firme", "Várzea", "Igapó"), each=2),
  Season = rep(c("High-water", "Low-water"), 3),
  obs = c(beta.tf.high$nest_perc, beta.tf.low$nest_perc, beta.vz.high$nest_perc,
          beta.vz.low$nest_perc, beta.ig.high$nest_perc, beta.ig.low$nest_perc),
  std.boot = c(sd(boot.tf.high), sd(boot.tf.low), sd(boot.vz.high), sd(boot.vz.low),
               sd(boot.ig.high), sd(boot.ig.low)))

tab.gg$Habitat <- factor(tab.gg$Habitat, levels = c("Terra firme", "Várzea", "Igapó"))

ggplot(data=tab.gg, aes(x=Season, y=obs, fill=Habitat)) +
  geom_col(position = position_dodge(width=0.9)) + 
  geom_errorbar(aes(ymin = obs-(2*std.boot), 
                    ymax = ifelse(obs+(2*std.boot)>1,1,obs+(2*std.boot))), width = 0.1, 
                position = position_dodge(width=0.9)) +
  theme_cowplot(font_size = 18) +
  scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values=c("forestgreen","tan2","grey30"))+
  ylab("Nestedness") +
  #ylim (0,1)+
  theme(legend.position = "none") +
  facet_wrap(~Habitat)
ggsave(filename = "bootstrap_isa_nest.png", width = 10, height = 6)

### Beta-diversity per strata ####
df_traps_strata <- read.table("df_traps_strata.txt", header=TRUE)

# preparing the data
# Terra firme
tf_cheia_under<- subset(df_traps_strata, Habitat == "Terra_Firme" & Season == "Flooded" &
                          Strata == "Understory")
tf_cheia_canop<- subset(df_traps_strata, Habitat == "Terra_Firme" & Season == "Flooded" &
                          Strata == "Canopy")
tf_seca_under <- subset(df_traps_strata, Habitat == "Terra_Firme" & Season == "Dry" &
                          Strata == "Understory")
tf_seca_canop <- subset(df_traps_strata, Habitat == "Terra_Firme" & Season == "Dry" &
                          Strata == "Canopy")

# Várzea
vz_cheia_under<- subset(df_traps_strata, Habitat == "Varzea" & Season == "Flooded" &
                          Strata == "Understory")
vz_cheia_canop<- subset(df_traps_strata, Habitat == "Varzea" & Season == "Flooded" &
                          Strata == "Canopy")
vz_seca_under <- subset(df_traps_strata, Habitat == "Varzea" & Season == "Dry" &
                          Strata == "Understory")
vz_seca_canop <- subset(df_traps_strata, Habitat == "Varzea" & Season == "Dry" &
                          Strata == "Canopy")

# Igapó
ig_cheia_under<- subset(df_traps_strata, Habitat == "Igapo" & Season == "Flooded" &
                          Strata == "Understory")
ig_cheia_canop<- subset(df_traps_strata, Habitat == "Igapo" & Season == "Flooded" &
                          Strata == "Canopy")
ig_seca_under <- subset(df_traps_strata, Habitat == "Igapo" & Season == "Dry" &
                          Strata == "Understory")
ig_seca_canop <- subset(df_traps_strata, Habitat == "Igapo" & Season == "Dry" &
                          Strata == "Canopy")

### Bootstrap Terra firme ####
tf_cheia_under_zero <- as.matrix (tf_cheia_under[rowSums(tf_cheia_under[,-c(1:7)])!=0,-c(1:7)])
tf_seca_under_zero  <- as.matrix (tf_seca_under[rowSums(tf_seca_under[,-c(1:7)])!=0,-c(1:7)])

boot.tf.high.under <- replicate(99, boot.div.comp(x = tf_cheia_under_zero,
                                                  n = round(nrow(tf_cheia_under_zero)/2, 0)))

boot.tf.low.under  <- replicate(99, boot.div.comp(x = tf_seca_under_zero,
                                                  n = round(nrow(tf_seca_under_zero)/2, 0)))

tf_cheia_canop_zero <- as.matrix (tf_cheia_canop[rowSums(tf_cheia_canop[,-c(1:7)])!=0,-c(1:7)])
tf_seca_canop_zero  <- as.matrix (tf_seca_canop[rowSums(tf_seca_canop[,-c(1:7)])!=0,-c(1:7)])

boot.tf.high.canop <- replicate(99, boot.div.comp(x = tf_cheia_canop_zero,
                                                  n = round(nrow(tf_cheia_canop_zero)/2, 0)))

boot.tf.low.canop  <- replicate(99, boot.div.comp(x = tf_seca_canop_zero,
                                                  n = round(nrow(tf_seca_canop_zero)/2, 0)))

### Bootstrap Várzea ####
vz_cheia_under_zero <- as.matrix (vz_cheia_under[rowSums(vz_cheia_under[,-c(1:7)])!=0,-c(1:7)])
vz_seca_under_zero  <- as.matrix (vz_seca_under[rowSums(vz_seca_under[,-c(1:7)])!=0,-c(1:7)])

boot.vz.high.under <- replicate(99, boot.div.comp(x = vz_cheia_under_zero,
                                                  n = round(nrow(vz_cheia_under_zero)/2, 0)))

boot.vz.low.under  <- replicate(99, boot.div.comp(x = vz_seca_under_zero,
                                                  n = round(nrow(vz_seca_under_zero)/2, 0)))

vz_cheia_canop_zero <- as.matrix (vz_cheia_canop[rowSums(vz_cheia_canop[,-c(1:7)])!=0,-c(1:7)])
vz_seca_canop_zero  <- as.matrix (vz_seca_canop[rowSums(vz_seca_canop[,-c(1:7)])!=0,-c(1:7)])

boot.vz.high.canop <- replicate(99, boot.div.comp(x = vz_cheia_canop_zero,
                                                  n = round(nrow(vz_cheia_canop_zero)/2, 0)))

boot.vz.low.canop  <- replicate(99, boot.div.comp(x = vz_seca_canop_zero,
                                                  n = round(nrow(vz_seca_canop_zero)/2, 0)))

### Bootstrap Igapó ####
ig_cheia_under_zero <- as.matrix (ig_cheia_under[rowSums(ig_cheia_under[,-c(1:7)])!=0,-c(1:7)])
ig_seca_under_zero  <- as.matrix (ig_seca_under[rowSums(ig_seca_under[,-c(1:7)])!=0,-c(1:7)])

boot.ig.high.under <- replicate(99, boot.div.comp(x = ig_cheia_under_zero,
                                                  n = round(nrow(ig_cheia_under_zero)/2, 0)))

boot.ig.low.under  <- replicate(99, boot.div.comp(x = ig_seca_under_zero,
                                                  n = round(nrow(ig_seca_under_zero)/2, 0)))

ig_cheia_canop_zero <- as.matrix (ig_cheia_canop[rowSums(ig_cheia_canop[,-c(1:7)])!=0,-c(1:7)])
ig_seca_canop_zero  <- as.matrix (ig_seca_canop[rowSums(ig_seca_canop[,-c(1:7)])!=0,-c(1:7)])

boot.ig.high.canop <- replicate(99, boot.div.comp(x = ig_cheia_canop_zero,
                                                  n = round(nrow(ig_cheia_canop_zero)/2, 0)))

boot.ig.low.canop  <- replicate(99, boot.div.comp(x = ig_seca_canop_zero,
                                                  n = round(nrow(ig_seca_canop_zero)/2, 0)))

### Plots ####
# Table
# Turnover
tab.gg.strata <- data.frame(
  Habitat = rep(c("Terra firme", "Várzea", "Igapó"), each=4),
  Season = rep(c("High-water", "Low-water"), 6),
  Strata = rep(c("Understory", "Canopy"), each=2),
  obs = c(
    beta.div.comp(tf_cheia_under_zero, coef="BS", quant=TRUE)$turn_perc, 
    beta.div.comp(tf_seca_under_zero, coef="BS", quant=TRUE)$turn_perc, 
    beta.div.comp(tf_cheia_canop_zero, coef="BS", quant=TRUE)$turn_perc, 
    beta.div.comp(tf_seca_canop_zero, coef="BS", quant=TRUE)$turn_perc,
    beta.div.comp(vz_cheia_under_zero, coef="BS", quant=TRUE)$turn_perc, 
    beta.div.comp(vz_seca_under_zero, coef="BS", quant=TRUE)$turn_perc, 
    beta.div.comp(vz_cheia_canop_zero, coef="BS", quant=TRUE)$turn_perc, 
    beta.div.comp(vz_seca_canop_zero, coef="BS", quant=TRUE)$turn_perc,
    beta.div.comp(ig_cheia_under_zero, coef="BS", quant=TRUE)$turn_perc, 
    beta.div.comp(ig_seca_under_zero, coef="BS", quant=TRUE)$turn_perc, 
    beta.div.comp(ig_cheia_canop_zero, coef="BS", quant=TRUE)$turn_perc, 
    beta.div.comp(ig_seca_canop_zero, coef="BS", quant=TRUE)$turn_perc),
  std.boot = c(sd(boot.tf.high.under), sd(boot.tf.low.under), 
               sd(boot.tf.high.canop), sd(boot.tf.low.canop),
               sd(boot.vz.high.under), sd(boot.vz.low.under), 
               sd(boot.vz.high.canop), sd(boot.vz.low.canop),
               sd(boot.ig.high.under), sd(boot.ig.low.under), 
               sd(boot.ig.high.canop), sd(boot.ig.low.canop)))

tab.gg.strata$Habitat <- factor(tab.gg$Habitat, levels = c("Terra firme", "Várzea", "Igapó"))
tab.gg.strata$std.boot[tab.gg.strata$std.boot == 0] <- NA

ggplot(data=tab.gg.strata, aes(x=Season, y=obs, fill=Habitat)) +
  geom_col(position = position_dodge(width=0.9)) + 
  geom_errorbar(aes(ymin = obs-(2*std.boot), 
                    ymax = ifelse(obs+(2*std.boot)>1,1,obs+(2*std.boot))), width = 0.1, 
                position = position_dodge(width=0.9)) +
  theme_cowplot(font_size = 14) +
  scale_fill_brewer(palette="Paired")+
  scale_fill_manual(values=c("forestgreen","tan2","grey30"))+
  ylab("Turnover") +
  theme(legend.position = "none") +
  coord_cartesian(ylim = c(0,1)) +
  #  scale_y_continuous(expand = expansion(mult = c(0,0))) +
  facet_wrap(Habitat ~ Strata, ncol = 6)
ggsave(filename = "bootstrap_strata_isa.png", width = 12, height = 6)

#END
########################################
