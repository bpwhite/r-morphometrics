# Created by Bryan P. White for morphometrics analysis, 2014
library(FactoMineR)
library(ggplot2)
library(reshape)

fissurellidae_data <- read.csv(file="MeDi_Matrix_length_110114.csv",head=TRUE,sep=",", row.names=1)

# Change IWW throughout to change variables
melted_data = melt(fissurellidae_data, id=c("Species","Length"))
species_list = levels(melted_data$Species)
variable_list = levels(melted_data$variable)

# Print regression stats for each variable vs. Length, for exapmle to find allometry
for(i in 1:length(variable_list)) {
  print(variable_list[i])
  for(j in 1:length(species_list)) {
    print(paste(cat("\t"),species_list[j]),sep="")
    sub_data = melted_data[ which(melted_data$Species==species_list[j]
                                  & melted_data$variable==variable_list[i]), ]
    model = lm(log10(value) ~ log10(Length), data=sub_data)
    
    print(summary(model)$coefficients)
  }
}

# Do this to get Length as a variable like the rest
melted_data2 = melt(fissurellidae_data, id=c("Species"))

# Stacked histograms
hist_plot = ggplot(melted_data2, aes(x = value, fill=Species))+
  #geom_histogram(binwidth = 1, position="dodge") +
  facet_wrap(~ variable, scales="free")+
  geom_density(alpha=.3)+
  xlab("Value (mm)")+ylab("Frequency")+
  theme_bw()
plot(hist_plot)
ggsave(hist_plot, filename="stacked_histograms_fissurellidae_dodge.pdf", width=9, height=5, units="in")

# Print a stacked plot of log scale graphs vs. a variable, e.g., IWW
melt_plot   = ggplot(melted_data, aes(log10(Length), log10(value),  group=Species)) +
  #melt_plot   = ggplot(melted_data, aes(Length, value,  group=Species)) +
  geom_point(aes(colour = Species, shape=Species))+
  geom_smooth(method="lm",se=FALSE, aes(linetype=Species,color=Species))+
  #geom_abline(slope=1)+
  #coord_cartesian(xlim = c(-5, 5))+
  #coord_cartesian(ylim = c(-5, 5))+
  facet_wrap(~ variable, scales="free")+
  xlab("log10(IWW)")+ylab("varible")+
  theme_bw()
plot(melt_plot)
ggsave(melt_plot, filename="melted_fissurellidae.pdf", width=8, height=5, units="in")

##############################
# PCA Analaysis with FactoMineR
# Perform PCA analysis and plot factor loadings
res.pca = PCA(fissurellidae_data, scale.unit=TRUE, ncp=2:12, graph=T,  quali.sup=c(13))
# Plot individuals in ordination space
plot.PCA(res.pca, axes=c(1, 2), choix="ind", habillage=13, cex=0.5)
# Report correlations and p-values for each value
dimdesc(res.pca, axes=c(1,2))
##############################

# Plot 95% confidence interval ellipses
concat = cbind.data.frame(fissurellidae_data[,13],res.pca$ind$coord)
ellipse.coord = coord.ellipse(concat,bary=T)
plot.PCA(res.pca,habillage=13,ellipse=ellipse.coord,cex=0.2)

# Assign species listings to this variable
species = res.pca$call$quali.sup$quali.sup[,1]
# Plot normal scatter plot of colored individuals along PC1 and PC2 
plot(res.pca$ind$coord, pch=c(19), col=species )

# Plot colored species plot using ggplot2
pca.dataframe = data.frame(res.pca$ind$coord)
pca_plot = ggplot(data=pca.dataframe,
                  aes(x = Dim.1, y=Dim.2, shape = species))+
  #scale_shape_manual(values=c(0,1))+
  #scale_fill_manual(values=c("black","white"))+
  geom_point(size=(3.5), aes(colour = species))+
  #geom_point(size=3, aes(colour = species))+
  theme_bw()+
  xlab("PC1 (31.91%)")+ylab("PC2 (23.93%)")
#theme(axis.text.x = element_text(angle = 45, hjust = 1))+
#scale_y_continuous(limits=c(0,12),breaks=seq(0, 12, 1))+
#theme(legend.position = "none")
plot(pca_plot)
ggsave(pca_plot, filename="pca_fissurellidae.pdf", width=8, height=5, units="in")


