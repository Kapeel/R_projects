library(ggplot2)
library(cowplot)
setwd("/Users/kchougul/R_project/Violin_plots_OGE")
ds = read.table("RLC.txt", sep="\t",header = TRUE)
ds$Specie <- factor(ds$Specie, levels = c('O. sativa vg. japonica', 'O. rufipogon', 'O. sativa vg. indica [93-11]', 'O. nivara', 'O. glaberrima', 'O. barthii', 'O. glumaepatula', 'O. meridionalis', 'O. punctata', 'O. brachyantha', 'L. perrieri'))

#ds$Specie <- factor(ds$Specie, levels = c('O. sativa vg. japonica', 'O. rufipogon', 'O. sativa vg. indica [93-11]', 'O. nivara', 'O. glaberrima', 'O. barthii', 'O. glumaepatula', 'O. meridionalis', 'O. punctata', 'O. brachyantha', 'L. perrieri'))
mylabels <- c(expression(paste("O. ",italic("sativa vg. japonica"))),
              expression(paste("O. ",italic("rufipogon"))),
              'O. sativa vg. indica [93-11]',
              expression(paste("O. ",italic("nivara"))),
              expression(paste("O. ",italic("glaberrima"))),
              expression(paste("O. ",italic("barthii"))),
              expression(paste("O. ",italic("glumaepatula"))),
              expression(paste("O. ",italic("meridionalis"))),
              expression(paste("O. ",italic("punctata"))),
              expression(paste("O. ",italic("brachyantha"))),
              expression(paste("L. ",italic("perrieri")))
) 
ds$Specie <- factor(ds$Specie, levels = mylabels)

# save plots
savePlot <- function(myPlot) {
  pdf("copia.pdf")
  print(myPlot)
  dev.off()
}

# Create a Violin plots
myplot <- ggplot(ds, aes(x = Specie, y =Insertion_age, fill="red")) + 
  geom_violin() +
  geom_boxplot(width=0.2) +
  ylab('Species') +
  ylab('Insertion age (My)') + 
  labs(title = "Copia") +
  theme(text = element_text(size=12)) +
  theme(axis.text.x=element_text(size=rel(0.8), angle=90)) +
  theme(legend.position="none") +
 scale_fill_manual(values=c("#FF3366"))+
#  scale_colour_manual(values=c("#FF3366"))
# scale_x_discrete(labels = mylabels)
  scale_x_discrete("Species", labels = expression(paste("O. ",italic("sativa vg. japonica")),
                                                  paste("O. ",italic("rufipogon")),
                                                  'O. sativa vg. indica [93-11]',
                                                  paste("O. ",italic("nivara")),
                                                  paste("O. ",italic("glaberrima")),
                                                  paste("O. ",italic("barthii")),
                                                  paste("O. ",italic("glumaepatula")),
                                                  paste("O. ",italic("meridionalis")),
                                                  paste("O. ",italic("punctata")),
                                                  paste("O. ",italic("brachyantha")),
                                                  paste("L. ",italic("perrieri"))))
  # #99FF99 -> Gypsy
  # FF3366 -> Copia
  # 99CCFF > unknown
savePlot(myPlot)
