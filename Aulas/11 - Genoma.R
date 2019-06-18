nci <- read.table('nci.tsv')

colnames(nci) <- c(1:64)


## Criando mapa de calor

# Derrete os dados para o formato long
library(reshape2)
nci.long.samp <- melt(as.matrix(nci[1:200,]))
names(nci.long.samp) <- c('gene', 'case', 'value')
head(nci.long.samp)

# Cria o mapa de calor
ggplot(aes(y = gene, x = case, fill = value),
       data = nci.long.samp) + 
  geom_tile() +
  scale_fill_gradientn(colours = colorRampPalette(c('blue', 'red'))(100))
