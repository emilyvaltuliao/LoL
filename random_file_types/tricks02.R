#=== tricks02.in.R by pfs 02/2023
# upset and treemap plots

library(data.table)
library(tidyverse)
setwd("/Users/pfsulliv2/Documents/_routines/Rgenetics/99storage/R-tricks")

#=== read geneMatrix
# one row per ensgid (Ensembl gene ID)
# 111 information columns per ensgid
a <- paste0(GMLOC, "/geneMatrix.tsv")
b <- fread(a)


#=== upset plots (not Venn diagram)
# ComplexUpset :: https://krassowski.github.io/complex-upset/articles/Examples_R.html 
library(ComplexUpset)
c <- b %>% 
  filter(asd2021.wes.fdr05==T | dd2021.wes.fdr05==T | scz2022.wes.qvalSig05==T)
upset(data = c, 
      intersect = c("asd2021.wes.fdr05", "dd2021.wes.fdr05", "scz2022.wes.qvalSig05"), 
      min_size = 1,
      name="WES comparison") + 
  labs(title = "", caption = "no more Venn diagrams!")
ggsave("tricks2-upset.png", height = 8, width = 8, dpi = 300)
ggsave("tricks2-upset.pdf")


#=== treemap (not pie chart)
d <- b %>% 
  group_by(gene_type) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  arrange(desc(n))
e <- d %>% 
  filter(n > 1000)

library(treemapify)
ggplot(e, aes(area = n, fill = gene_type, label = paste(gene_type, n, sep = "\n"))) +
  geom_treemap() +
  geom_treemap_text(colour = "white", place = "centre", size = 15) +
  theme(legend.position = "none", aspect.ratio=0.8)

ggsave("tricks2-treemap.png", height = 8, width = 8)

rm(list=ls(pattern="^[a-z]"))   # remove all objects starting with lowercase letter



