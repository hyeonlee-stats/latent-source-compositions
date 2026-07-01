source('PSAP_setup.R')

## ==== preprocessing ====
## blt.source contains source lithology of experiments.
## blt.melt contains the chemical compositions of basaltic melts of the above
##     source lithologies under various environmental factors.
## blt.source$type: type from actual source lithology
## blt.melt$type.inferred: type inferred based on log-ratio

blt.source = read.csv('basalt/yang 2019 basalt source labels.csv', strip.white = T)
blt.melt = read.csv('basalt/yang 2019 basalt melt labels.csv', strip.white = T)

## map lithology to source type based on Yang et al. (2019) paper
source2type = data.frame(Lithology = unique(blt.source$Lithology),
                         type = c('Peridotite','Harzburgite','Peridotite','Peridotite',
                                  'Mafic','Mafic','Mafic','Mafic','Mafic','Mafic',
                                  'Mafic','Mafic','Mafic','Mafic','Mafic'))
blt.source = blt.source %>%
  left_join(source2type, by = 'Lithology') %>%
  mutate(carbonated = sapply(Volatiles, function(x){ifelse('C' %in% strsplit(x, ', ')[[1]], 'Carbonated ','')}),
         type = factor(paste0(carbonated, type),
                       levels = c('Mafic','Peridotite','Harzburgite',
                                  'Carbonated Mafic','Carbonated Peridotite'))) %>%
  select(References, Source.composition, type) %>%
  unique()

## keep record for inferred types for checking mapping reliability
blt.melt = blt.melt %>%
  dplyr::rename(type.inferred = Type) %>%
  mutate(type.inferred = factor(type.inferred,
                                levels = c('mafic','transitional','peridotite','harburgite',
                                           'carbonated mafic','carbonated transitional','carbonated peridotite')))

## map source type to melt compositions
blt.melt = left_join(blt.melt, blt.source,
                     by = c('References','Source.composition'), relationship = 'many-to-many')

## check how many melts are not mapped to source
setdiff(blt.melt$Source.composition, blt.source$Source.composition)
setdiff(blt.melt$References, blt.source$References)

## check if mapping was reasonable
table(blt.melt$type.inferred, blt.melt$type)

## remove missing/outliers
blt.melt$Cr2O3[is.na(blt.melt$Cr2O3)] = 0
blt.melt = blt.melt[blt.melt$P2O5 < 10,]

## extact melt composition as matrix
## sort variables in decreasing mean proportion
oxide.mass = c(60.08, 79.87, 101.96, 151.99, 71.84, 70.94, 40.30, 56.08, 61.98, 94.20, 74.69, 141.94)
#               SiO2   TiO2   Al2O3   Cr2O3   FeO    MnO    MgO    CaO    Na2O   K2O    NiO    P2O5
cation.mass = c(28.09, 47.87, 53.96, 103.99, 55.85, 54.94, 24.31, 40.08, 45.98, 78.20, 58.69, 61.95)
#               Si     Ti     Al2     Cr2     Fe     Mn     Mg     Ca     Na2    K2     Ni     P2
varnames = setNames(c("Si",  "Ti",  "Al",   "Cr",   "Fe", "Mn", "Mg", "Ca", "Na",  "K",  "Ni", "P"),
                    c("SiO2","TiO2","Al2O3","Cr2O3","FeO","MnO","MgO","CaO","Na2O","K2O","NiO","P2O5"))

blt.X = as.matrix(blt.melt[,8:19]) %>%
  sweep(2, cation.mass/oxide.mass, '*') %>%
  sweep(1, rowSums(.), '/')
colnames(blt.X) = varnames[colnames(blt.X)]

var_order = blt.X %>%
  colMeans() %>%
  sort(decreasing = T) %>%
  names()
blt.X = blt.X[,var_order]

## ==== Analysis ====
blt.psap = psa(blt.X, 'g')
blt.pca = comp_pca(blt.X)
blt.apca = comp_apca(blt.X)
blt.psas = psa(blt.X, 's')
blt.psas = rotate_psa(blt.psas, 2, c(1,3,2))

blt.color = c(1,3,6,5,7)
blt.shape = c(1,6,2,3,8)
names(blt.color) <- names(blt.shape) <- levels(blt.melt$type)

## ==== biplots ====
### ==== PSAP biplot ====

psap.score = as.data.frame(blt.psap$scores) %>%
  mutate(type = blt.melt$type) %>%
  na.omit()
psap.loading = as.data.frame(blt.psap$loadings[,1:2]) %>%
  mutate(oxide = rownames(blt.psap$loadings))

psap.biplot = ggplot(psap.score) +
  geom_hline(yintercept = 0, color = 'darkgray') +
  geom_vline(xintercept = 0, color = 'darkgray') +
  geom_point(aes(x = PC1, y = PC2, col = type, shape = type), size = 1) +
  scale_color_manual(values = blt.color) +
  scale_shape_manual(values = blt.shape) +
  coord_cartesian(xlim = c(-0.2,0.35), ylim = c(-0.2,0.35)) +
  theme(aspect.ratio = 1) +
  geom_text(data = psap.loading,
            aes(x = PC1/2, y = PC2/2, label = oxide), col='red') +
  ggtitle('PSA-P') +
  theme(legend.position = 'none')

### ==== PCA biplot ====

pca.score = as.data.frame(blt.pca$scores) %>%
  mutate(type = blt.melt$type) %>%
  na.omit()
pca.loading = as.data.frame(blt.pca$loadings[,1:2]) %>%
  mutate(oxide = rownames(blt.pca$loadings))

pca.biplot = ggplot(pca.score) +
  geom_hline(yintercept = 0, color = 'darkgray') +
  geom_vline(xintercept = 0, color = 'darkgray') +
  geom_point(aes(x = PC1, y = PC2, col = type, shape = type), size = 1) +
  scale_color_manual(values = blt.color) +
  scale_shape_manual(values = blt.shape) +
  coord_cartesian(xlim = c(-0.25,0.4), ylim = c(-0.25,0.4)) +
  theme(aspect.ratio = 1) +
  geom_text(data = pca.loading,
            aes(x = PC1/2.5, y = PC2/2.5, label = oxide), col='red') +
  ggtitle('PCA') +
  theme(legend.position = 'none')

### ==== LogRatio PCA biplot ====

apca.score = as.data.frame(blt.apca$scores) %>%
  mutate(type = blt.melt$type) %>%
  na.omit()
apca.loading = as.data.frame(blt.apca$loadings[,1:2]) %>%
  mutate(oxide = rownames(blt.apca$loadings))

apca.biplot = ggplot(apca.score) +
  geom_hline(yintercept = 0, color = 'darkgray') +
  geom_vline(xintercept = 0, color = 'darkgray') +
  geom_point(aes(x = PC1, y = PC2, col = type, shape = type), size = 1) +
  scale_color_manual(values = blt.color) +
  scale_shape_manual(values = blt.shape) +
  coord_cartesian(xlim = c(-5,7.5), ylim = c(-5,7.5)) +
  theme(aspect.ratio = 1) +
  geom_text(data = apca.loading,
            aes(x = PC1*7, y = PC2*7, label = oxide), col='red') +
  ggtitle('Log-ratio PCA') +
  theme(legend.position = 'none')

### ==== combined biplots ====
blt.legend = get_legend(pca.biplot +
                          theme(legend.position = 'right') +
                          labs(color = 'Type', shape = 'Type'))
plot_grid(psap.biplot, pca.biplot, apca.biplot, blt.legend,
          nrow = 1, rel_widths = c(1,1,1,0.5))
ggsave('basalt/figures/biplots.jpeg', width = 5*3.5, height = 5)

## ==== ternary diagrams ====
### ==== PSAP ternary ====
psap.ternary = plot_subsimplex() +
  geom_point(data = to_2d_V(blt.psap$Xhat_reduced$`r=2`) %>%
               mutate(type = as.factor(blt.melt$type)) %>%
               na.omit(),
             aes(x = e1, y = e2, col = type, shape = type), size = 1) +
  scale_color_manual(values = blt.color) +
  scale_shape_manual(values = blt.shape) +
  theme(legend.position = 'none') +
  coord_cartesian(xlim = c(-0.1, 1.5), ylim = c(-0.1, 1.5))

psap.ternary.zoom = psap.ternary +
  coord_cartesian(xlim = c(-0.05, 0.85), ylim = c(-0.07, 0.73)) +
  theme(aspect.ratio = 8/9) +
  ggtitle(NULL)

psap.vertex = plot_grid(
  plot_bar(blt.psap$Vhat$`r=2`[,1], sort_vertices = F) +
    ggtitle('V1') +
    scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1)),
  plot_bar(blt.psap$Vhat$`r=2`[,2], sort_vertices = F) +
    ggtitle('V2') +
    scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1)),
  plot_bar(blt.psap$Vhat$`r=2`[,3], sort_vertices = F) +
    ggtitle('V3') +
    scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1)),
  nrow = 1) +
  theme(plot.margin = margin(3,3,3,3),
        aspect.ratio = 1)


plot_grid(psap.ternary + annotate('text', x=0, y=1.4, hjust = 0, label = 'PSA-P', size = 5),
          plot_grid(NULL, psap.ternary.zoom, rel_heights = c(1,8), ncol = 1),
          psap.vertex,
          nrow = 1, rel_widths = c(1,1,1))
ggsave('basalt/Figures/psap_ternary.jpeg', width = 12, height = 4)

## ==== RSS ====

get_rss <- function(X, X.fit){
  rss = sapply(X.fit$Xhat, function(mat) mean(rowSums((X-mat)^2)))
  names(rss) = names(X.fit$Xhat)
  return(rss)
}

(get_rss(blt.X, blt.pca)*1000) %>% round(3)
(get_rss(blt.X, blt.apca)*1000) %>% round(3)
(get_rss(blt.X, blt.psap)*1000) %>% round(3)
(get_rss(blt.X, blt.psas)*1000) %>% round(3)
plot_bars(blt.psas$Vhat$`r=2`)
blt.major = list()
blt.major$Xhat <- lapply(0:11, function(r){
  cols = 1:(r+1)
  mat = matrix(0, nrow(blt.X), ncol(blt.X))
  mat[,setdiff(1:12,cols)] = rep(colMeans(blt.X[,setdiff(1:12,cols),drop=F]), each = nrow(blt.X))
  mat[,cols] = to_simplex(blt.X[,cols,drop=F]) * sum(colMeans(blt.X[,cols,drop=F]))
  return(mat)
})
names(blt.major$Xhat) = paste0('r=',0:11)

cols = c(1,3,5)
mat = matrix(0, nrow(blt.X), ncol(blt.X))
mat[,setdiff(1:12,cols)] = rep(colMeans(blt.X[,setdiff(1:12,cols),drop=F]), each = nrow(blt.X))
mat[,cols] = to_simplex(blt.X[,cols,drop=F]) * sum(colMeans(blt.X[,cols,drop=F]))
round(get_rss(blt.X, list(Xhat = list(major=mat)))*1000, 3)



plot_subsimplex() +
  geom_point(data = to_2d_V(to_simplex(blt.X[,1:3])) %>%
               mutate(type = blt.melt$type),
             aes(col = type, shape = type)) +
  scale_color_manual(values = blt.color) +
  scale_shape_manual(values = blt.shape)

plot_subsimplex() +
  geom_point(data = to_2d_V(blt.psap$Xhat_reduced$`r=2`) %>%
               mutate(type = blt.melt$type) %>%
               na.omit(),
             aes(col = type, shape = type)) +
  scale_color_manual(values = blt.color) +
  scale_shape_manual(values = blt.shape) +
  coord_cartesian(xlim = c(-0.1, 0.8), ylim = c(-0.1, 0.7))
