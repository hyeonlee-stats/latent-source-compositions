blt.X = readRDS('basalt/blt.X.RDS')
blt.type = readRDS('basalt/blt.type.RDS')

# ==== fit models ====
blt.psap = psa(blt.X, 'p')
blt.pca = comp_pca(blt.X)
blt.apca = comp_apca(blt.X)

blt.psas = psa(blt.X, 's')
blt.psas = rotate_psa(blt.psas, 2, c(1,3,2))
blt.psas = flip_loading(blt.psas, c(1,2))

# set type colors and shapes
blt.color = c(1,3,6,5,7)
blt.shape = c(1,6,2,3,8)
names(blt.color) <- names(blt.shape) <- levels(blt.type)

# ==== biplots ====
psap.biplot = biplot_psa(blt.psap$scores[!is.na(blt.type),1:2], blt.psap$loadings[,1:2], label = blt.type[!is.na(blt.type)]) +
  scale_color_manual(values = blt.color) +
  scale_shape_manual(values = blt.shape) +
  ggtitle('PSA-P') +
  theme(legend.position = 'none')

psas.biplot = biplot_psa(blt.psas$scores[!is.na(blt.type),1:2], blt.psas$loadings[,1:2], label = blt.type[!is.na(blt.type)]) +
  scale_color_manual(values = blt.color) +
  scale_shape_manual(values = blt.shape) +
  ggtitle('PSA-S') +
  theme(legend.position = 'none')

pca.biplot = biplot_psa(blt.pca$scores[!is.na(blt.type),1:2], blt.pca$loadings[,1:2], label = blt.type[!is.na(blt.type)]) +
  scale_color_manual(values = blt.color) +
  scale_shape_manual(values = blt.shape) +
  ggtitle('PCA') +
  theme(legend.position = 'none')

apca.biplot = biplot_psa(blt.apca$scores[!is.na(blt.type),1:2], blt.apca$loadings[,1:2], label = blt.type[!is.na(blt.type)]) +
  scale_color_manual(values = blt.color) +
  scale_shape_manual(values = blt.shape) +
  ggtitle('Log-ratio PCA') +
  theme(legend.position = 'none') +
  geom_text(fontface = 'bold')

blt.legend.biplot = get_legend(pca.biplot +
                                 theme(legend.position = 'right',
                                       legend.title = element_text(size = 12, face = "bold"),
                                       legend.text = element_text(size = 10)) +
                                 labs(color = 'Type', shape = 'Type'))
blt.biplots = plot_grid(plot_grid(psap.biplot, pca.biplot, apca.biplot,
                                  nrow = 1),
                        blt.legend.biplot,
                        nrow = 1, rel_widths = c(3,0.5)) +
  ggtitle('(b)') +
  theme(plot.title = element_text(hjust = 0, size = 20, face = 'bold'))


# ==== ternary diagrams ====

## ==== PSAP ternary ====
psap.ternary = plot_subsimplex() +
  geom_point(data = to_2d_V(blt.psap$Xhat_reduced$`r=2`) %>%
               mutate(type = as.factor(blt.type)) %>%
               na.omit(),
             aes(x = e1, y = e2, col = type, shape = type), size = 1) +
  scale_color_manual(values = blt.color) +
  scale_shape_manual(values = blt.shape) +
  theme(legend.position = 'none') +
  coord_cartesian(xlim = c(-0.1, 1.5), ylim = c(-0.2, 1.4))

psap.ternary.zoom = psap.ternary +
  coord_cartesian(xlim = c(-0.1, 0.8), ylim = c(-0.1, 0.7)) +
  theme(aspect.ratio = 8/9) +
  ggtitle(NULL) +
  theme(plot.margin = margin(0,0,0,0))

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
  theme(plot.margin = margin(10,10,10,20),
        aspect.ratio = 1)

blt.ternary = plot_grid(psap.ternary  + annotate('text', x=0.0, y=1.3, hjust = 0, label = 'PSA-P', size = 6),
                        NULL, psap.ternary.zoom,
                        psap.vertex + theme(plot.margin = margin(5,5,25,15)),
                        nrow = 1, rel_widths = c(1,0.01,1,1)) +
  ggtitle('(a)') +
  theme(plot.title = element_text(hjust = 0, size = 20, face = 'bold'))

# ==== Figure 7 ====

plot_grid(blt.ternary, blt.biplots, ncol = 1)
ggsave('basalt/figures/basalt.pdf', width = 12, height = 8)

# ==== RSS ====
## ==== representation using top major elements ====
blt.major = list()
blt.major$Xhat <- lapply(0:11, function(r){
  cols = 1:(r+1)
  mat = matrix(0, nrow(blt.X), ncol(blt.X))
  mat[,setdiff(1:12,cols)] = rep(colMeans(blt.X[,setdiff(1:12,cols),drop=F]), each = nrow(blt.X))
  mat[,cols] = to_simplex(blt.X[,cols,drop=F]) * sum(colMeans(blt.X[,cols,drop=F]))
  return(mat)
})
names(blt.major$Xhat) = paste0('r=',0:11)

## ==== compute RSS ====
get_rss <- function(X, Xhat){
  rss = sapply(Xhat, function(mat) mean(rowSums((X-mat)^2)))
  names(rss) = names(Xhat)
  return(rss)
}

rss.df = cbind(get_rss(blt.X, blt.pca$Xhat),
               get_rss(blt.X, blt.apca$Xhat),
               get_rss(blt.X, blt.psap$Xhat),
               get_rss(blt.X, blt.psas$Xhat)) %>%
  as.data.frame() %>%
  setNames(c('PCA','logratio','PSAP','PSAS'))
round(rss.df*1000, 2)


cols = c(1,2,5)
mat = matrix(0, nrow(blt.X), ncol(blt.X))
mat[,setdiff(1:12,cols)] = rep(colMeans(blt.X[,setdiff(1:12,cols),drop=F]), each = nrow(blt.X))
mat[,cols] = to_simplex(blt.X[,cols,drop=F]) * sum(colMeans(blt.X[,cols,drop=F]))
round(get_rss(blt.X, list(major=mat))*1000, 3)
