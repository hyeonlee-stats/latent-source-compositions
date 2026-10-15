image.path = 'simulations_toy/figures/'

## ==== Example 1 ====

### ==== data generation ====
centers = list(c(1, 0, 0),
               c(0.7, 0.05, 0.25),
               c(0, 1, 0),
               c(0.05, 0.7, 0.25))
ns = c(10, 10, 10, 10)
sigma2 = 0.04^2

set.seed(1)
X1 = rbind(make_cluster(centers[[1]], ns[1], sigma2),
           make_cluster(centers[[2]], ns[2], sigma2),
           make_cluster(centers[[3]], ns[3], sigma2),
           make_cluster(centers[[4]], ns[4], sigma2))
colnames(X1) = paste0('e',1:3)
ex1.df = as.data.frame(X1) %>%
  mutate(label = as.factor(c(rep(1,ns[1]), rep(2,ns[2]), rep(3,ns[3]), rep(4,ns[4]))))

### ==== PSA application ====

ex1.psap = psa(X1, 'p')
ex1.psas = psa(X1, 's')
ex1.pca = comp_pca(X1)
ex1.apca = comp_apca(X1)

ex1.psas = flip_loading(ex1.psas, 1:2)
ex1.pca = flip_loading(ex1.pca, 2)
ex1.apca = flip_loading(ex1.apca, 2)

### ==== ternary ====

g.data = ternary_pc(X1, ex1.df$label, type = 'data') + ggtitle('Data')

g.psap = ternary_pc(X1, ex1.df$label, type = 'psa', X.res = ex1.psap) + ggtitle('PSA-P')
g.psas = ternary_pc(X1, ex1.df$label, type = 'psa', X.res = ex1.psas) + ggtitle('PSA-S')
g.pca = ternary_pc(X1, ex1.df$label, type = 'pca', X.res = ex1.pca) + ggtitle('PCA')
g.apca = ternary_pc(X1, ex1.df$label, type = 'logratio', X.res = ex1.apca) + ggtitle('Log-ratio')
plot_grid(g.data, g.psap, g.psas, NULL, g.pca, g.apca, nrow = 2)
ggsave('ex1_ternary_pc.pdf', path = image.path, width = 12, height = 8)

### ==== scores ====

ex_plot_scores <- function(scores, label){
  g = as.data.frame(scores) %>%
    mutate(label = label) %>%
    ggplot(aes(x = PC1, y = PC2, col = label)) +
    geom_point(shape = 16) +
    scale_x_continuous(limits = range(scores)) +
    scale_y_continuous(limits = range(scores)) +
    theme(legend.position = 'none',
          plot.title = element_text(hjust = 0.5, size = 20))
  if(is.numeric(label)){
    g = g + scale_color_gradientn(colors = rainbow(5))
  }
  return(g)
}

g.psap = ex_plot_scores(ex1.psap$scores, ex1.df$label) + ggtitle('PSA-P')
g.psas = ex_plot_scores(ex1.psas$scores, ex1.df$label) + ggtitle('PSA-S')
g.pca = ex_plot_scores(ex1.pca$scores, ex1.df$label) + ggtitle('PCA')
g.apca = ex_plot_scores(ex1.apca$scores, ex1.df$label) + ggtitle('Log-ratio')

plot_grid(g.data, g.psap, g.psas, NULL, g.pca, g.apca, nrow = 2)
ggsave('ex1_scores.pdf', path = image.path, width = 9, height = 6)
