# ==== read data ====
fp <- read.csv('feldspar/feldspar.csv')

get_group <- function(x){
  k <- 39.102; na <- 22.9898; ca <- 40.08;
  weights = c(x[1]/k, x[2]/na, x[3]/ca)
  weights = weights/sum(weights)
  or = weights[1]; ab = weights[2]; an = weights[3]

  if(or <= 0.1 & an <= 0.1){ 'Albite'
  }else if(or/an >= 0.15/0.2){
    if(or/ab <= 0.37/0.63){'Anorthoclase'
    }else{'Sanidine'}
  }else{
    if(an/ab > 0.9/0.1){'Anorthite'
    }else if(an/ab > 0.7/0.3){'Bytownite'
    }else if(an/ab > 0.5/0.5){'Labradorite'
    }else if(an/ab > 0.3/0.7){'Andesine'
    }else{'Oligoclase'}
  }
}

mineral.group = factor(apply(fp[,c('K','Na','Ca')], 1, get_group),
                       levels = c('Sanidine','Anorthoclase','Albite','Oligoclase',
                                  'Andesine','Labradorite','Bytownite','Anorthite'))
fp.color = 1:8
fp.shape = 1:8


# ==== PSA-P ====
fp.psap = psa(fp, 'p')
fp.psap = rotate_psa(fp.psap, 2, c(3, 1, 2))

## ==== feldspar system ====
g.feldspar_system <- readRDS('feldspar/feldspar_system.RDS')

## ==== PSA-P ternary ====
g.psap_ternary = as.data.frame(fp.psap$Xhat_reduced$`r=2`) %>%
  mutate(mineral.group = mineral.group) %>%
  ggtern(aes(x=V1, y = V3, z = V2, col = mineral.group, shape = mineral.group)) +
  geom_point() +
  scale_shape_manual(values = fp.color, name = 'Mineral group') +
  scale_color_manual(values = fp.shape, name = 'Mineral group') +
  ggtitle('PSA-P') +
  theme(plot.title = element_text(hjust = 0.5))

## ==== vertex ====
al<-26.9815; na<-22.9898; mg<-24.312; si<-28.086;
k<-39.102; ca<-40.08; ti<-47.9; mn<-54.938; fe<-55.847;

vertex_true = as.data.frame(cbind(c(si*3,ti*0,al*1,fe*0,ca*0,mg*0,mn*0,k*1,na*0),
                                  c(si*3,ti*0,al*1,fe*0,ca*0,mg*0,mn*0,k*0,na*1),
                                  c(si*2,ti*0,al*2,fe*0,ca*1,mg*0,mn*0,k*0,na*0)) %>%
                              sweep(2, colSums(.), '/'))
colnames(vertex_true) = c('Orthoclase','Alibte','Anorthite')
rownames(vertex_true) = colnames(fp)

df_vertex = as.data.frame(rbind(t(vertex_true),
                                t(fp.psap$Vhat$`r=2`))) %>%
  mutate(vertex = c('V3','V1','V2','V1','V2','V3'),
         method = c(rep('True',3),rep('PSA-P',3))) %>%
  melt(id.vars = c('vertex','method')) %>%
  mutate(method = factor(method, levels = c('True','PSA-P')))

plot_compare_vertex <- function(df){
  df %>%
    ggplot(aes(y = variable, x = value, fill = method)) +
    geom_col(stat = 'identity', position = 'dodge', width = 0.6) +
    scale_y_discrete(limits = rev) +
    guides(fill = guide_legend(reverse = TRUE)) +
    scale_fill_manual(values = c('gray','black')) +
    scale_x_continuous(limits = c(0,1), breaks = c(0,0.5,1)) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = 'none',
          plot.title = element_text(size = 10))
}

g.psap_vertex = plot_grid(plot_compare_vertex(df_vertex %>% filter(vertex == 'V3')) +
                            ggtitle('V3\n(Orthoclase)'),
                          plot_compare_vertex(df_vertex %>% filter(vertex == 'V1')) +
                            ggtitle('V1\n(Albite)'),
                          plot_compare_vertex(df_vertex %>% filter(vertex == 'V2')) +
                            ggtitle('V2\n(Anorthite)'),
                          get_legend(plot_compare_vertex(df_vertex %>% filter(vertex == 'V3')) +
                                       labs(fill = '') +
                                       theme(legend.position = 'right') +
                                       theme(plot.background = element_rect(fill = 'white'))),
                          nrow = 1, rel_widths = c(2,2,2,2)) +
  theme(plot.margin = margin(t = 20, r = 10, b = 20, l = 10, unit = "pt"))


# ==== Figure 5 ====
plot_grid(ggtern::ggplotGrob(
  g.feldspar_system +
    ggtitle('(a)') +
    theme(plot.title = element_text(hjust = 0, vjust = 0, size = 20, face = 'bold'))),
  plot_grid(ggtern::ggplotGrob(g.psap_ternary), g.psap_vertex,
            nrow = 1, rel_widths = c(1.5,1)) +
    ggtitle('(b)') +
    theme(plot.title = element_text(hjust = 0, vjust = 0, face = 'bold', size = 20)),
  ncol = 1)
ggsave('feldspar/figures/feldspar_ternery.pdf', width = 12, height = 10)


# ==== biplots ====
## ==== PCA ====
fp.pca = comp_pca(fp)
pca.biplot = biplot_psa(fp.pca$scores[,1:2],
                        fp.pca$loadings[,1:2],
                        label = rocktype) +
  scale_color_manual(name = 'Mineral group', values = 1:8) +
  scale_shape_manual(name = 'Mineral group', values = 1:8) +
  ggtitle('PCA') +
  coord_cartesian(xlim = range(fp.pca$scores[,1])*1.3, ylim = range(fp.pca$scores[,2]*1.3), expand = FALSE)

## ==== PSA-P ====
fp.psap = flip_loading(fp.psap, c(1,2))
psap.biplot = biplot_psa(fp.psap$scores[,1:2],
                         fp.psap$loadings[,1:2],
                         label = rocktype) +
  scale_color_manual(name = 'Mineral group', values = fp.color) +
  scale_shape_manual(name = 'Mineral group', values = fp.shape) +
  ggtitle('PSA-P')

## ==== Figure 6 ====
plot_grid(pca.biplot + theme(legend.position = 'none'),
          psap.biplot + theme(legend.position = 'none'),
          get_legend(pca.biplot + theme(plot.background = element_rect(fill = 'white'))),
          nrow = 1, rel_widths = c(2,2,1))
ggsave('feldspar/figures/feldspar_biplot.pdf', width = 12.5, height = 5)
