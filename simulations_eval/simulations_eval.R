
# ==== Run ====
## Be cautious that these may take hours
# res_10_2 = test_set(d = 10, k = 2, n = 100, threshold = 1e-6, r = 10, seeds = 0 + 1:100, fast = F)
# saveRDS(res_10_2, 'simulations_eval/results/res_10_2.rds')
# res_10_5 = test_set(d = 10, k = 5, n = 100, threshold = 1e-6, r = 10, seeds = 100 + 1:100, fast = F)
# saveRDS(res_10_5, 'simulations_eval/results/res_10_5.rds')
# res_20_2 = test_set(d = 20, k = 2, n = 100, threshold = 1e-6, r = 10, seeds = 200 + 1:100, fast = F)
# saveRDS(res_20_2, 'simulations_eval/results/res_20_2.rds')
# res_20_5 = test_set(d = 20, k = 5, n = 100, threshold = 1e-6, r = 10, seeds = 300 + 1:100, fast = F)
# saveRDS(res_20_5, 'simulations_eval/results/res_20_5.rds')
# res_40_2 = test_set(d = 40, k = 2, n = 100, threshold = 1e-6, r = 20, seeds = 400 + 1:100, fast = T)
# saveRDS(res_40_2, 'simulations_eval/results/res_40_2.rds')
# res_40_5 = test_set(d = 40, k = 5, n = 100, threshold = 1e-6, r = 20, seeds = 500 + 1:100, fast = T)
# saveRDS(res_40_5, 'simulations_eval/results/res_40_5.rds')

# pmean = c(2, 2, 1, 1, 1, rep(0.5, 5), rep(0.05, 31))
# res_40_2_sparse = test_set(d = 40, k = 2, n = 100, pmean = pmean,
#                            threshold = 1e-6, r = 20, seeds = 600 + 1:100, fast = T)
# saveRDS(res_40_2_sparse, 'simulations_eval/results/res_40_2_sparse.rds')
# res_40_5_sparse = test_set(d = 40, k = 5, n = 100, pmean = pmean,
#                            threshold = 1e-6, r = 20, seeds = 700 + 1:100, fast = T)
# saveRDS(res_40_5_sparse, 'simulations_eval/results/res_40_5_sparse.rds')


# ==== plot ====
g.10.2 = plot_sim(readRDS('simulations_eval/results/res_10_2.rds'), fast = F)
g.10.5 = plot_sim(readRDS('simulations_eval/results/res_10_5.rds'), fast = F)
g.20.2 = plot_sim(readRDS('simulations_eval/results/res_20_2.rds'), fast = F)
g.20.5 = plot_sim(readRDS('simulations_eval/results/res_20_5.rds'), fast = F)
g.40.2 = plot_sim(readRDS('simulations_eval/results/res_40_2.rds'))
g.40.5 = plot_sim(readRDS('simulations_eval/results/res_40_5.rds'))
g.40.2.sparse = plot_sim(readRDS('simulations_eval/results/res_40_2_sparse.rds'))
g.40.5.sparse = plot_sim(readRDS('simulations_eval/results/res_40_5_sparse.rds'))
g = plot_grid(g.10.2, g.10.5,
              g.20.2, g.20.5,
              g.40.2, g.40.5,
              g.40.2.sparse, g.40.5.sparse,
              ncol = 2, align = 'h', axis = 'tblr')
g.legend = data.frame(y = 1:4,
                      Method = factor(c('PCA','PSA-P','fast PSA-P','PSA-S'),
                                      levels = c('PCA','PSA-P','fast PSA-P','PSA-S'))) %>%
  ggplot(aes(y = y, col = Method)) +
  geom_boxplot() +
  theme(legend.position = 'bottom')

plot_grid(NULL,
          plot_grid(ggdraw() + draw_label('k=2') + theme(plot.background = element_rect(fill = 'gray90', color = 'white')),
                    ggdraw() + draw_label('k=5') + theme(plot.background = element_rect(fill = 'gray90', color = 'white')),
                    nrow = 1),
          plot_grid(ggdraw() + draw_label('d=10') + theme(plot.background = element_rect(fill = 'gray90', color = 'white')),
                    ggdraw() + draw_label('d=20') + theme(plot.background = element_rect(fill = 'gray90', color = 'white')),
                    ggdraw() + draw_label('d=40') + theme(plot.background = element_rect(fill = 'gray90', color = 'white')),
                    ggdraw() + draw_label('d=40\nUnbalanced') + theme(plot.background = element_rect(fill = 'gray90', color = 'white')),
                    ncol = 1),
          g,
          NULL, get_legend(g.legend),
          nrow = 3, rel_widths = c(1, 10), rel_heights = c(1, 20, 1))
ggsave('simulations_eval/figures/simulation_ve.pdf', width = 12, height = 9)



g.10.2 = plot_sim(readRDS('simulations_eval/results/res_10_2.rds'), fast = F, ve = F)
g.10.5 = plot_sim(readRDS('simulations_eval/results/res_10_5.rds'), fast = F, ve = F)
g.20.2 = plot_sim(readRDS('simulations_eval/results/res_20_2.rds'), fast = F, ve = F)
g.20.5 = plot_sim(readRDS('simulations_eval/results/res_20_5.rds'), fast = F, ve = F)
g.40.2 = plot_sim(readRDS('simulations_eval/results/res_40_2.rds'), ve = F)
g.40.5 = plot_sim(readRDS('simulations_eval/results/res_40_5.rds'), ve = F)
g.40.2.sparse = plot_sim(readRDS('simulations_eval/results/res_40_2_sparse.rds'), ve = F)
g.40.5.sparse = plot_sim(readRDS('simulations_eval/results/res_40_5_sparse.rds'), ve = F)
g = plot_grid(g.10.2, g.10.5,
              g.20.2, g.20.5,
              g.40.2, g.40.5,
              g.40.2.sparse, g.40.5.sparse,
              ncol = 2, align = 'h', axis = 'tblr')
g.legend = data.frame(y = 1:4,
                      Method = factor(c('PCA','PSA-P','fast PSA-P','PSA-S'),
                                      levels = c('PCA','PSA-P','fast PSA-P','PSA-S'))) %>%
  ggplot(aes(y = y, col = Method)) +
  geom_boxplot() +
  theme(legend.position = 'bottom')

plot_grid(NULL,
          plot_grid(ggdraw() + draw_label('k=2') + theme(plot.background = element_rect(fill = 'gray90', color = 'white')),
                    ggdraw() + draw_label('k=5') + theme(plot.background = element_rect(fill = 'gray90', color = 'white')),
                    nrow = 1),
          plot_grid(ggdraw() + draw_label('d=10') + theme(plot.background = element_rect(fill = 'gray90', color = 'white')),
                    ggdraw() + draw_label('d=20') + theme(plot.background = element_rect(fill = 'gray90', color = 'white')),
                    ggdraw() + draw_label('d=40') + theme(plot.background = element_rect(fill = 'gray90', color = 'white')),
                    ggdraw() + draw_label('d=40\nUnbalanced') + theme(plot.background = element_rect(fill = 'gray90', color = 'white')),
                    ncol = 1),
          g,
          NULL, get_legend(g.legend),
          nrow = 3, rel_widths = c(1, 10), rel_heights = c(1, 20, 1))
ggsave('simulations_eval/figures/simulation_rss.jpeg', width = 12, height = 9)
