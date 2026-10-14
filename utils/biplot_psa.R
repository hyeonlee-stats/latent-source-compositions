biplot_psa <- function(scores, loadings, lambda = NULL, label = NULL,
                       boundary = F, mu = NA, threshold = 1e-6){

  scores = as.data.frame(scores)
  loadings = as.data.frame(loadings)
  x = colnames(scores)[1]
  y = colnames(scores)[2]

  if(!is.null(label)){
    scores = scores %>% mutate(label = label)
  }
  if(is.null(lambda)){
    lambda = min(min(scores[,1])/min(loadings[,1]),
                 max(scores[,1])/max(loadings[,1]),
                 min(scores[,2])/min(loadings[,2]),
                 max(scores[,2])/max(loadings[,2]))
  }

  g = ggplot() +
    geom_hline(yintercept = 0, col = 'gray') +
    geom_vline(xintercept = 0, col = 'gray') +
    geom_point(data = scores,
               mapping = aes(x = .data[[x]], y = .data[[y]], col = label, shape = label)) +
    geom_text(data = loadings*lambda,
              mapping = aes(x = .data[[x]], y = .data[[y]]),
              label = rownames(loadings),
              col = 'red', fontface = 'bold') +
    theme(aspect.ratio = 1)

  if(boundary){
    pc1_seq <- seq(min(scores[,1])*1.3, max(scores[,1])*1.3, length.out = 300)
    pc2_seq <- seq(min(scores[,2])*1.3, max(scores[,2])*1.3, length.out = 300)
    grid <- expand.grid(PC1 = pc1_seq, PC2 = pc2_seq)

    X_hat <- as.matrix(grid) %*% t(loadings[,1:2]) +
      matrix(mu, nrow = nrow(grid), ncol = length(mu), byrow = TRUE)
    grid$min_val <- apply(X_hat, 1, min)
    grid$is_inside <- factor(grid$min_val > -threshold, levels = c(T, F))

    g = g +
      geom_raster(data = grid, aes(x = PC1, y = PC2, fill = is_inside), alpha = 0.2) +
      scale_fill_manual(name = paste0('Min > ', -threshold),
                        values = c("TRUE" = "white", "FALSE" = "gray")) +
      geom_contour(data = grid, aes(x = PC1, y = PC2, z = min_val),
                   breaks = 0, col='black', linewidth = 1) +
      coord_cartesian(xlim = range(scores[,1])*1.3, ylim = range(scores[,2]*1.3), expand = FALSE)
  }

  return(g)
}
