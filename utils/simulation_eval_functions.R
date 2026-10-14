get_rss <- function(X, X.fit){
  rss = sapply(X.fit$Xhat, function(mat) mean(rowSums((X-mat)^2)))
  names(rss) = names(X.fit$Xhat)
  return(rss)
}

generate_data <- function(d, k, n = 100, pmean = NULL, threshold = 1e-6){
  ## If pmean is supplied, generate vertices w.r.t. the given mean proportions.
  ## Otherwise, pick vertices from the uniform distribution.

  if(!is.null(pmean)){
    if(length(pmean) != d+1){
      stop('length of `pmean` does not match `d`')
    }
    ## generate vertex set; regenerate if the vertex set is not affinely independent
    V = matrix(0, nrow = d+1, ncol = k+1)
    while(eigen(var(V), only.values = T)$values[k] < threshold){
      V = t(unclass(compositions::rDirichlet.rcomp(k+1, pmean)))
    }
  }else{
    ## generate vertex set; regenerate if the vertex set is not affinely independent
    V = matrix(0, nrow = d+1, ncol = k+1)
    while(eigen(var(V), only.values = T)$values[k] < threshold){
      V = t(unclass(compositions::rDirichlet.rcomp(k+1, rep(1, d+1))))
    }
  }

  ## generate mixing rate
  tt = unclass(compositions::rDirichlet.rcomp(n, rep(1, k+1)))

  ## generate gaussian error
  e = matrix(rnorm(n*(d+1), 0, 0.05/sqrt(d)), nrow = n, ncol = d+1)

  ## combine all
  X = to_simplex(tt %*% t(V) + e)
  return(X)
}



test_case <- function(d, k, n, pmean = NULL, threshold = 1e-6, r = min(20,d), seed = NULL, fast = T){

  set.seed(seed)
  X = generate_data(d = d, k = k, n = n, pmean = pmean, threshold = threshold)

  time.pca = system.time(rss.pca <- get_rss(X, comp_pca(X))[1:r])['elapsed']
  time.psas = system.time(rss.psas <- get_rss(X, psa(X, 's'))[1:r])['elapsed']
  time.psap = system.time(rss.psap <- get_rss(X, psa(X, 'p'))[1:r])['elapsed']
  if(fast){
    time.fast_psap = system.time(rss.fast_psap <- get_rss(X, fast_psap(X, n_components = min(20,d)))[1:r])['elapsed']
  }else{
    time.fast_psap = system.time(rss.fast_psap <- 0)['elapsed']
  }

  time.mat <- c(pca = time.pca, psas = time.psas, psap = time.psap, fast_psap = time.fast_psap)

  return(list(time.mat = time.mat,
              rss = rbind(rss.pca, rss.psas, rss.psap, rss.fast_psap)))
}

test_set <- function(d, k, n, pmean = NULL, threshold = 1e-6, r = min(20,d), seeds = NULL, fast = T){

  if(is.null(seeds)){
    stop('seeds must be supplied.')
  }

  res = NULL
  pb <- txtProgressBar(min = 0, max = length(seeds), style = 3)

  for(i in 1:length(seeds)){
    seed = seeds[i]
    res[[as.character(seed)]] = suppressMessages(test_case(d = d, k = k, n = n, pmean = pmean,
                                                           threshold = threshold, r = r, seed = seed, fast = fast))
    setTxtProgressBar(pb, i)
  }
  close(pb)

  return(res)
}


plot_sim <- function(res, r = 8, fast = T, ve = T){
  if(ve){
    y.lab = 'Variance explained'
    res = list(time.mat = t(sapply(res, function(ls) ls$time.mat)),
               rss.pca = t(sapply(res, function(ls) 1-ls$rss[1,]/ls$rss[1,1])),
               rss.psas = t(sapply(res, function(ls) 1-ls$rss[2,]/ls$rss[1,1])),
               rss.psap = t(sapply(res, function(ls) 1-ls$rss[3,]/ls$rss[1,1])),
               rss.fast_psap = t(sapply(res, function(ls) 1-ls$rss[4,]/ls$rss[1,1])))
  }else{
    y.lab = 'RSS'
    res = list(time.mat = t(sapply(res, function(ls) ls$time.mat)),
               rss.pca = t(sapply(res, function(ls) ls$rss[1,])),
               rss.psas = t(sapply(res, function(ls) ls$rss[2,])),
               rss.psap = t(sapply(res, function(ls) ls$rss[3,])),
               rss.fast_psap = t(sapply(res, function(ls) ls$rss[4,])))
  }

  if(fast){
    df = rbind(as.data.frame(res$rss.pca) %>%
                 mutate(method = 'PCA'),
               as.data.frame(res$rss.psas) %>%
                 mutate(method = 'PSA-S'),
               as.data.frame(res$rss.psap) %>%
                 mutate(method = 'PSA-P'),
               as.data.frame(res$rss.fast_psap) %>%
                 mutate(method = 'fast PSA-P')) %>%
      select(c(paste0('r=',1:r),'method')) %>%
      melt(id.vars = 'method') %>%
      mutate(method = factor(method, levels = c('PCA','PSA-P','fast PSA-P','PSA-S')))
    g.rss = ggplot(df, aes(x = variable, y = value, gruop = method, col = method)) +
      geom_boxplot() +
      theme(legend.position = 'none') +
      labs(x = 'Dimension', y = y.lab) +
      scale_y_continuous(limits = c(0, 1))

    g.time = as.data.frame(res$time.mat) %>%
      setNames(c('PCA','PSA-S','PSA-P','fast\nPSA-P')) %>%
      melt(id.vars = NULL) %>%
      mutate(variable = factor(variable, levels = c('PCA','PSA-P','fast\nPSA-P','PSA-S'))) %>%
      ggplot(aes(x = variable, y = value)) +
      geom_boxplot() +
      labs(x = 'Method', y = 'Time (sec)') +
      theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))

  }else{
    df = rbind(as.data.frame(res$rss.pca) %>%
                 mutate(method = 'PCA'),
               as.data.frame(res$rss.psas) %>%
                 mutate(method = 'PSA-S'),
               as.data.frame(res$rss.psap) %>%
                 mutate(method = 'PSA-P')) %>%
      select(c(paste0('r=',1:r),'method')) %>%
      melt(id.vars = 'method') %>%
      mutate(method = factor(method, levels = c('PCA','PSA-P','PSA-S')))
    g.rss = ggplot(df, aes(x=variable, y = value, gruop = method, col = method)) +
      geom_boxplot() +
      theme(legend.position = 'none') +
      labs(x = 'Dimension', y = y.lab) +
      scale_color_manual(values = gg_color_hue(4)[c(1,2,4)]) +
      scale_y_continuous(limits = c(0, 1))

    g.time = as.data.frame(res$time.mat[,1:3]) %>%
      setNames(c('PCA','PSA-S','PSA-P')) %>%
      melt(id.vars = NULL) %>%
      mutate(variable = factor(variable, levels = c('PCA','PSA-P','PSA-S'))) %>%
      ggplot(aes(x = variable, y = value)) +
      geom_boxplot() +
      labs(x = 'Method', y = 'Time (sec)') +
      theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5))
  }

  g = plot_grid(g.rss, g.time, nrow = 1, rel_widths = c(2.5,1)) +
    theme(panel.border = element_rect(fill = NA, color = 'gray90'),
          plot.margin = margin(2,2,2,2))
  return(g)

}

plot_sim <- function(res, r = 8, fast = T, ve = T){
  if(ve){
    y.lab = 'Variance explained'
    y.lim = c(0,1)
    res = list(time.mat = t(sapply(res, function(ls) ls$time.mat)),
               rss.pca = t(sapply(res, function(ls) 1-ls$rss[1,]/ls$rss[1,1])),
               rss.psas = t(sapply(res, function(ls) 1-ls$rss[2,]/ls$rss[1,1])),
               rss.psap = t(sapply(res, function(ls) 1-ls$rss[3,]/ls$rss[1,1])),
               rss.fast_psap = t(sapply(res, function(ls) 1-ls$rss[4,]/ls$rss[1,1])))
  }else{
    y.lab = 'RSS'
    y.lim = c(0, base::max(sapply(res, function(ls) ls$rss[,-1])))
    res = list(time.mat = t(sapply(res, function(ls) ls$time.mat)),
               rss.pca = t(sapply(res, function(ls) ls$rss[1,])),
               rss.psas = t(sapply(res, function(ls) ls$rss[2,])),
               rss.psap = t(sapply(res, function(ls) ls$rss[3,])),
               rss.fast_psap = t(sapply(res, function(ls) ls$rss[4,])))
  }

  df.rss = rbind(as.data.frame(res$rss.pca) %>%
                   mutate(method = 'PCA'),
                 as.data.frame(res$rss.psas) %>%
                   mutate(method = 'PSA-S'),
                 as.data.frame(res$rss.psap) %>%
                   mutate(method = 'PSA-P'),
                 as.data.frame(res$rss.fast_psap) %>%
                   mutate(method = 'fast PSA-P')) %>%
    select(c(paste0('r=',1:r),'method')) %>%
    melt(id.vars = 'method') %>%
    mutate(method = factor(method, levels = c('PCA','PSA-P','fast PSA-P','PSA-S')))

  df.time = as.data.frame(res$time.mat) %>%
    setNames(c('PCA','PSA-S','PSA-P','fast\nPSA-P')) %>%
    melt(id.vars = NULL) %>%
    mutate(method = factor(variable, levels = c('PCA','PSA-P','fast\nPSA-P','PSA-S'))) %>%
    select(-variable)

  colors = gg_color_hue(4)
  if(!fast){
    df.rss = df.rss %>% filter(method != 'fast PSA-P')
    df.time = df.time %>% filter(method != 'fast\nPSA-P')
    colors = gg_color_hue(4)[c(1,2,4)]
  }

  g.rss = ggplot(df.rss, aes(x = variable, y = value, gruop = method, col = method)) +
    geom_boxplot() +
    theme(legend.position = 'none') +
    labs(x = 'Dimension', y = y.lab) +
    scale_y_continuous(limits = y.lim) +
    scale_color_manual(values = colors)

  g.time = ggplot(df.time, aes(x = method, y = value, col = method)) +
    geom_boxplot() +
    labs(x = 'Method', y = 'Time (sec)') +
    theme(axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.5)) +
    scale_color_manual(values = colors) +
    theme(legend.position = 'none')

  g = plot_grid(g.rss, g.time, nrow = 1, rel_widths = c(2.5,1)) +
    theme(panel.border = element_rect(fill = NA, color = 'gray90'),
          plot.margin = margin(2,2,2,2))
  return(g)

}
