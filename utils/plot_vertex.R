#' @title Single Bar Plot for PSA Loadings or Vertices
#' @description Creating a bar plot summarizing a loading vector or a vertex vector
#' @import ggplot2
#' @param v a loading vector
#' @param sort_vertices logical; if True, all variables are presented in order
#' @param max.k maximum number of elements to display
#' @return a ggplot object
plot_bar <- function(v, sort_vertices = T, max.k = 12, xlim = NULL, grayscale = NULL){

  if(is.null(grayscale)){
    grayscale = (min(v) >= -1e-8)
  }

  if(sort_vertices){
    v = v[abs(v)>1e-10]
    if(length(v) > max.k){
      threshold = sort(abs(v), decreasing = T)[max.k]
      v = v[abs(v) >= threshold]
    }
    v = sort(v, decreasing = T)
  }
  v = rev(v)
  df = data.frame(variable = factor(names(v), levels = names(v)),
                  value = v,
                  col = factor(v > 0, levels = c(T,F)))
  rownames(df) = NULL

  g = ggplot(data = df, aes(y = .data$variable, x = .data$value, fill = .data$col)) +
    geom_bar(stat = 'identity') +
    theme_bw() +
    theme(axis.ticks = element_blank()) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = 'none') +
    theme(plot.title = element_text(hjust = 0.5))

  if(grayscale){
    g = g + scale_fill_manual(values = c('darkgray','darkgray'))
  }
  if(!is.null(xlim)){
    g = g + scale_x_continuous(limits = xlim)
  }

  return(g)
}

#' @title Plot Vertices
#' @description Creating a bar plot summarizing vertices of PSA
#' @import ggplot2
#' @param V matrix of vertices
#' @param sort_vertices logical; if True, all variables are presented in order
#' @param max.k maximum number of elements to display
#' @param title title of plot
#' @return a ggplot object
plot_vertex <- function(V, sort_vertices = T, max.k = 12, xlim = NULL, title = NULL){

  if(is.null(rownames(V))){
    rownames(V) = paste0('e',1:nrow(V))
  }
  ls = list()
  for(i in 1:ncol(V)){
    ls[[i]] = plot_bar(V[,i], sort_vertices = sort_vertices,
                       max.k = max.k, xlim = xlim, grayscale = T) + ggtitle(colnames(V)[i])
  }
  g = cowplot::plot_grid(plotlist = ls, nrow = 1, align = 'v', axis = 'l')
  if(!is.null(title)){
    g = plot_grid(ggdraw() + draw_label(title), g,
                  ncol = 1, rel_heights = c(1,10))
  }
  return(g)
}

#' @title Plot Loading Vectors
#' @description Creating a bar plot summarizing loading vectors of PSA
#' @import ggplot2
#' @inheritParams plot_vertex
#' @return a ggplot object
plot_loadings <- function(V, sort_vertices = T, max.k = 12, xlim = NULL, title = NULL){

  if(is.null(rownames(V))){
    rownames(V) = paste0('e',1:nrow(V))
  }
  ls = list()
  for(i in 1:ncol(V)){
    ls[[i]] = plot_bar(V[,i], sort_vertices = sort_vertices,
                       max.k = max.k, xlim = xlim, grayscale = F) + ggtitle(colnames(V)[i])
  }
  g = cowplot::plot_grid(plotlist = ls, nrow = 1, align = 'v', axis = 'l')
  if(!is.null(title)){
    g = plot_grid(ggdraw() + draw_label(title), g,
                  ncol = 1, rel_heights = c(1,10))
  }
  return(g)
}

