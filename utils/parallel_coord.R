parallel_coord <- function(df, columns = 1:ncol(df)){

  df = as.data.frame(df)
  n = nrow(df)
  p = length(columns)
  var.names = colnames(df)[1:p]
  if(is.null(var.names)){
    var.names = paste0('V',1:p)
  }

  modes = df[columns]
  features = df[-columns]

  modes.df = as.data.frame(matrix(0, nrow = n, ncol = 2*p))
  modes.df[,(1:p)*2-1] = modes
  modes.df[,(1:p)*2] = modes
  colnames(modes.df) = paste0('parallel',1:(2*p))
  modes.df = cbind(modes.df,
                   features,
                   data.frame(id_parallel = 1:n))

  df.melt = modes.df %>%
    melt(measure.vars = 1:(2*p)) %>%
    mutate(variable = as.numeric(variable))

  g = ggplot(df.melt, aes(x = variable, y = value, group = id_parallel)) +
    geom_line() +
    theme_bw() +
    ylab(NULL) +
    scale_x_continuous(name = NULL, breaks = 2*(1:p)-0.5,
                       labels = var.names,
                       minor_breaks = 1:(ncol(modes)*2)) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_line(color = 'gray'))

  return(g)
}

