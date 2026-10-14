clr <- function(X){
  if(is.array(X)){
    t(apply(X, 1, function(x) log(x) - mean(log(x))))
  }else{
    log(X) - mean(log(X))
  }
}

clrInv <- function(X){
  if(is.array(X)){
    t(apply(X, 1, function(x) exp(x-max(x))/sum(exp(x-max(x)))))
  }else{
    exp(X-max(X))/sum(exp(X-max(X)))
  }
}


## orthogonal projection onto the hyperplane spanned by the simplex for displaying power transform PCA
proj_simplex <- function(X){
  if(!inherits(X, 'matrix')){
    X = as.matrix(as.data.frame(X))
  }
  d = ncol(X) - 1
  nvec = t(t(rep(1, d+1)/sqrt(d+1)))
  return(X - (X %*% nvec - 1/sqrt(d+1)) %*% t(nvec))
}

#' @title Draw Ternary Diagram with Points and 1-dimensional Approximating Subset
#' @description Draw a ternary diagram with points and 1-dimensional approximating subset.
#' All data are given in R3 coordinates
#' @param X original data points in 2-simplex
#' @param X.approx 1-dimensional approximations in 2-simplex
#' @param X.pc1 1-dimensional approximating subset
#' @param X.center center point
#' @param X.colors colors of X and X.approx
#' @return a ggplot object
draw_ternary_pc <- function(X, X.approx, X.pc1, X.center, X.colors){
  ## X, X.approx, X.pc1, X.center are data frames with three columns
  g <- plot_subsimplex() +
    geom_path(data = to_2d_V(X.pc1), color = 'red', linewidth = 1) +
    geom_point(data = to_2d_V(X.center), color = 'black', size = 3) +
    geom_point(data = to_2d_V(X),
               col = X.colors, shape = 16, size = 2) +
    geom_point(data = to_2d_V(X.approx),
               col = X.colors, shape = 1, size = 2)
  return(g)
}

#' @title Draw Ternary Diagram with Principal Components
#' @description Draw a ternary diagram with points, 1-dimensional approximating subset, and projection lines.
#' @param X matrix of data points
#' @param X.label label for colors of data points. Must be discrete.
#' @param type types of dimension reduction method. Allowed types are 'data', 'psa', 'pca', 'power', 'logratio'
#' @param X.res an R object containing appropriate dimension reduction results. Not required for type = 'data'.
#' @param t.grid vector of t points for which the 1-dimensional approximating subset is to be drawn.
#' Only used for type = 'pca', 'power', 'logratio'.
#' @return a ggplot object
ternary_pc <- function(X, X.label,
                       type = c('data', 'psa', 'pca', 'power', 'logratio'),
                       X.res = NULL, t.grid = NULL){
  type = match.arg(type)
  m = 30 # resolution of paths connecting points to their approximations
  X.palette = gg_color_hue(length(unique(X.label)))
  X.colors = X.palette[X.label]

  if(type == 'data'){

    g <- plot_subsimplex() +
      geom_point(data = to_2d_V(X), shape = 16, color = X.colors, size = 2)

  }else if(type == 'pca'){

    if(is.null(t.grid)){
      t.grid = c(-0.75, 0.75)
    }

    X.pc1 = sweep(t.grid %*% t(X.res$loadings[,1]), 2, X.res$center, '+')
    X.pc1 = as.data.frame(X.pc1)
    X.center = as.data.frame(t(X.res$center))

    g <- draw_ternary_pc(X, X.res$Xhat$`r=1`, X.pc1, X.center, X.colors)

    for(i in 1:nrow(X)){
      proj.path = rbind(X[i,], X.res$Xhat$`r=1`[i,])
      g <- g + geom_path(data = to_2d_V(proj.path), color = X.colors[i])
    }

  }else if(type == 'logratio'){

    if(is.null(t.grid)){
      t.grid = c(-exp(seq(5,-5,length.out = 51)), exp(seq(-5,5,length.out = 51)))
    }

    X.pc1 = sweep(t.grid %*% t(X.res$loadings[,1]), 2, X.res$center, '+')
    X.pc1 = as.data.frame(clrInv(X.pc1))
    X.center = as.data.frame(t(clrInv(X.res$center)))

    g <- draw_ternary_pc(X, X.res$Xhat$`r=1`, X.pc1, X.center, X.colors)

    for(i in 1:nrow(X)){
      x = as.numeric(clr(X.res$Xhat[[length(X.res$Xhat)]][i,]))
      y = as.numeric(clr(X.res$Xhat$`r=1`[i,]))
      proj.path = cbind(seq(x[1], y[1], length.out = m),
                        seq(x[2], y[2], length.out = m),
                        seq(x[3], y[3], length.out = m))
      proj.path = as.data.frame(clrInv(proj.path))
      g <- g + geom_path(data = to_2d_V(proj.path), color = X.colors[i])
    }

  }else if(type == 'power'){

    alpha = 1/2
    if(is.null(t.grid)){
      t.grid = seq(-5, 5, length.out = 101)
    }

    X.pc1 = sweep(t.grid %*% t(X.res$loadings[,1]), 2, X.res$center, '+')
    X.pc1 = to_simplex(X.pc1[X.pc1[,1] >= 0 & X.pc1[,2] >= 0 & X.pc1[,3] >= 0,] ^ (1/alpha))
    X.pc1 = as.data.frame(X.pc1)
    X.center = to_simplex(X.res$center ^ (1/alpha))
    X.center = as.data.frame(t(X.center))

    g <- draw_ternary_pc(X, proj_simplex(X.res$Xhat$`r=1`),
                         proj_simplex(X.pc1), proj_simplex(X.center), X.colors)

    for(i in 1:nrow(X)){
      x = to_simplex(X.res$Xhat$`r=1`[i,] ^ alpha) # approximation
      y = to_simplex(X[i,] ^ alpha) # original point
      proj.path = cbind(seq(x[1], y[1], length.out = m),
                        seq(x[2], y[2], length.out = m),
                        seq(x[3], y[3], length.out = m))
      proj.path = as.data.frame(to_simplex(proj.path ^ (1/alpha)))
      g <- g +
        geom_path(data = to_2d_V(proj_simplex(proj.path)), color = X.colors[i])
    }

  }else if(type == 'psa'){

    t.grid = c(0,1)

    X.pc1 = cbind(t.grid, 1-t.grid) %*% t(X.res$Vhat$`r=1`)
    X.pc1 = as.data.frame(X.pc1)
    X.center = as.data.frame(t(X.res$Vhat$`r=0`))

    g <- draw_ternary_pc(X, X.res$Xhat$`r=1`, X.pc1, X.center, X.colors)

    for(i in 1:nrow(X)){
      x = as.numeric(X[i,])
      y = as.numeric(X.res$Xhat$`r=1`[i,])
      proj.path = cbind(seq(x[1], y[1], length.out = m),
                        seq(x[2], y[2], length.out = m),
                        seq(x[3], y[3], length.out = m))
      g <- g + geom_path(data = to_2d_V(proj.path), color = X.colors[i])
    }
  }

  g = g +
    theme(plot.title = element_text(hjust = 0.5, size = 20))



  return(g)
}

