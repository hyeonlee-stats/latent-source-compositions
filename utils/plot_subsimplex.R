#' @title Convert High-dimensional Coordinates to 2D Coordinates
#' @description Converts high-dimensional coordinates to 2D coordinates.
#' 2D coordinates are computed with respect to the vertex matrix V.
#' V1 is set to origin; the edge between V1 and V2 is aligned to the positive x axis.
#' The lengths of edges of the 2D simplex are proportional to actual distances between vertices.
#' @param X matrix of points; n times d+1
#' @param V matrix of vertices; d+1 times 3.
#' @return a data frame with columns e1 and e2
to_2d_V <- function(X, V = diag(3)){
  X = as.matrix(X)
  V = as.matrix(V)
  X = sweep(X, 2, V[,1], '-')
  V = sweep(V, 1, V[,1], '-')
  e1 = V[,2]/sqrt(sum(V[,2]^2))
  v2 = V[,3] - sum(V[,3]*e1)*e1
  e2 = v2/sqrt(sum(v2^2))
  return(as.data.frame(X %*% cbind(e1, e2)))
}

#' @title Draw an Empty Ternary Diagram
#' @description Draw an empty ternary diagram with vertex matrix V. See @seealso [to_2d_V()]
#' @param V matrix of vertices
#' @return a ggplot object with aes(x = e1, y = e2)
plot_subsimplex <- function(V = diag(3)){
  if(is.null(colnames(V))){
    colnames(V) = c('V1','V2','V3')
  }
  V2d = as.matrix(to_2d_V(t(V), V))
  pad = max(V2d) - min(V2d)

  df = cbind(V2d, V2d[c(2,3,1),]) %>%
    as.data.frame() %>%
    setNames(c('x','y','xend','yend'))

  t = seq(0.1,0.9,0.1)
  t = cbind(t, 1-t)
  Vgrid = rbind(t %*% cbind(V2d[c(2,1),], V2d[c(3,1),]),
                t %*% cbind(V2d[c(1,2),], V2d[c(3,2),]),
                t %*% cbind(V2d[c(1,3),], V2d[c(2,3),])) %>%
    as.data.frame() %>%
    setNames(c('x','y','xend','yend'))
  ggplot(df, aes(x = e1, y = e2)) +
    theme_void() +
    theme(aspect.ratio = 1) +
    scale_x_continuous(limits = c(min(V2d)-0.05*pad, max(V2d)+0.05*pad)) +
    scale_y_continuous(limits = c(min(V2d)-0.05*pad, max(V2d)+0.05*pad)) +
    geom_segment(data = df, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = as.data.frame(V2d) %>%
                mutate(e1 = e1 + c(-0.05, 0.05, 0),
                       e2 = e2 + c(0, 0, 0.05),
                       label = colnames(V)),
              aes(x = e1, y = e2, label = label)) +
    geom_segment(data = Vgrid, aes(x = x, y = y, xend = xend, yend = yend),
                 col = 'gray', alpha = 0.4) +
    geom_text(data = as.data.frame(t[c(2,4,6,8),] %*% V2d[c(2,1),]) %>%
                mutate(e2 = e2 - 0.05*pad,
                       label = c(20,40,60,80)),
              aes(x=e1, y=e2, label = label), size = 3) +
    geom_text(data = as.data.frame(t[c(2,4,6,8),] %*% V2d[c(3,2),]) %>%
                mutate(e1 = e1 + 0.05*pad,
                       label = c(20,40,60,80)),
              aes(x=e1, y=e2, label = label), size = 3) +
    geom_text(data = as.data.frame(t[c(2,4,6,8),] %*% V2d[c(1,3),]) %>%
                mutate(e1 = e1 - 0.05*pad,
                       label = c(20,40,60,80)),
              aes(x=e1, y=e2, label = label), size = 3)
}
