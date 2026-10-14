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
plot_subsimplex <- function(V = diag(3), vnames = NULL){
  if(is.null(vnames)){
    if(!is.null(colnames(V))){
      vnames = colnames(V)
    }else{
      vnames = c('V1','V2','V3')
    }
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
  g = ggplot(df, aes(x = e1, y = e2)) +
    theme_void() +
    theme(aspect.ratio = (max(V2d)-min(V2d)+0.1*pad)/(max(V2d)-min(V2d)+0.2*pad)) +
    scale_x_continuous(limits = c(min(V2d)-0.1*pad, max(V2d)+0.1*pad)) +
    scale_y_continuous(limits = c(min(V2d)-0.1*pad, max(V2d))) +
    geom_segment(data = df, aes(x = x, y = y, xend = xend, yend = yend)) +
    geom_text(data = as.data.frame(V2d) %>%
                mutate(e1 = e1 + c(-0.1, 0.1, 0),
                       e2 = e2 + c(-0.05, -0.05, 0.1),
                       label = vnames),
              aes(x = e1, y = e2, label = label)) +
    geom_segment(data = Vgrid, aes(x = x, y = y, xend = xend, yend = yend),
                 col = 'gray', alpha = 0.4) +
    geom_text(data = as.data.frame(cbind(c(0.2,0.4,0.6,0.8,1.0) - 0.03,
                                         c(0.8,0.6,0.4,0.2,0.0) + 0.03) %*% V2d[c(2,1),]) %>%
                mutate(e2 = e2 - 0.04*pad,
                       label = c(20,40,60,80,100)),
              aes(x=e1, y=e2, label = label), size = 3, angle = 60) +
    geom_text(data = as.data.frame(cbind(c(0.2,0.4,0.6,0.8,1.0) + 0.005,
                                         c(0.8,0.6,0.4,0.2,0.0) - 0.005) %*% V2d[c(3,2),]) %>%
                mutate(e1 = e1 + 0.05*pad,
                       label = c(20,40,60,80,100)),
              aes(x=e1, y=e2, label = label), size = 3) +
    geom_text(data = as.data.frame(cbind(c(0.2,0.4,0.6,0.8,1.0) - 0.05,
                                         c(0.8,0.6,0.4,0.2,0.0) + 0.05) %*% V2d[c(1,3),]) %>%
                mutate(e1 = e1 - 0.05*pad,
                       label = c(20,40,60,80,100)),
              aes(x=e1, y=e2, label = label), size = 3, angle = -60)

  make_ticks <- function(from, to, tick_length = 0.015 * pad) {
    p <- seq(0.2, 1, by = 0.2)

    A <- V2d[from, ]
    B <- V2d[to, ]
    C <- V2d[setdiff(seq_len(3), c(from, to)), ]

    pos <- (1 - p) %o% A + p %o% B

    direction <- A - C
    direction <- direction / sqrt(sum(direction^2))

    ends <- sweep(pos, 2, tick_length * direction, "+")

    data.frame(
      x = pos[, 1],
      y = pos[, 2],
      xend = ends[, 1],
      yend = ends[, 2]
    )
  }

  ticks <- rbind(
    make_ticks(1, 2),  # Bottom edge: proportion of V2.
    make_ticks(2, 3),  # Right edge: proportion of V3.
    make_ticks(3, 1)   # Left edge: proportion of V1.
  )

  g + geom_segment(
    data = ticks,
    aes(x = x, y = y, xend = xend, yend = yend),
    inherit.aes = FALSE,
    linewidth = 0.4
  )

}

