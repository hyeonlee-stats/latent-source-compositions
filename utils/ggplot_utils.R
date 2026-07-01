gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

get_lims <- function(x, margin = 0.1){
  llim = min(x) - margin*(max(x)-min(x))
  ulim = max(x) + margin*(max(x)-min(x))
  return(c(llim, ulim))
}
