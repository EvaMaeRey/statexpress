qstat <- function(compute_group = ggplot2::Stat$compute_group, ...) {

  ggproto("StatTemp", Stat, compute_group = compute_group, ...)
  
}
