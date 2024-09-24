qstat <- function(compute_group, ...) {

  ggproto("StatTemp", Stat, compute_group = compute_group, ...)
  
}
