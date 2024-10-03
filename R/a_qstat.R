qstat <- function(compute_group = ggplot2::Stat$compute_group, ...) {

  ggproto("StatTemp", Stat, compute_group = compute_group, ...)
 
}

qstat_group <- function(compute_panel, ...) {

  ggproto("StatTemp", Stat, qstat_group = qstat_group, ...)
 
}

qstat_panel <- function(compute_panel, ...) {

  ggproto("StatTemp", Stat, compute_panel = compute_panel, ...)
 
}

qstat_layer <- function(compute_layer, ...) {

  ggproto("StatTemp", Stat, compute_layer = compute_layer, ...)
 
}
