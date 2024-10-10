#' @export
qstat <- function(compute_group = ggplot2::Stat$compute_group, ...) {

  ggplot2::ggproto("StatTemp", Stat, compute_group = compute_group, ...)
 
}

#' @export
qstat_group <- function(qstat_group, ...) {

  ggplot2::ggproto("StatTemp", Stat, qstat_group = qstat_group, ...)
 
}

#' @export
qstat_panel <- function(compute_panel, ...) {

  ggplot2::ggproto("StatTemp", Stat, compute_panel = compute_panel, ...)
 
}

#' @export
qstat_layer <- function(compute_layer, ...) {

  ggplot2::ggproto("StatTemp", Stat, compute_layer = compute_layer, ...)
 
}
