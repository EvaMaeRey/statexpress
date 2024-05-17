stat_panel <- function(...){stat_express(computation_scope = "panel", ...)}
# stat function used in ggplot - we reorder from conventional
# stat_panel <- function(fun = NULL, 
#                        geom = "point", 
#                        mapping = NULL, data = NULL,
#                       position = "identity",
#                       required_aes = NULL,
#                       default_aes = NULL,
#                       dropped_aes = NULL,
#                       ...,
#                       show.legend = NA,
#                       inherit.aes = TRUE) {
# 
#    StatTemp <- 
#    ggplot2::ggproto("StatTemp", 
#                    ggplot2::Stat,
#                    compute_panel = compute_rasa)
#   
#    # Check arguments 
#    if (!is.function(fun)) stop ("fun must be a function")
#    
#    # Pass dotted arguments to a list
#    fun.args <- match.call(expand.dots = FALSE)$`...`
#    
#    if(!is.null(required_aes)){StatTemp$required_aes <- required_aes}
#    if(!is.null(default_aes)){StatTemp$default_aes <- default_aes}
#    if(!is.null(dropped_aes)){StatTemp$dropped_aes <- dropped_aes}
# 
#    ggplot2::layer(
#       data = data,
#       mapping = mapping,
#       stat = StatTemp,
#       geom = geom,
#       position = position,
#       show.legend = show.legend,
#       inherit.aes = inherit.aes,
#       check.aes = FALSE,
#       check.param = FALSE,
#       params = list(
#          fun = fun, 
#          fun.args = fun.args,
#          na.rm = FALSE,
#          ...
#       )
#    )
# }



