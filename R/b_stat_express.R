# stat function used in ggplot - right now putting fun and geom first
# with eye to positional augmenting
stat_express <- function(fun = NULL,
                       geom = "point", 
                       mapping = NULL, 
                       data = NULL,
                       position = "identity",
                       required_aes = NULL, 
                       default_aes = NULL, 
                       dropped_aes = NULL,
                      ...,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      computation_scope = "group"
                  ) {
  
   # Check arguments 
   if (!is.function(fun)) stop("fun must be a function")
   
   # Pass dotted arguments to a list
   fun.args <- match.call(expand.dots = FALSE)$`...`
  
   StatTemp <- ggplot2::ggproto(
   `_class` = "StatTemp", 
   `_inherit` = ggplot2::Stat,
   )
   
   if(!is.null(required_aes)){StatTemp$required_aes <- required_aes}
   if(!is.null(default_aes)){StatTemp$default_aes <- default_aes}
   if(!is.null(dropped_aes)){StatTemp$dropped_aes <- dropped_aes}
   
   if(computation_scope == "group"){StatTemp$compute_group <- compute_rasa}
   if(computation_scope == "panel"){StatTemp$compute_panel <- compute_rasa}

   ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = StatTemp,
      geom = geom,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      check.aes = FALSE,
      check.param = FALSE,
      params = list(
         fun = fun, 
         fun.args = fun.args,
         na.rm = FALSE,
         ...
      )
   )
}
