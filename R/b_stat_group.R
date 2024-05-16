# stat function used in ggplot - but reordered from conventional!
stat_group <- function(fun = NULL,
                       geom = "point", 
                       mapping = NULL, 
                       data = NULL,
                       position = "identity",
                       required_aes = NULL, 
                       default_aes = NULL, 
                       dropped_aes = NULL,
                      ...,
                      show.legend = NA,
                      inherit.aes = TRUE) {
  
   StatRasagroup <- ggplot2::ggproto(
   `_class` = "StatRasagroup", 
   `_inherit` = ggplot2::Stat,
   compute_group = compute_rasa
   )
  
   # Check arguments 
   if (!is.function(fun)) stop("fun must be a function")
   
   # Pass dotted arguments to a list
   fun.args <- match.call(expand.dots = FALSE)$`...`
   
   if(!is.null(required_aes)){StatRasagroup$required_aes <- required_aes}
   if(!is.null(default_aes)){StatRasagroup$default_aes <- default_aes}
   if(!is.null(dropped_aes)){StatRasagroup$dropped_aes <- dropped_aes}
   
   ggplot2::layer(
      data = data,
      mapping = mapping,
      stat = StatRasagroup,
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
