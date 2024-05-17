# stat function used in ggplot - we reorder from conventional
stat_panel_sf <- function(geo_ref_data = NULL,
                          fun = NULL, 
                       geom = "sf", 
                       mapping = NULL, 
                       data = NULL,
                      position = "identity",
                      required_aes = NULL,
                      default_aes = NULL,
                      dropped_aes = NULL,
                      ...,
                      show.legend = NA,
                      inherit.aes = TRUE,
                      na.rm = FALSE,
                      crs = "NAD27" # "NAD27", 5070, "WGS84", "NAD83", 4326 , 3857
) {
  
  StatTemp <- 
   ggplot2::ggproto("StatTemp", 
                   ggplot2::Stat,
                   compute_panel = compute_rasa)
  
    if(!is.null(geo_ref_data) & is.null(fun)){
    
      
      fun <- function(data, keep_id = NULL, 
                                       drop_id = NULL, 
                                       stamp = FALSE){
  
             if(!stamp){data <- dplyr::inner_join(data, geo_ref_data)}
             if( stamp){data <- geo_ref_data }
             
             if(!is.null(keep_id)){ data <- dplyr::filter(data, id_col %in% keep_id) }
             if(!is.null(drop_id)){ data <- dplyr::filter(data, !(id_col %in% drop_id))}
             
             data
  
      }
    
    
  }
  
  
   # Check arguments 
   if (!is.function(fun)) stop ("fun must be a function")
   
   # Pass dotted arguments to a list
   fun.args <- match.call(expand.dots = FALSE)$`...`
   
   if(!is.null(required_aes)){StatTemp$required_aes <- required_aes}
   if(!is.null(default_aes)){StatTemp$default_aes <- default_aes}
   if(!is.null(dropped_aes)){StatTemp$dropped_aes <- dropped_aes}
   
     c(ggplot2::layer_sf(
              data = data,
              mapping = mapping,
              stat = StatTemp,  # proto object from step 2
              geom = geom,  # inherit other behavior
              position = position,
              show.legend = show.legend,
              inherit.aes = inherit.aes,
              check.aes = FALSE,
              check.param = FALSE,
              params = rlang::list2(
                fun = fun, 
                fun.args = fun.args,
                na.rm = na.rm, ...)
              ),
              
              ggplot2::coord_sf(crs = crs,
                       default_crs = sf::st_crs(crs),
                       datum = crs,
                       default = TRUE)
     )
   
}
