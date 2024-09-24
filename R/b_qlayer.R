qlayer <- function (mapping = NULL, data = NULL, stat = "identity", geom = "point", position = "identity", 
    ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) 
{
    layer(data = data, mapping = mapping, stat = stat, geom = geom, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = rlang::list2(na.rm = na.rm, ...))
}
