compute_rasa <- function(data, scales, fun, fun.args) {
     # Change default arguments of the function to the 
     # values in fun.args
     args <- formals(fun)
     for (i in seq_along(fun.args)) {
        if (names(fun.args[i]) %in% names(fun.args)) {
           args[[names(fun.args[i])]] <- fun.args[[i]]
        } 
     }
     formals(fun) <- args
     
     # Apply function to data
     fun(data)
}
