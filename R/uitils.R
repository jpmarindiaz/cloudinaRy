
isUrl <- function(url){
  grepl("^http",url)
}

file_path_sans_ext <- function (x)
{
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

`%||%` <- function (x, y)
{
  if (is.empty(x))
    return(y)
  else if (is.null(x) || is.na(x))
    return(y)
  else if( class(x)=="character" && nchar(x)==0 )
    return(y)
  else x
}

#' @export
is.empty <- function(x){
  #   !is.null(x)
  !as.logical(length(x))
}

createEmptyDf <- function(nms){
  as.data.frame(t(matrix(nrow=length(nms),ncol=0,dimnames=list(nms))))
}
