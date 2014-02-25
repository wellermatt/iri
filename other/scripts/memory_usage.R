.ls.objects <- function (pos = 1, pattern, order.by,
                         decreasing=FALSE, head=FALSE, n=5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.prettysize <- sapply(obj.size, function(r) prettyNum(r, big.mark = ",") )
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size,obj.prettysize, obj.dim)
  names(out) <- c("Type", "Size", "PrettySize", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing=decreasing), ]
  out <- out[c("Type", "PrettySize", "Rows", "Columns")]
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (head)
    out <- head(out, n)
  out
}


# shorthand
lsos <- function(..., n=10) {
  .ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}

lsos()


mem <- function() {
  bit <- 8L * .Machine$sizeof.pointer
  if (!(bit == 32L || bit == 64L)) {
    stop("Unknown architecture", call. = FALSE)
  }
  
  node_size <- if (bit == 32L) 28L else 56L
  
  usage <- gc()
  sum(usage[, 1] * c(node_size, 8)) / (1024 ^ 2)
}
#mem()

mem_change <- function(code) {
  start <- mem()
  
  expr <- substitute(code)
  eval(expr, parent.frame())
  rm(code, expr)
  
  round(mem() - start, 3)
}
# Need about 4 mb to store 1 million integers
#mem_change(x <- 1:1e6)
#> [1] 3.823
# We get that memory back when we delete it
#mem_change(rm(x))
#> [1] -3.815