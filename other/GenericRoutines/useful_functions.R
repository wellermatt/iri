

#========= parallel processing utilities ============
#  iterator function for data.table splitting (reduces the data overhead)
isplitDT <- function(x, vals) {
  ival <- iter(vals)
  nextEl <- function() {
    val <- nextElem(ival)
    list(value=x[val], key=val)
  }
  obj <- list(nextElem=nextEl)
  class(obj) <- c('abstractiter', 'iter')
  obj
}

dtcomb <- function(...) {
  rbindlist(list(...))
}
