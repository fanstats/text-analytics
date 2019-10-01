syn.replace = compiler::cmpfun(function (terms, protect = NULL) {
  skip <- integer(0)
  replace.list <- c()
  replace.w <- c()
  remove <- c()
  for (i in seq_along(terms)) {
    if (i %in% skip) 
      next
    s <- qdap::syn(terms[i],report.null=F)
    m <- terms %in% unlist(s)
    m[terms[m] %in% protect] <- FALSE
    if (sum(m) >= 1) {
      replace.w <- c(replace.w, sprintf("[%s][%s]", paste(terms[m], 
                                                          collapse = ", "), terms[i]))
      terms[m] <- terms[i]
      skip <- c(skip, which(m))
    }
  }
  if (!is.null(replace.w)) {
    replaced.words <- unlist(strsplit(replace.w, split = "\\]\\["))
    replaced.words <- gsub(replaced.words, pattern = "\\[", 
                           replacement = "")[seq(1, length(replaced.words), 
                                                 by = 2)]
    replacement.words <- unlist(strsplit(replace.w, split = "\\]\\["))
    replacement.words <- gsub(replacement.words, pattern = "\\]", 
                              replacement = "")[seq(2, length(replacement.words), 
                                                    by = 2)]
    replace.list <- cbind(replaced.words, replacement.words)
    for (j in 1:nrow(replace.list)) {
      r <- unique(unlist(strsplit(replace.list[j, 1], split = ", ")))
      r <- r[r != replace.list[j, 2]]
      if (length(r) > 0) {
        replace.list[j, 1] <- paste(unique(r), collapse = " ")
      }
      if (length(r) == 0) {
        remove <- c(remove, j)
      }
    }
    if (length(remove) > 0) {
      replace.list <- replace.list[-remove, ]
    }
    replace.list <- replace.list[replace.list[, 1] != replace.list[, 
                                                                   2], ]
    replace.list <- matrix(replace.list, ncol = 2)
    remove <- c()
    if (nrow(replace.list) != 0) {
      for (j in 1:nrow(replace.list)) {
        r <- unlist(strsplit(replace.list[j, 1], split = " "))
        if (length(r) > 1) {
          replace.list <- rbind(replace.list, cbind(r, 
                                                    replace.list[j, 2]))
          remove <- c(remove, j)
        }
      }
    }
    if (length(remove) > 0) {
      replace.list <- replace.list[-remove, ]
    }
    output <- list(terms, replace.list)
    names(output) <- c("terms", "replacements")
  }
  if (length(replace.w) == 0 | length(replace.list) == 0) {
    output <- NULL
  }
  output
})
