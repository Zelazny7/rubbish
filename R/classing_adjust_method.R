#' @include classing_class.R

#' @export
Classing$methods(adjust = function(meta=NULL, ...) {
  i <- 1

  while(i <= length(x)) {
    cat("\014")
    variables[[i]]$show()
    variables[[i]]$plot()
    cat ("\nEnter command (Q to quit):")
    command <- readLines(n = 1)
    if (command == "Q") {
      break
    }  else if (command %in% c("h", "help")) {
      cat(
        "binnr interactive commands:
        (Q)uit
        (n)ext, (N)ext in model
        (p)revious, (P)revious in model
        (g)oto
        (m)ono
        (e)xceptions
        (s)et equal
        (u)ndo
        (r)eset
        (d)rop
        (a)ssign reason code
        (c)omment
        binnr bin operations
        != <#> : Neutralize level
        +  <#> : Expand level
        -  <#> : Collapse level(s)
        <= <#> : Cap at # and rebin\n")
      cat("Press any key to continue")
      readLines(n=1)
      invisible()
    # } else if (command == "c") {
    #   cat("Enter a comment")
    #   inp <- readLines(n=1)
    #   invisible()
    #   if (inp == "DELETE") {
    #     out[[i]]$notes <- NULL
    #   } else if (length(grep("\\S+", inp)) > 0) {
    #     out[[i]]$notes <- paste(out[[i]]$notes, inp, sep="\n -")
    #   }
    # } else if (command == "a") {
    #   cat("Enter position(optional) and reason code")
    #   inp <- readLines(n=1)
    #   invisible()
    #   type1 <- grep("\\d+\\s+\\S+", inp) # check for matching input
    #   type2 <- grep("^\\s*\\S+\\s*$", inp) # check for matching input
    #   if (length(type1) > 0) {
    #     inp <- strsplit(inp, "\\s+")
    #     pos <- as.integer(inp[[1]][1])
    #     aac <- (inp[[1]][2])
    #     rcs(out[[i]])[pos] <- aac
    #   } else if (length(type2) > 0) {
    #     aac <- gsub("\\s", "", inp)
    #     rcs(out[[i]]) <- aac
    #   }
    } else if (command == "g") {
      cat("Goto variable:")
      v <- readLines(n = 1)
      # find the position of the variable
      while (!(v %in% c("","Q"))) {
        pos <- which(names(variables) == v)[1]
        if (is.na(pos)) {
          # find similar matches
          sim <- agrep(v, names(variables), ignore.case = T, max.distance = 0.1)
          if (length(sim) > 0){
            cat(sprintf("%s not found, similar matches:", v))
            cat(sprintf("\n %2d: %s", seq_along(sim), names(variables)[sim]))
            cat("\nGoto variable:")
            input <- readLines(n = 1)
            n <- suppressWarnings(as.integer(input))
            if (!is.na(n) & n <= length(sim)) { # check if number entered
              v <- names(variables)[sim[n]]
            } else {
              v <- input
            }
          } else {
            cat("No similar variables found")
            cat("\nHit [Enter] to continue")
            readLines(n=1)
            invisible()
            break
          }
        } else { # found exact match
          i <- pos
          break
        }
      }
    } else if (command == "d") {
      step[names(variables)[i]] <<- 3
    } else if (command == "m") {
      cat("Enter Monotonicity:")
      v <- readLines(n = 1)
      variables[[i]]$mono(v)
    } else if (command == "e") {
      cat("Enter Exceptions:")
      v <- readLines(n = 1)
      e <- eval(parse(text=v))
      if (is.numeric(e) | is.null(e)) {
        variables[[i]]$exceptions(e)
      }
    } else if (command == "s") {
      cat("Enter Level to Change:")
      v1 <- as.integer(readLines(n = 1))
      cat("Change WoE to which level?:")
      v2 <- as.integer(readLines(n = 1))
      variables[[i]]$set_equal(v1, v2)
    } else if (command == "n") {
      i <- i + 1
    # } else if (command == "N") {
    #   nv <- get.meta.attr(out, "new")
    #   nvi <- which(nv) # index of the in-model
    #   if (any(nv) & any(nvi > i)) i <- nvi[nvi > i][1]
    } else if (command == "p") {
      if (i > 1) {
        i <- i - 1
      } else {
        cat("\nAt beginning of list")
      }
    # } else if (command == "P") {
    #   nv <- get.meta.attr(out, "new")
    #   nvi <- rev(which(nv)) # index of the last in model
    #   if (any(nv) & any(nvi < i)) i <- nvi[nvi < i][1]
    } else if (command == "u") {
      variables[[i]]$undo()
    } else if (command == "r") {
      variables[[i]]$reset()
    } else {
      tryCatch({
        eval(parse(text=paste("variables[[i]]", command)))
      }, error = function(err) {
        cat("\nInvalid command entered")
      })
    }
  }
})
