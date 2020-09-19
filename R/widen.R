

# we should have ... (name1 = val1, ...) and dotted .fill
# we should skip the sub aggregation when value is a symbol

#' widen
#'
#' @param ... expressions
#' @param .fill .fill
#'
#' @export
widen <- function(..., .fill = NULL) {
  full_data_env <- sys.frames()[[length(sys.frames()) - 3]]
  group_data <- eval.parent(quote(across()))

  # the function is run several times and we don't want to repeat some
  # operations so we hide some results as attributes of the full data in an
  # environment above
  # yes this is dirty
  cached <- attr(full_data_env$.data, "pivot.widen")


  if(is.null(cached)) {

    unique_transformer <-function (text, envir) {
      eval(parse(text = sprintf("unique(%s)", text), keep.source = FALSE), envir)
    }
    full_data <- full_data_env$.data
    # rhs
    values_expr <- eval(substitute(alist(...)))
    # lhs
    raw_names <- names(values_expr)
    names_from <- vector("list", ...length())
    res <- vector("list", ...length())

    for(i in seq(...length())) {
      # lhs {} matches
      #curly_matches <- gregexpr("\\{.*?\\}", raw_names)[[1]]
      curly_matches <- regexpr("(?<=\\{).*?(?=\\})", raw_names[[i]], perl = TRUE)

      # lhs {} content
      names_from[[i]] <- mapply(
        function(pos, length) substr(raw_names[[i]], pos, pos+length-1),
        pos = curly_matches,
        length = attr(curly_matches, "match.length"))
      if(length(names_from[[i]]) > 1)
        stop("The name should (currently) contain only 1 {}")

      # lhs {} actual variable names
      names_from[[i]] <- intersect(all.vars(str2lang(names_from[[i]])), names(group_data))
      if(length(names_from[[i]]) != 1)
        stop("The {} should (currently) contain only one name from the data")

      # rhs actual variable names
      values_from <- intersect(all.vars(values_expr[[i]]), names(group_data))
      if(length(values_from) != 1)
        stop("The values should (currently) conly use one column")

      # set fill depending on type
      # for some reason using `vctrs::vec_init` didn't work
      if(is.null(.fill)) {
        .fill <- switch(
          typeof(full_data[[values_from]]),
          integer = NA_integer_,
          double = NA_real_,
          character = NA_character_,
          complex = NA_complex_,
          list = list(NULL))
      }

      # new columns
      names_to  <- glue::glue(raw_names[[i]], .transformer = unique_transformer, .envir = full_data)
      # initiate empty row
      empty_row <-

        res[[i]] <-tibble_row(!!!setNames(rep(.fill, length(names_to)), names_to))
    }

    cached <- new.env()
    attr(full_data_env$.data, "pivot.widen") <- cached
    cached$names_from  <- names_from
    cached$raw_names   <- raw_names
    cached$values_expr <- values_expr
    cached$res         <- res
  } else {
    names_from  <- cached$names_from
    raw_names   <- cached$raw_names
    values_expr <- cached$values_expr
    res         <- cached$res
  }

  for(i in seq(...length())) {
    if(is.symbol(values_expr[[i]])) {
      nms <- glue::glue(raw_names[[i]], .envir = group_data)
      vals <- group_data[[as.character(values_expr[[i]])]]
      vals <- setNames(as.list(vals), nms)
    } else  {

      sym_ <- sym(names_from[[i]])
      group_data_i <- group_data
      group_data_i[raw_names[[i]]] <-
        glue::glue(raw_names[[i]], .envir = group_data_i)

      vals <-
        group_data_i %>%
        #mutate(!!sym_ := glue::glue(raw_names[[i]], .envir = .)) %>%
        group_by(!!sym_) %>%
        summarize(!!values_expr[[i]], .groups = "drop") %>%
        deframe() %>%
        as.list()
    }

    # fill empty row with local result where available
    res[[i]][names(vals)] <- vals
  }
  res <- bind_cols(res)
  res
}
