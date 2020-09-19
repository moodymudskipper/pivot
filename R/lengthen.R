

#' lengthen
#'
#' @param expr expr
#' @param drop_na drop_na
#'
#' @export
lengthen <- function(expr, drop_na = FALSE) {
  full_data_env <- sys.frames()[[length(sys.frames()) - 3]]
  group_data <- eval.parent(quote(across()))
  `:=` <- NULL # for cmd check notes

  # the function is run several times and we don't want to repeat some
  # operations so we hide some results as attributes of the full data in an
  # environment above
  # yes this is dirty
  cached <- attr(full_data_env$.data, "pivot.lengthen")

  if(is.null(cached)) {
    nms <- names(group_data) # names without grouping columns
    expr <- substitute(expr)
    cols_pattern <- eval(expr[[3]])
    names_to  <- eval(expr[[2]])
    values_to <- tail(names_to, 1)
    names_to  <- head(names_to, -1)

    regexpr_ <- regexpr(cols_pattern, nms, perl = TRUE)
    cols_lgl <- regexpr_ != -1
    capture.starts <- attr(regexpr_, "capture.start")[cols_lgl,, drop = FALSE]

    if(is.null(capture.starts)) {
      df <- tibble(!!sym(names_to) := nms[cols_lgl])
    } else {
      match.lengths <- attr(regexpr_, "capture.length")[cols_lgl,, drop = FALSE]
      n_groups <- ncol(capture.starts)
      # build a vector from the matched strings, and convert it to a named df
      groups <- mapply(
        function(nm, start, length) substr(nm, start, start+length-1),
        nm = unlist(replicate(n_groups, nms[cols_lgl])),
        start = capture.starts,
        length = match.lengths)
      groups <- matrix(groups, ncol = n_groups)
      group_nms <- attr(regexpr_, "capture.names")
      group_nms[group_nms == ""] <- setdiff(names_to, setdiff(group_nms, ""))
      colnames(groups) <- group_nms
      df <- as_tibble(groups)[names_to]
    }

    # record attributes
    cached <- new.env()
    attr(full_data_env$.data, "pivot.lengthen") <- cached
    cached$values_to <- values_to
    cached$cols_lgl  <- cols_lgl
    cached$df  <- df
  } else {
    values_to <- cached$values_to
    cols_lgl  <- cached$cols_lgl
    df <- cached$df

  }

  if(values_to == "_val_") {
    `_merge_` <- `_value_` <- NULL # for cmd check notes
    # the supported case here is also one with several rows per group
    # we need test cases with several rows and no "_val_", and with "_val_"
    # and single row

    # by row
    values <- c(t(
      group_data[cols_lgl]))
    df <- Map(function(split_group_data, split_df) {
      col <- split_df[["_val_"]][[1]]
      split_group_data[["_merge_"]] <- seq.int(nrow(split_group_data))
      split_group_data <-
        gather(split_group_data,"_key_", "_value_", - `_merge_`) %>%
        arrange(`_merge_`) %>%
        mutate(`_key_` = NULL) %>%
        rename(!!sym(col) := `_value_`) %>%
        as.data.frame()
      split_df["_val_"] <- NULL
      split_group_data[names(split_df)] <- split_df
      split_group_data
    },
    split.default(group_data[cols_lgl], df[["_val_"]]),
    split(df, df[["_val_"]]))

    df <- Reduce(function(x,y) merge(x, y, all = TRUE), df) %>%
      arrange(`_merge_`) %>%
      select(-`_merge_`)


  } else {
    # fetch values
    values <- unlist(
      group_data[cols_lgl],
      use.names = FALSE)

    # add the values to df
    df[[values_to]] <- values
  }

  # drop NA rows if relevant
  if(drop_na) df <- df[!is.na(values),, drop = FALSE]

  df
}
