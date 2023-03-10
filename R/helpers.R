est_names_by_op <- function(est, op) {
    est1 <- est[est$op %in% op, ]
    out <- paste0(est1$lhs, est1$op, est1$rhs)
    out
  }

est_names_selected <- function(est, params) {
    pnames_full <- paste0(est$lhs, est$op, est$rhs)
    pnames_user <- gsub(" ", "", params)
    out1 <- pnames_full[which(pnames_full %in% pnames_user)]
    out2 <- est_names_by_op(est, params)
    out <- unique(c(out1, out2))
    out
  }

est_names_free <- function(fit) {
    ptable <- lavaan::parameterTable(fit)
    # For using lhs-op-rhs
    ptable$label <- ""
    lavaan::lav_partable_labels(ptable, type = "free")
  }

est_ids_selected <- function(fit, params) {
    # This function returns all valid rows, free or not
    ops <- c("~~", "~", "=~", "~1", "|", "~*~", "<~")
    pars1 <- setdiff(params, ops)
    if (length(pars1) > 0) {
        out1 <- pars_id(pars = params, fit = fit, where = "partable")
      } else {
        out1 <- numeric(0)
      }
    pars2 <- intersect(params, ops)
    if (length(pars2) > 0) {
        ptable <- lavaan::parameterTable(fit)
        out2 <- ptable$id[ptable$op %in% pars2]
      } else {
        out2 <- numeric(0)
      }
    out <- union(out1, out2)
    out
  }