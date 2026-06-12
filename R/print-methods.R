# Print methods for the ensemble-analysis result classes. Following the
# family's two-layer convention, the underlying objects remain plain lists of
# matrices/data.frames (so $-access and downstream code are unchanged); these
# methods just give a readable summary at the console. Plotting lives in
# lipdViz (plotCorEns/plotRegressEns/plotSpectraEns/plotPcaEns).

#' @export
print.corEns <- function(x, ...) {
  n <- if (!is.null(x$cor.df)) nrow(x$cor.df) else NA
  cat("<corEns> ensemble correlation\n")
  cat("  ", n, " ensemble correlations\n", sep = "")
  if (!is.null(x$cor.stats)) {
    med <- x$cor.stats$r[which.min(abs(x$cor.stats$percentiles - 0.5))]
    cat("  median r: ", signif(med, 3), "\n", sep = "")
  }
  if (!is.null(x$cor.fdr.stats)) {
    cat("  significance columns: ",
        paste(names(x$cor.fdr.stats), collapse = ", "), "\n", sep = "")
  }
  cat("  components: ", paste(names(x), collapse = ", "), "\n", sep = "")
  invisible(x)
}

#' @export
print.regressEns <- function(x, ...) {
  cat("<regressEns> ensemble regression\n")
  cat("  ", length(x$m), " regressions\n", sep = "")
  if (!is.null(x$regStats)) {
    mid <- which.min(abs(x$regStats$percentiles - 0.5))
    cat("  median slope: ", signif(x$regStats$m[mid], 3),
        " | intercept: ", signif(x$regStats$b[mid], 3), "\n", sep = "")
  }
  cat("  modeled Y: ", NROW(x$modeledY), " x ", NCOL(x$modeledY),
      " (time x ensemble)\n", sep = "")
  invisible(x)
}

#' @export
print.spectraEns <- function(x, ...) {
  cat("<spectraEns> ensemble power spectra\n")
  nf <- NROW(x$power)
  ne <- NCOL(x$power)
  cat("  ", nf, " frequencies x ", ne, " ensemble members\n", sep = "")
  fr <- range(x$freqs, na.rm = TRUE)
  cat("  frequency range: ", signif(fr[1], 3), " - ", signif(fr[2], 3), "\n", sep = "")
  if (!is.null(x$power.CL) && !all(is.na(x$power.CL))) {
    cat("  includes confidence limits\n")
  }
  invisible(x)
}

#' @export
print.pcaEns <- function(x, ...) {
  cat("<pcaEns> ensemble principal component analysis\n")
  nPC <- NROW(x$variance)
  ne <- NCOL(x$variance)
  cat("  ", nPC, " PCs x ", ne, " ensemble members\n", sep = "")
  if (!is.null(x$variance)) {
    v <- apply(x$variance, 1, stats::median, na.rm = TRUE)
    topn <- min(3, length(v))
    cat("  median variance explained (PC1-", topn, "): ",
        paste0(signif(100 * v[seq_len(topn)], 2), "%", collapse = ", "),
        "\n", sep = "")
  }
  invisible(x)
}
