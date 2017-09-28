#' @rdname caixa
#' @name caixa
#' @title Generate phonetic versions of strings with Caixa's method
#'
#' @description
#' The function \code{caixa} phonentically encodes the given input
#' using algorithm developed by Caixa Economica Federal.
#'
#' @param word string or vector of strings to encode
#' @param max maximum length of the resulting encodings, in characters
#'
#' @details To be inserted
#'
#' @return a character vector containing the phonetical output of
#' \code{word}, or an NA if the \code{word} value is NA
#'
#' @family utilsIPEA
#'
#' @examples
#' caixa(c("Pedro", "Igor"))
#'
#' @useDynLib utilsIPEA
#' @importFrom Rcpp evalCpp
#' @export
caixa <- function(word, max = 20L){
    caixa_(word, max)
}
