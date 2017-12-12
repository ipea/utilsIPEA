#' Remove de da e dos of names .
#'
#' \code{contar_letras} return number of any latters on alphabet for each column.
#'
#'
#' @param base A data table, data frame or character vector.
#' @param columns for apply the function
#' @param suffixo Suffix name for the new column.
#'
#' @import data.table
#' @return a data.table number of any latters on alphabet for each column
#'
#' @examples
#'    base <- data.frame(nome = c("João das Neves", "Pedro dos Anjos", "Maria das Gracas"))
#'    base <- contar_letras(base, "nome")
#' @export
#'
contar_letras <- function(base, columns, suffix = NULL){
  stopifnot(is.character(columns))
  ret <- lapply(columns, function(column){
    re <- data.table(a=str_count(base[[column]], 'a|A|á|Á|â|Â|ã|Â|á|À'),
                     b=str_count(base[[column]], 'b|B'),
                     c=str_count(base[[column]], 'c|C|ç|Ç'),
                     d=str_count(base[[column]], 'd|D'),
                     e=str_count(base[[column]], 'e|E|é|É|ê|Ê|è|È'),
                     f=str_count(base[[column]], 'f|F'),
                     g=str_count(base[[column]], 'g|G'),
                     h=str_count(base[[column]], 'h|H'),
                     i=str_count(base[[column]], 'i|I|í|Í|î|Î|ì|Ì'),
                     j=str_count(base[[column]], 'j|J'),
                     k=str_count(base[[column]], 'k|K'),
                     l=str_count(base[[column]], 'l|L'),
                     m=str_count(base[[column]], 'm|M'),
                     n=str_count(base[[column]], 'n|N'),
                     o=str_count(base[[column]], 'o|O|ô|ô|ó|Ó|õ|Õ|ò|Ò'),
                     p=str_count(base[[column]], 'p|P'),
                     q=str_count(base[[column]], 'q|Q'),
                     r=str_count(base[[column]], 'r|R'),
                     s=str_count(base[[column]], 's|S'),
                     t=str_count(base[[column]], 't|T'),
                     u=str_count(base[[column]], 'u|U|ú|Ú|û|û|Ù|Ù'),
                     v=str_count(base[[column]], 'v|V'),
                     w=str_count(base[[column]], 'w|W'),
                     x=str_count(base[[column]], 'x|X'),
                     y=str_count(base[[column]], 'y|Y'),
                     z=str_count(base[[column]], 'z|Z'))
    if(is.null(suffix)){
      setnames(re, names(re),paste(names(re), column, sep = "_") )
    }else{
      setnames(re, names(re),paste(names(re), suffix, sep = "_") )
    }
  })
  return(do.call(cbind, lapply(ret, data.table, stringsAsFactors=FALSE)))
}
