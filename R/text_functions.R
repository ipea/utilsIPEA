#' Remove de da e dos of names .
#'
#' \code{remove_preposicao_nomes} return names without de, da e dos.
#'
#'
#' @param base A data table or data frame.
#' @param column Caracter, the column with the names.
#' @param ... more columns for apply the function
#'
#' @import data.table
#' @importFrom stringr str_replace_all
#' @return the base param with a new column.
#'
#' @export
#'

# retira "DE" "DA" e "DOS" dos nomes
remove_preposicao_nomes <- function(base, column, ...){
  other_columns <- unlist(eval(substitute(alist(...))))
  subs <- " DA | DE | DOS | D. | DAS | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | X | Z"
  subs <- paste(subs, tolower(subs), sep = "|")
  stopifnot(is.character(column))
  if(!is.data.table(base)){ setDT(base) }
  other_columns <- c(column, other_columns)
  new_columns <- sapply(other_columns, function(x) paste0(x,"_semD"))
  mapply( function(x, y){ set(base, j = x, value = str_replace_all(base[[y]], subs, " ")) },
        new_columns, other_columns)

  return(base)

}
