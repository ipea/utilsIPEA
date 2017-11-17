#' Remove de da e dos of names .
#'
#' \code{remove_preposicao_nomes} return names without de, da e dos.
#'
#'
#' @param base A data table, data frame or character vector.
#' @param ... columns for apply the function
#' @param suffixo Suffix name for the new column.
#'
#' @import data.table
#' @importFrom stringr str_replace_all
#' @return the base parameter with a new column.
#'
#' @examples
#'    base <- data.frame(nome = c("João das Neves", "Pedro dos Anjos", "Maria das Gracas"))
#'    base <- remove_preposicao_nomes(base, "nome")
#' @export
#'
remove_preposicao_nomes <- function(base, ..., suffixo = "_semD"){
  return(funcao_generica(base, ..., suffixo = suffixo, FUN = remove_preposicao_nomes_coluna))

}

remove_preposicao_nomes_coluna <- function(nomes){
  subs <- " DA | DE | DOS | D. | DAS | A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | X | Z"
  subs <- paste(subs, tolower(subs), sep = "|")
  if(is.character(nomes)){
    return(str_replace_all(nomes, subs, " "))
  }
}

abrevia_nomes_meio_coluna<- function(nomes){
  novos_nomes <- sapply(nomes, USE.NAMES = F, function(nome){
    nomes_separados <- str_extract_all(nome, "(\\w+)", simplify = T)
    if(length(nomes_separados) <= 2) return(nome)
    n <- length(nomes_separados) - 1
    for(i in 2:n){
      nomes_separados[i] <- str_extract(nomes_separados[i], "^\\w{1}")
    }
    nome_corrigido<- paste(nomes_separados, collapse = " ")
    return(nome_corrigido)
  } )
  return(novos_nomes)
}


funcao_generica <- function(base, ..., suffixo, FUN, spark_conn){
  FUN <- match.fun(FUN)
  if(is.character(base)){
    if(!is.null(spark_conn)){ return(FUN(base,spark_conn)) }
    return(FUN(base))
  }
  other_columns <- unlist(eval(substitute(alist(...))))
  stopifnot(length(other_columns) > 0)
  if(!is.data.table(base)){ setDT(base) }
  new_columns <- sapply(other_columns, function(x) paste0(x, suffixo))
  if(!is.null(spark_conn)){
    mapply( function(x, y){ set(base, j = x, value = FUN(base[[y]],spark_conn)) }, new_columns, other_columns)
  } else {
    mapply( function(x, y){ set(base, j = x, value = FUN(base[[y]])) }, new_columns, other_columns)
  }
  return(base)
}


#' Abrevia o nome do meio.
#'
#' \code{abrevia_nome_meio} return names .
#'
#'
#' @param base A data table, data frame or character vector.
#' @param suffixo A character indicating the final part of the new columns' names
#' @param ... columns for apply the function
#'
#' @import data.table
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_extract
#' @return the base parameter with a new column.
#'
#' @examples
#'    base <- data.frame(nome = c("Carlos Pereira Neves", "Pedro Aparecido Anjos"))
#'    base <- remove_preposicao_nomes(base, "nome")
#' @export
#'
abrevia_nome_meio <- function(base, ..., suffixo = "_abrev"){
  return(funcao_generica(base, ..., suffixo = suffixo, FUN = abrevia_nomes_meio_coluna))

}

#' Remove commom treatment pronouns used in Brazil.
#'
#' \code{remove_pronome_tratamento} return names without treatment pronouns (Sra, Sr, Dr, etc).
#'
#'
#' @param base A data table, data frame or character vector.
#' @param suffixo Name of the new column to be created.
#' @param ... columns for apply the function
#' @param spark_conn A character with the spark's connection name. For NULL, it runs locally.
#'
#' @import data.table sparklyr dplyr
#' @importFrom stringr str_replace_all
#' @return the base param with a new column.
#'
#' @examples
#'    remove_pronome_tratamento("Dr. Fulano")
#'    remove_pronome_tratamento("Exmo. Sr. Cicrano de Tal")
#'
#'    base <- data.frame(nome = c("Ph.D Pedro Anjos", "Prof Maria Gracas", "Pe. João"))
#'    base <- remove_pronome_tratamento(base, "nome", suffixo = "_new_names")
#'
#' @export
remove_pronome_tratamento <- function(base, ..., suffixo = "_sem_pron", spark_conn = NULL){
  if(is.null(spark_conn)){
    return(funcao_generica(base, ..., suffixo = suffixo, FUN = remove_pronome_tratamento_coluna, spark_conn = spark_conn))
  } else{
    return(funcao_generica(base, ..., suffixo = suffixo, FUN = remove_pronome_tratamento_coluna_spark, spark_conn = spark_conn))
  }

}


remove_pronome_tratamento_coluna <- function(nomes){
  lista <- NULL
  data("list_pronomes",envir = environment())
  novos_nomes <- sapply(nomes, USE.NAMES = F, function(nome){
    if(is.na(nome)){ return(nome) }
      nome <- str_replace_all(nome, "\\s+"," ")
      nome <- str_replace_all(toupper(nome),lista,"")
      return(nome)
  })
  return(novos_nomes)
}


remove_pronome_tratamento_coluna_spark <- function(nomes,spark_conn){
  lista_spark <- NULL
  data("list_pronomes_spark",envir = environment())
  nomes <- data.table(nome = nomes)
  nomes_tbl <- dplyr::copy_to(spark_conn,nomes,"nomes",overwrite = TRUE)
  if(!("nomes" %in% src_tbls(spark_conn))){ stop("Unable to copy 'base' to Spark") }
  nomes_tbl <- nomes_tbl %>% dplyr::summarise(nome = toupper(nome)) %>% dplyr::mutate(regex = regexp_replace(nome,lista_spark,""))
  novos_nomes <- nomes_tbl %>% dplyr::select(regex) %>% dplyr::collect() %>% as.data.table()
  dplyr::db_drop_table(spark_conn,"nomes")
  return(novos_nomes)
}

