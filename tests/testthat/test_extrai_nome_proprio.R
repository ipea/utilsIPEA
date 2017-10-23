test_that("nomes proprios", {
  nomes <- c("Carlos das Neves", "Pedro dos Anjos", "Ana Santos", "Edileusa maria Ferreira", "Maria Eduarda Almeida")
  nomes_proprios <- c("CARLOS","PEDRO","ANA","EDILEUSA MARIA","MARIA EDUARDA")
  nomes_retorno <- extrai_NomeProprio(nomes)
  expect_equal(nomes_proprios, nomes_retorno)
})


test_that("nomes proprios com sexo", {
  nomes <- c("Edileusamaria Ferreira", "Carlos das Neves", "Pedro dos Anjos", "Ana Santos", "Maria Eduarda Almeida")
  nomes_proprios <- data.frame(NomeProprio = c("EDILEUSA MARIA","CARLOS","PEDRO","ANA","MARIA EDUARDA"), gender = c(1,2,2,1,1), stringsAsFactors = FALSE)
  nomes_retorno <- extrai_NomeProprio(nomes,gender = TRUE)
  expect_equal(c("NomeProprio","gender"), names(nomes_retorno))
  expect_equal(nomes_proprios, nomes_retorno)
})


test_that("nomes proprios com sobrenome", {
  nomes <- c("Carlos das Neves", "Pedro dos Anjos", "Ana Santos", "Edileusa maria Ferreira", "Maria Eduarda Almeida")
  nomes_proprios <- data.frame(NomeProprio = c("CARLOS","PEDRO","ANA","EDILEUSAMARIA","MARIAEDUARDA"), surname = c("DASNEVES", "DOSANJOS", "SANTOS", "FERREIRA", "ALMEIDA"), stringsAsFactors = FALSE)
  nomes_retorno <- extrai_NomeProprio(nomes,surname = FALSE, spaces = FALSE)
  expect_equal(c("NomeProprio","surname"), names(nomes_retorno))
  expect_equal(nomes_proprios, nomes_retorno)
})
