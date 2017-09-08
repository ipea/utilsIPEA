test_that("remove_preposicao_nomes", {
  nomes <- c("Carlos das Neves", "Pedro dos Anjos", "Maria das Gracas")
  nomes_proprios <- c("CARLOS","PEDRO","MARIA DAS GRACAS")
  nomes_retorno <- extrai_NomeProprio(nomes)
  expect_equal(c("nome", "NomeProprio"), names(nomes_retorno))
  expect_equal(nomes_proprios, nomes_retorno$NomeProprio)
})
