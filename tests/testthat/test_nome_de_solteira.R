
test_that("nome_de_solteira teste penúltimo nome", {
  nome_casada <- "Joana Neves Silva Pinto"
  nome_conjuge <- "João Neves Cunha"
  nome_antes_casar <- "Joana Silva Pinto"
  resultado <- nome_de_solteira(nome_casada, nome_conjuge)
  expect_equal(nome_antes_casar, resultado)
})

