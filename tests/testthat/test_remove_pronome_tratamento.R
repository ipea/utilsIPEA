test_that("remove_pronome_tratamento", {
  nomes <- c("Dr. Joao das Neves", "Exmo. Sr. Pedro dos Anjos", "Maria das Gracas")
  nomes_sem_pronomes <- c("JOAO DAS NEVES", "PEDRO DOS ANJOS", "MARIA DAS GRACAS")
  base <- data.table(nome = nomes)
  base <- remove_pronome_tratamento(base, "nome")
  expect_equal(c("nome", "nome_sem_pron"), names(base))
  expect_equal(nomes_sem_pronomes, base$nome_sem_pron)
})

test_that("remove_pronome_tratamento de um vetor de caracteres", {
  nomes <- c("Dr. Joao das Neves", "Exmo. Sr. Pedro dos Anjos", "Maria das Gracas")
  nomes_sem_pronomes <- c("JOAO DAS NEVES", "PEDRO DOS ANJOS", "MARIA DAS GRACAS")
  expect_equal(nomes_sem_pronomes, remove_pronome_tratamento(nomes))
})
