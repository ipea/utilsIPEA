% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nome_de_solteira.R
\name{nome_de_solteira}
\alias{nome_de_solteira}
\title{Return women single's name}
\usage{
nome_de_solteira(nome_casada, nome_conjuge)
}
\arguments{
\item{nome_casada}{Character, married woman's name}

\item{nome_conjuge}{character, husband's name.}
}
\value{
Returns a list of possible names
}
\description{
\code{nome_de_solteira} Return women single's name using the husband last name.
}
\examples{
  nome_de_solteira(nome_casada = "Maria Conceicao da Costa", nome_conjuge = "Mario Silva da Costa")
}
