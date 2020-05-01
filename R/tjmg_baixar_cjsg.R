#' Baixar pesquisa jurisprudencial do TJMG
#'
#' @param busca Busca livre
#' @param pesquisa_por Por ora somente acórdão. Logo incluo monocráticas.
#' @param dt_pub_inicial Data inicial da publicação no formato dd/mm/yyyy
#' @param dt_pub_final  Data final da publicação no formato dd/mm/yyy
#' @param diretorio Diretório onde salvar os htmls
#'
#' @details Se o número de decisões for muito grande, está função retornará o mesmo
#'     aviso da página de busca do TJMG, de que se deve refinar melhor a busca.
#' @return htmls
#' @export
#'
tjmg_baixar_cjsg <- function(busca = NULL,
                             pesquisa_por = "acordao",
                             dt_pub_inicial = "",
                             dt_pub_final = "",
                             diretorio = ".") {


  url1 <- "https://www5.tjmg.jus.br/jurisprudencia/pesquisaPalavrasEspelhoAcordao.do"
  body <-
    list(
      numeroRegistro = "1",
      totalLinhas = "1",
      palavras = busca,
      pesquisarPor = pesquisa_por,
      orderByData = "2",
      codigoOrgaoJulgador = "",
      codigoCompostoRelator = "",
      classe = "",
      codigoAssunto = "",
      dataPublicacaoInicial = dt_pub_inicial,
      dataPublicacaoFinal = dt_pub_final,
      dataJulgamentoInicial = "",
      dataJulgamentoFinal = "",
      siglaLegislativa = "",
      referenciaLegislativa = "Clique+na+lupa+para+pesquisar+as+refer\xeancias+cadastradas...",
      numeroRefLegislativa = "",
      anoRefLegislativa = "",
      legislacao = "",
      norma = "",
      descNorma = "",
      complemento_1 = "",
      listaPesquisa = "",
      descricaoTextosLegais = "",
      observacoes = "",
      linhasPorPagina = "100",
      pesquisaPalavras = "Pesquisar"
    )

  r1 <- httr::RETRY("GET",url1, query = body)

  c1 <- httr::content(r1,encoding = "latin1")

  if (xml2::xml_find_first(c1, "boolean(//p[@class='aviso'])")) {

    c1 %>%
      xml2::xml_find_first("//p[@class='aviso']") %>%
      xml2::xml_text(trim=TRUE)

  } else {

    paginas <- c1 %>%
       xml2::xml_find_first("//tr[@align='right']/td") %>%
       xml2::xml_text() %>%
       stringr::str_extract("\\d+$") %>%
       as.numeric()


    arquivo <- file.path(diretorio,paste0(stringr::str_replace_all(Sys.Date(),"\\D","_"),"_pagina_1.html"))

    writeBin(r1$content,arquivo)

    purrr::walk(2:paginas,purrr::possibly(purrrogress::with_progress(~{



      uri2 <- "https://www5.tjmg.jus.br/jurisprudencia/pesquisaPalavrasEspelhoAcordao.do"

      body <-
        list(
          palavras = busca,
          pesquisarPor = pesquisa_por,
          orderByData = "2",
          referenciaLegislativa = "Clique na lupa para pesquisar as refer\xeancias cadastradas...",
          pesquisaPalavras = "Pesquisar",
         # "",
          linhasPorPagina = "100",
          linhasPorPagina = "100",
          paginaNumero = .x
        )

      arquivo <- file.path(diretorio,paste0(stringr::str_replace_all(Sys.Date(),"\\D","_"),"_pagina_",.x,".html"))

      httr::GET(uri2,query = body,httr::write_disk(arquivo,overwrite = TRUE))

    }),NULL))

    }


}
