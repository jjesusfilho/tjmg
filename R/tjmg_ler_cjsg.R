#' Lê dados da consulta jurisprudencial baixados com tjmg_baixar_cjsg
#'
#' @param arquivos Informar arquivos
#' @param diretorio Se não informar arquivos, informar diretório
#'
#' @return tibble
#' @export
#'
tjmg_ler_cjsg <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern="\\.html$",full.names = TRUE)
  }


  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{

    x <- .x %>%
      xml2::read_html()

   classe_processo <- x %>%
                   xml2::xml_find_all("//div[@class='caixa_processo']/a") %>%
                   xml2::xml_text(trim = TRUE)
                   #stringr::str_replace_all("\\p{Z}+"," ")

   classe <- stringr::str_match(classe_processo,"(.+)(?:\n\\s+)(.+)(?:\n\\s+)(.+)")[,2:4] %>%
     tibble::as_tibble(.name_repair = ~vctrs::vec_as_names(c("classe","processo_tjmg","processo_cnj"))) %>%
     dplyr::mutate(classe = stringr::str_remove(classe,".+:")) %>%
     dplyr::mutate_all(stringr::str_squish)

   # var1 <- x %>%
   #    xml2::xml_find_all("//td[@class='corpo']/strong") %>%
   #    xml2::xml_text()

   relator <- x %>%
     xml2::xml_find_all("//td[@class='corpo']/strong/following-sibling::text()") %>%
     xml2::xml_text(trim = TRUE)


   valores <- x %>%
     xml2::xml_find_all("//div[@class='corpo']/strong/following-sibling::text()") %>%
     xml2::xml_text(trim = TRUE) %>%
     split(1:3) %>%
     tibble::as_tibble() %>%
     .[1:2] %>%
     setNames(c("data_julgamento","data_publicacao")) %>%
     dplyr::mutate_all(lubridate::dmy)

   ementa <- x %>%
     xml2::xml_find_all("//div[@class='corpo'][@style='text-align: justify;']") %>%
     xml2::xml_text(trim = TRUE) %>%
     stringr::str_remove("(?i)\\X+:") %>%
     stringr::str_squish()

    dplyr::bind_cols(classe,relator = relator,valores,ementa = ementa)

  }),NULL))



}
