# В архиве letters вы найдете письма Льва Толстого в формате XML.
# Вам надо клонировать репозиторий с дз, выполнить задание и закоммитить изменения.
# Не меняйте структуру репозитория. Не переименовывайте файл с заданием.
# Не переименовывайте переменные. 

# Вам надо извлечь из каждого файла том, дату и адресата, и собрать эти данные в одну таблицу. 
# дата письма: в header — тег correspAction, тип sending — тег date, атрибут when;
# адресат: в header – тег correspAction, тип receiving — имя получателя (текст)
# том: в header — biblScope, юнит vol — номер тома

# Применяйте trimws() к результату парсинга, чтобы избавиться от лишних строк. 


library(xml2)
library(dplyr)
library(purrr)

unzip("letters.zip")
my_xmls <- list.files("letters/", full.names = TRUE)

# Сначала напишите код для первого письма в датасете, чтобы потренироваться. 

test_xml <- my_xmls[1]
doc <- read_xml(test_xml)
ns <- xml_ns(doc)

  
# дата письма
date <- doc |>
  xml_find_first("//correspAction[@type='sending']/date") |>
  xml_attr("when") |>
  trimws()

# адресат письма
corresp <- doc |>
  xml_find_first("//correspAction[@type='receiving']") |>
  xml_text() |>
  trimws()

# том 
vol <- doc |>
  xml_find_first("//biblScope[@unit='vol']") |>
  xml_text() |>
  trimws()


## Когда все получится, оберните свое решение в функцию read_letter().


read_letter <- function(xml_path) {
  
  date_node <- xml_find_first(doc, "//correspAction[@type='sending']/date")
  date <- ifelse(length(date_node) == 0, 
                 NA_character_, 
                 xml_attr(date_node, "when") |>
                   trimws())
  
  corresp_node <- xml_find_first(doc, ".//correspAction[@type='receiving']")
  if (length(corresp_node) == 0) {
    corresp_node <- xml_find_first(doc, ".//correspAction[contains(@type, 'receiving')]")
  }
  
  corresp <- NA_character_
  if (length(corresp_node) > 0) {
    # Пробуем разные способы извлечения текста адресата
    corresp_text <- xml_text(corresp_node) %>% trimws()
    if (corresp_text != "") {
      corresp <- corresp_text
    } else {
      # Если текста нет напрямую, ищем в дочерних элементах
      pers_name <- xml_find_first(corresp_node, ".//persName")
      if (length(pers_name) > 0) {
        corresp <- xml_text(pers_name) %>% trimws()
      }
    }
  }
  
  vol_node <- xml_find_first(doc, "//biblScope[@unit='vol']")
  vol <- ifelse(length(vol_node) == 0, 
                NA_character_, 
                xml_text(vol_node) |>
                  trimws())

  
  # записываем в тиббл
  res <- tibble(
    vol = vol,
    date = date,
    corresp = corresp
   )

  return(res)
}


# Прочтите все письма в один тиббл при помощи map_dfr(). 
letters_tbl <- map_dfr(my_xmls, read_letter)
letters_tbl


