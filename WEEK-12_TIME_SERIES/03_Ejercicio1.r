########### Exercise #########################
library(tidyverse)
library(purrr)
library(pdftools)
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
tab <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_data_frame() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
})


#1. Haz un gráfico del número de muertes por fecha. Hint: convierte la variable meses de caracteres a números usando `recode` para redefinir la variable `tab`.


#2. Crea una nueva columna "date" con la fecha de cada entrada. Hint: use the `make_date` function.

#3. Deaths vs date.

#4. Probablemente las observaciones después de mayo de 2018 no se tomaron. Rehaz el plot sin esas observaciones
