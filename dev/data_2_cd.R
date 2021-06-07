# setup -------------------------------------------------------------------
library(tidyverse)


# data --------------------------------------------------------------------
data <- readRDS("~/../Downloads/06_dvars.rds")

glimpse(data)

data %>% count(segmento_ori, segmento.x, segmento.y)

data %>% count(incum)


# remover variables en monto ----------------------------------------------
vars_to_rm <- data %>%
  select(
    matches("mean_\\d{2}$"),
    matches("max_\\d{2}$"),
    matches("max_disponible_compra"),
    -matches("porc"),
    -matches("ratio"),
    -matches("n_inst"),
    -matches("mora"),
  ) %>%
  names()

data <- select(data, setdiff(names(data), vars_to_rm))


# select ------------------------------------------------------------------
d <- data %>%
  filter(segmento.x == "Segmento 4 Montos Bajos") %>%
  select(
    -contains("segmento"),
    -incum,
    -periodo_defuncion_m12,
    -sexo
    ) %>%
  rename(
    id = rut,
    time = periodo,
    bad_good = incum12
  ) %>%
  select(id, time, bad_good, everything()) %>%
  sample_frac(1) %>%
  mutate(id = row_number())

names(d)

names(d %>% select(where(negate(is.numeric))))

# rename ------------------------------------------------------------------
rename_vars <- function(x = c("saldo", "saldo_car", "saldo_car_max")){

  x %>%
    str_replace_all("banca_", "matur_") %>%
    str_replace_all("marca_base", "basemark") %>%
    str_replace_all("producto_integrado", "interprod") %>%
    str_replace_all("m4", "n5") %>%
    str_replace_all("r04", "r50") %>%
    str_replace_all("edad", "age") %>%
    str_replace_all("hipoteca", "jom") %>%
    str_replace_all("saldo_", "monte_") %>%
    str_replace_all("_car_", "_auto_") %>%
    str_replace_all("_car$", "_auto") %>%
    str_replace_all("_banco_", "_bank_") %>%
    str_replace_all("_banco$", "_bank") %>%
    str_replace_all("_avance_", "_advance_") %>%
    str_replace_all("_avance$", "_advance") %>%
    str_replace_all("_sav_", "_save_") %>%
    str_replace_all("_sav$", "_save") %>%
    str_replace_all("_cad_", "_dac_") %>%
    str_replace_all("_cad$", "_dac") %>%
    str_replace_all("_deuda_", "_dado_") %>%
    str_replace_all("_deuda$", "_dado") %>%
    str_replace_all("uso_", "puso_") %>%
    str_replace_all("_uso_", "_puso_") %>%
    str_replace_all("_uso$", "_puso") %>%
    str_replace_all("_porc_", "_perc_") %>%
    str_replace_all("_porc$", "_perc") %>%
    str_replace_all("dias_", "time_") %>%
    str_replace_all("_prom_", "_mean_") %>%
    str_replace_all("_porc_", "_perc_") %>%
    str_replace_all("_porc$", "_perc")

}

d <- d %>% rename_with(rename_vars)

d %>%
  rename_with(rename_vars) %>%
  names() %>%
  sample()

# change var --------------------------------------------------------------
change_char <- function(x = head(data$marca_base, 10)){

  x %>%
    as.factor() %>%
    as.numeric() %>%
    LETTERS[.]

}

d <- d %>%
  mutate(across(where(negate(is.numeric)), change_char))

# export ------------------------------------------------------------------
glimpse(d %>% select(1:30))
dim(d)

saveRDS(d, "data-raw/cd.rds")


