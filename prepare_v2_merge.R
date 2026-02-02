#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(readxl)
  library(readr)
  library(stringr)
  library(tidyr)
  library(purrr)
})

split_lines <- function(x) {
  if (is.na(x) || x == "") return(character(0))
  str_split(as.character(x), "\\r?\\n")[[1]] %>%
    str_trim() %>%
    discard(~ .x == "")
}

join_lines <- function(x) {
  if (length(x) == 0) return("")
  paste(x, collapse = "\r\n")
}

map_multi <- function(x, mapping) {
  if (is.na(x) || x == "") return(x)
  parts <- split_lines(x)
  parts2 <- vapply(parts, function(p) {
    if (!is.null(mapping[[p]])) mapping[[p]] else p
  }, character(1))
  join_lines(parts2)
}

map_single <- function(x, mapping) {
  if (is.na(x) || x == "") return(x)
  x <- str_trim(as.character(x))
  if (!is.null(mapping[[x]])) mapping[[x]] else x
}

age_map <- list(
  "Mwin ke 18 an" = "Moins de 18 ans",
  "Antre 18 ek 24" = "Entre 18 et 24 ans",
  "Antre 18 ek 24 an" = "Entre 18 et 24 ans",
  "Antre 25 ek 29 an" = "Entre 25 et 29 ans",
  "Antre 30 ek 34 an" = "Entre 30 et 34 ans",
  "Antre 35 ek 39 an" = "Entre 35 et 39 ans",
  "Antre 40 ek 44 an" = "Entre 40 et 44 ans",
  "Antre 45 ek 49 an" = "Entre 45 et 49 ans",
  "50 an ek pli" = "50 ans et plus",
  "50 an ek plis" = "50 ans et plus"
)

yn_map <- list("WI" = "OUI", "NON" = "NON")

q3_map <- list(
  "Fos grosess" = "Un arrêt précoce de grossesse",
  "Malfomasyon lo ker" = "Une malformation du cœur",
  "Problèm mantal, difikilté po aprand lékol" = "Des troubles psychiatriques",
  "Lamor subit de lo ti baba" = "Une mort subite du nourrisson",
  "Épilépsi" = "De l’épilepsie",
  "Retar dann dévlopman le cerveau" = "Des retards de développement neurologique",
  "Rétar dann dévlopman le cerveau" = "Des retards de développement neurologique"
)

q4_map <- list(
  "Rétar mantal" = "Un retard mental",
  "Retar mantal" = "Un retard mental",
  "Rétar dévlopman (ti tay, ti poids)" = "Un retard de croissance (petit poids et petite taille)",
  "Rétar dévlopman (ti tay, ti poids)" = "Un retard de croissance (petit poids et petite taille)",
  "Problem lo zyé" = "Une déficience visuelle",
  "Problèm konportman" = "Des troubles du comportement",
  "Malfomasyon lo vizaz" = "Des malformations du visage"
)

q5_map <- list(
  "Dann lo vant lo moman" = "Dans le ventre de la mère",
  "Dépi 6 an" = "Dès 6 ans",
  "A l’adolescans" = "À l'adolescence",
  "A l' adolescans" = "À l'adolescence",
  "A l’az adult" = "À l’âge adulte",
  "A l'az adult" = "À l’âge adulte",
  "Tou lo long la vi" = "Tout au long de la vie"
)

q6_map <- list(
  "Dépi premier ver" = "Dès le premier verre",
  "A partir 5 ver" = "À partir de 5 verres",
  "A partir 20 ver" = "À partir de 20 verres",
  "Plis ke 1 ver par mwa" = "Plus d’1 verre par mois",
  "Plis ke 1 ver par zour" = "Plus d’1 verre par jour"
)

q7_map <- list(
  "Tou nana mèm kantité" = "Tous contiennent la même quantité d’alcool",
  "In ver vin" = "Un verre de vin",
  "Rhum aranzé" = "Rhum arrangé",
  "Whisky" = "Whisky",
  "Mousseux" = "Mousseux",
  "Cocktail" = "Cocktail"
)

q8_map <- list(
  "Li osi li pé koz déga si li boi" = "Lui aussi pourrait causer des troubles à l’enfant s’il consomme de l’alcool",
  "Li doi arét boir ossi" = "Il devrait arrêter l’alcool",
  "Li doi souténir son femm" = "Il devrait être plus présent pour sa femme"
)

prepare_q1 <- function(path_fr, path_cr) {
  q1_fr <- read_excel(path_fr) %>% mutate(Langue = "français")
  q1_cr <- read_excel(path_cr) %>% mutate(Langue = "créole")

  # Canonical french headers
  fr_cols <- names(q1_fr)

  # Map creole columns to french equivalents
  creole_to_fr <- c(
    "Kélaz zot nana ?" = "Quel âge avez-vous ?",
    "Dann kél vil zot i habit ?" = "Dans quelle ville habitez-vous ?",
    "Zot i panse ke boi l’alkol pendant grosess i pé fé mal lo ti baba ?" =
      "Pensez-vous que boire de l’alcool pendant la grossesse peut nuire au bébé ?",
    "L’alkol ke lo moman I boi I sa va juska dann le korp lo ti baba ?" =
      "Est-ce que l’alcool bu par la mère arrive jusqu’au bébé ?",
    "Kosa l’alkol ilé kapab koz lo ti baba ?" =
      "Quelles peuvent être les conséquences de l’alcool sur le bébé à naître ?",
    "SAF (Syndrome d’Alcoolisation Fœtale) lé pli gro problèm l’alkol i pé fé pendant grosess. Kosa li i provoque ?" =
      "Le syndrome d’alcoolisation fœtale (SAF) est la conséquence la plus grave de l’alcool pendant la grossesse. Que provoque-t-il ?",
    "Kan sa bann problèm i pé parèt ?" =
      "À quel âge peut-on détecter les troubles liés à l’alcool pendant la grossesse ?",
    "Kantité l’alkol risqué pendant grosess ?" =
      "Quelle quantité d’alcool est risquée pendant la grossesse ?",
    "Kél ver nana plis alkol ?" =
      "Lequel de ces verres contient le plus d’alcool ?",
    "Kosa lo papa i pé fé pou évit bannd risk pou lo ti baba ?" =
      "Quelle peut être la place de l’homme dans la prévention des risques liés à l’alcool pendant la grossesse ?",
    "Es ke zot i peu met zot adresse mail s’il vous plaît ?" =
      "Pouvez-vous renseigner votre adresse mail s’il vous plaît ?",
    "Bravo ! Zot la gagne la note de : " =
      "Bravo ! (N’oubliez pas de cliquer sur « soumettre ») Vous avez obtenu la note de :"
  )

  q1_cr2 <- q1_cr %>%
    rename_with(~ ifelse(.x %in% names(creole_to_fr), creole_to_fr[.x], .x))

  # Translate age + answers
  q1_cr2 <- q1_cr2 %>%
    mutate(
      `Quel âge avez-vous ?` = vapply(`Quel âge avez-vous ?`, map_single, character(1), age_map),
      `Pensez-vous que boire de l’alcool pendant la grossesse peut nuire au bébé ?` = vapply(
        `Pensez-vous que boire de l’alcool pendant la grossesse peut nuire au bébé ?`,
        map_single,
        character(1),
        yn_map
      ),
      `Est-ce que l’alcool bu par la mère arrive jusqu’au bébé ?` = vapply(
        `Est-ce que l’alcool bu par la mère arrive jusqu’au bébé ?`,
        map_single,
        character(1),
        yn_map
      ),
      `Quelles peuvent être les conséquences de l’alcool sur le bébé à naître ?` = vapply(
        `Quelles peuvent être les conséquences de l’alcool sur le bébé à naître ?`,
        map_multi,
        character(1),
        q3_map
      ),
      `Le syndrome d’alcoolisation fœtale (SAF) est la conséquence la plus grave de l’alcool pendant la grossesse. Que provoque-t-il ?` = vapply(
        `Le syndrome d’alcoolisation fœtale (SAF) est la conséquence la plus grave de l’alcool pendant la grossesse. Que provoque-t-il ?`,
        map_multi,
        character(1),
        q4_map
      ),
      `À quel âge peut-on détecter les troubles liés à l’alcool pendant la grossesse ?` = vapply(
        `À quel âge peut-on détecter les troubles liés à l’alcool pendant la grossesse ?`,
        map_multi,
        character(1),
        q5_map
      ),
      `Quelle quantité d’alcool est risquée pendant la grossesse ?` = vapply(
        `Quelle quantité d’alcool est risquée pendant la grossesse ?`,
        map_single,
        character(1),
        q6_map
      ),
      `Lequel de ces verres contient le plus d’alcool ?` = vapply(
        `Lequel de ces verres contient le plus d’alcool ?`,
        map_single,
        character(1),
        q7_map
      ),
      `Quelle peut être la place de l’homme dans la prévention des risques liés à l’alcool pendant la grossesse ?` = vapply(
        `Quelle peut être la place de l’homme dans la prévention des risques liés à l’alcool pendant la grossesse ?`,
        map_multi,
        character(1),
        q8_map
      )
    )

  all_cols <- union(fr_cols, names(q1_cr2))
  q1_fr2 <- q1_fr %>% select(any_of(all_cols))
  q1_cr3 <- q1_cr2 %>% select(any_of(all_cols))

  out <- bind_rows(q1_fr2, q1_cr3)
  out
}

prepare_q2 <- function(path_fr, path_cr) {
  q2_fr <- read_excel(path_fr) %>% mutate(Langue = "français")
  q2_cr <- read_excel(path_cr) %>% mutate(Langue = "créole")

  fr_cols <- names(q2_fr)

  creole_to_fr <- c(
    "Kélaz zot nana ?" = "Quel âge avez-vous ?",
    "Dann kél vil zot i habit ?" = "Dans quelle ville habitez-vous ?",
    "Zot i panse ke boi l’alkol pendant grosess i pé fé mal lo ti baba ?" =
      "Pensez-vous que boire de l’alcool pendant la grossesse peut nuire au bébé ?",
    "L’alkol ke lo moman I boi I sa va juska dann le korp lo ti baba ?" =
      "Est-ce que l’alcool bu par la mère arrive jusqu’au bébé ?",
    "Kosa l’alkol ilé kapab koz lo ti baba ?" =
      "Quelles peuvent être les conséquences de l’alcool sur le bébé à naître ?",
    "SAF (Syndrome d’Alcoolisation Fœtale) lé pli gro problèm l’alkol i pé fé pendant grosess. Kosa li i provoque ?" =
      "Le syndrome d’alcoolisation fœtale (SAF) est la conséquence la plus grave de l’alcool pendant la grossesse. Que provoque-t-il ?",
    "Kan sa bann problèm i pé parèt ?" =
      "À quel âge peut-on détecter les troubles liés à l’alcool pendant la grossesse ?",
    "Kantité l’alkol risqué pendant grosess ?" =
      "Quelle quantité d’alcool est risquée pendant la grossesse ?",
    "Kél ver nana plis alkol ?" =
      "Lequel de ces verres contient le plus d’alcool ?",
    "Kosa lo papa i pé fé pou évit bannd risk pou lo ti baba ?" =
      "Quelle peut être la place de l’homme dans la prévention des risques liés à l’alcool pendant la grossesse ?",
    "Bravo ! Zot la gagne la note de : " =
      "Bravo ! Vous avez obtenu la note de :"
  )

  q2_cr2 <- q2_cr %>%
    rename_with(~ ifelse(.x %in% names(creole_to_fr), creole_to_fr[.x], .x))

  q2_cr2 <- q2_cr2 %>%
    mutate(
      `Quel âge avez-vous ?` = vapply(`Quel âge avez-vous ?`, map_single, character(1), age_map),
      `Pensez-vous que boire de l’alcool pendant la grossesse peut nuire au bébé ?` = vapply(
        `Pensez-vous que boire de l’alcool pendant la grossesse peut nuire au bébé ?`,
        map_single,
        character(1),
        yn_map
      ),
      `Est-ce que l’alcool bu par la mère arrive jusqu’au bébé ?` = vapply(
        `Est-ce que l’alcool bu par la mère arrive jusqu’au bébé ?`,
        map_single,
        character(1),
        yn_map
      ),
      `Quelles peuvent être les conséquences de l’alcool sur le bébé à naître ?` = vapply(
        `Quelles peuvent être les conséquences de l’alcool sur le bébé à naître ?`,
        map_multi,
        character(1),
        q3_map
      ),
      `Le syndrome d’alcoolisation fœtale (SAF) est la conséquence la plus grave de l’alcool pendant la grossesse. Que provoque-t-il ?` = vapply(
        `Le syndrome d’alcoolisation fœtale (SAF) est la conséquence la plus grave de l’alcool pendant la grossesse. Que provoque-t-il ?`,
        map_multi,
        character(1),
        q4_map
      ),
      `À quel âge peut-on détecter les troubles liés à l’alcool pendant la grossesse ?` = vapply(
        `À quel âge peut-on détecter les troubles liés à l’alcool pendant la grossesse ?`,
        map_multi,
        character(1),
        q5_map
      ),
      `Quelle quantité d’alcool est risquée pendant la grossesse ?` = vapply(
        `Quelle quantité d’alcool est risquée pendant la grossesse ?`,
        map_single,
        character(1),
        q6_map
      ),
      `Lequel de ces verres contient le plus d’alcool ?` = vapply(
        `Lequel de ces verres contient le plus d’alcool ?`,
        map_single,
        character(1),
        q7_map
      ),
      `Quelle peut être la place de l’homme dans la prévention des risques liés à l’alcool pendant la grossesse ?` = vapply(
        `Quelle peut être la place de l’homme dans la prévention des risques liés à l’alcool pendant la grossesse ?`,
        map_multi,
        character(1),
        q8_map
      )
    )

  all_cols <- union(fr_cols, names(q2_cr2))
  q2_fr2 <- q2_fr %>% select(any_of(all_cols))
  q2_cr3 <- q2_cr2 %>% select(any_of(all_cols))

  bind_rows(q2_fr2, q2_cr3)
}

q1_out <- prepare_q1("Q1_v2_francais.xlsx", "Q1_v2_creole.xlsx")
q2_out <- prepare_q2("Q2_v2_francais.xlsx", "Q2_v2_creole.xlsx")

write_delim(q1_out, "Q1_v2_merged.csv", delim = ";", na = "")
write_delim(q2_out, "Q2_v2_merged.csv", delim = ";", na = "")

cat("Wrote Q1_v2_merged.csv:", nrow(q1_out), "rows\n")
cat("Wrote Q2_v2_merged.csv:", nrow(q2_out), "rows\n")
