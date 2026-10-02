# ---- Inläsning och förberedelse av enkätresultat ----
# Statistiken ligger i sekretessdatabasen och läses med rollen shiny_las_sekretess.
# Sätt till NULL för att köra appen med slumpade exempeldata. Förväntat format, se hamta_digidel_data().
digidel_tabell <- list(
  databas   = "sekretess",
  anvandare = "shiny_las_sekretess",
  schema    = "digidel",
  tabell    = "enkatresultat_sammanstallt"
)

# Alla 21 länskoder och SCB:s officiella länsnamn, oavsett om länet finns i enkätdata -
# används för att kartan ska visa hela landet, inte bara de län som besvarat enkäten.
digidel_alla_lan <- tibble::tribble(
  ~lanskod, ~lansnamn,
  "01", "Stockholms län",
  "03", "Uppsala län",
  "04", "Södermanlands län",
  "05", "Östergötlands län",
  "06", "Jönköpings län",
  "07", "Kronobergs län",
  "08", "Kalmar län",
  "09", "Gotlands län",
  "10", "Blekinge län",
  "12", "Skåne län",
  "13", "Hallands län",
  "14", "Västra Götalands län",
  "17", "Värmlands län",
  "18", "Örebro län",
  "19", "Västmanlands län",
  "20", "Dalarnas län",
  "21", "Gävleborgs län",
  "22", "Västernorrlands län",
  "23", "Jämtlands län",
  "24", "Västerbottens län",
  "25", "Norrbottens län"
)

# Enkätens regionnamn skrivs inte enhetligt mot SCB:s standardnamn (t.ex. "Sörmland",
# "Västra Götalandsregionen"), så de slås upp manuellt mot länskoden i stället för att
# gissas fram. Kontrollerad mot rdverktyg::hamtaregtab() 2026-10-02.
digidel_region_lan <- tibble::tribble(
  ~region_enkat,               ~lanskod,
  "Blekinge",                  "10",
  "Dalarna",                   "20",
  "Gävleborg",                 "21",
  "Halland",                   "13",
  "Jönköpings län",            "06",
  "Kronoberg",                 "07",
  "Norrbotten",                "25",
  "Skåne",                     "12",
  "Sörmland",                  "04",
  "Uppsala",                   "03",
  "Västerbotten",              "24",
  "Västernorrland",            "22",
  "Västra Götalandsregionen",  "14",
  "Örebro län",                "18",
  "Östergötland",              "05"
)

# Enkätsvaren ligger i långt format med en rad per besök (id) och fråga som besvarades:
#
#   id       chr  besöksid, samma id för alla rader som hör till samma besök
#   tid_min  dbl  tidsåtgång i minuter för besöket, samma värde för alla rader med samma id
#   region   chr  enkätens eget (inte alltid SCB-standard) namn på länet
#   fraga    chr  frågetext, t.ex. "Fråga 1: Vad ville besökaren ha hjälp med"
#   svar     chr  svarstext. Flera frågor (t.ex. Fråga 1) tillåter flera svar per besök -
#                 då blir det flera rader med samma id och fraga men olika svar.
hamta_digidel_data <- function(tabell) {
  if (is.null(tabell)) {
    df <- skapa_exempeldata_digidel()
  } else {
    con <- shiny_uppkoppling_las(db_name = tabell$databas, db_user = tabell$anvandare)
    df <- DBI::dbReadTable(con, DBI::Id(schema = tabell$schema, table = tabell$tabell))
    DBI::dbDisconnect(con)
  }

  resultat <- forbered_digidel_data(df)
  attr(resultat, "exempeldata") <- is.null(tabell)
  resultat
}

# Lägger till lanskod och stoppar om enkätens regionnamn skulle utökas med ett namn som
# saknas i digidel_region_lan - annars skulle de besöken tyst falla bort ur kartan och
# statistiken i stället för att synas som ett fel.
forbered_digidel_data <- function(df) {
  df <- dplyr::mutate(df, id = as.character(id), tid_min = as.numeric(tid_min))

  okanda <- setdiff(unique(df$region), digidel_region_lan$region_enkat)
  if (length(okanda) > 0) {
    stop("Okända regionnamn i enkätdata, saknas i digidel_region_lan i R/data_digidel.R: ",
         paste(okanda, collapse = ", "), call. = FALSE)
  }

  df |>
    dplyr::left_join(digidel_region_lan, by = c("region" = "region_enkat")) |>
    dplyr::left_join(digidel_alla_lan, by = "lanskod")
}

# Slumpade svar i samma format som den riktiga tabellen, används när digidel_tabell är NULL.
# Täcker bara ett urval av de riktiga frågorna/svaren - tillräckligt för att testa appen
# lokalt utan uppkoppling mot sekretessdatabasen.
skapa_exempeldata_digidel <- function() {
  set.seed(42)

  fraga1_svar <- c(
    "Utskrift, kopiering och/eller skanning",
    "Bibliotekssystem, e-böcker, e-tidningar, strömmad film eller databaser",
    "Inloggning och lösenord",
    "Nybörjare, behöver hjälp att komma igång digitalt",
    "Ekonomi, grundläggande digitala betaltjänster",
    "Hitta information på internet"
  )
  fraga2_svar <- c(
    "Bibliotekets digitala tjänster, tex e-böcker, e-tidskrifter, bibliotekssystem",
    "Sociala medier och kommunikation",
    "Myndighetsärenden eller kontakt med offentlig service",
    "Bankärenden, ekonomiska transaktioner, Swish",
    NA_character_
  )

  regioner <- digidel_region_lan$region_enkat
  # Ungefär samma skeva fördelning mellan länen som i de riktiga enkätsvaren
  vikter <- c(2, 1, 2, 1, 2, 1, 4, 2, 1, 1, 2, 1, 6, 2, 5)

  besok <- data.frame(
    id = as.character(seq_len(600)),
    tid_min = sample(c(2, 5, 10, 15, 20, 30, 45, 60), 600, replace = TRUE, prob = c(20, 25, 25, 15, 8, 4, 2, 1)),
    region = sample(regioner, 600, replace = TRUE, prob = vikter),
    stringsAsFactors = FALSE
  )

  rader <- lapply(seq_len(nrow(besok)), function(i) {
    f1 <- data.frame(
      id = besok$id[i], tid_min = besok$tid_min[i], region = besok$region[i],
      fraga = "Fråga 1: Vad ville besökaren ha hjälp med",
      svar = sample(fraga1_svar, sample(1:2, 1), prob = c(30, 20, 15, 15, 10, 10)[seq_along(fraga1_svar)]),
      stringsAsFactors = FALSE
    )
    f2_svar <- sample(fraga2_svar, 1)
    if (is.na(f2_svar)) return(f1)
    f2 <- data.frame(
      id = besok$id[i], tid_min = besok$tid_min[i], region = besok$region[i],
      fraga = "Fråga 2: Kan besökarens fråga relateras till ett område?",
      svar = f2_svar,
      stringsAsFactors = FALSE
    )
    rbind(f1, f2)
  })

  do.call(rbind, rader)
}
