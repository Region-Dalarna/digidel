# ---- Länskarta (karta.lan_scb i geodata-databasen) ----
# OBS: kolumnnamnet lnkod är kontrollerat mot rdgis::hamta_karttabell(), men appen är
# inte testkörd mot databasen (ingen nätverksåtkomst till den vid utvecklingstillfället).
# Kontrollera gärna lnkod mot \dbListFields() första gången appen körs med riktig uppkoppling.
hamta_lan_karta <- function(forenkling_meter = 200) {
  con <- shiny_uppkoppling_las("geodata")
  on.exit(if (!is.null(con) && DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)

  dplyr::tbl(con, dbplyr::in_schema("karta", "lan_scb")) |>
    dplyr::collect() |>
    df_till_sf() |>
    forenkla_geometri(forenkling_meter) |>
    dplyr::select(lanskod = lnkod) |>
    dplyr::mutate(lanskod = as.character(lanskod)) |>
    sf::st_transform(crs = 4326)
}

# Förenklar geometrin så att kartan skickar färre punkter till webbläsaren vid varje kartbyte.
# Toleransen är i meter, så geometrin måste ligga i SWEREF99 TM (databasens originalprojektion).
# NULL = ingen förenkling.
forenkla_geometri <- function(geo, forenkling_meter) {
  if (is.null(forenkling_meter)) return(geo)
  punkter <- function(g) sum(vapply(sf::st_geometry(g), function(x) length(unlist(x)) / 2, numeric(1)))
  fore <- punkter(geo)
  geo <- sf::st_simplify(geo, preserveTopology = TRUE, dTolerance = forenkling_meter)
  message(sprintf("Kartgeometri förenklad med %s m: %s punkter blev %s", forenkling_meter,
                  formatera_tal(fore), formatera_tal(punkter(geo))))
  geo
}
