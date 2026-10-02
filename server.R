shinyServer(function(input, output, session) {
  rdshinyappar::telemetri_server(telemetry, navigation_id = 'flikval', forsta_flik = 'Karta och diagram')

  updateSelectInput(session, "val_fraga", choices = digidel_fragor, selected = digidel_fragor[1])

  valt_lan <- reactiveVal(NULL)   # NULL = hela riket

  # ---- Karta: besöksantal per län (oberoende av frågeval) ----
  # Alla 21 län ska visas, även de utan besök - därför left_join från lan_sf (21 rader),
  # inte ett join som bara ger med sig de län som finns i enkätdata.

  antal_besok_per_lan <- dplyr::count(digidel_besok, lanskod, name = "antal_besok")

  karta_data <- lan_sf |>
    dplyr::left_join(antal_besok_per_lan, by = "lanskod") |>
    dplyr::left_join(digidel_alla_lan, by = "lanskod")

  output$karta_digidel <- renderLeaflet({
    bbox <- sf::st_bbox(lan_sf)
    leaflet() |>
      addProviderTiles("OpenStreetMap.Mapnik", options = providerTileOptions(opacity = 0.45)) |>
      fitBounds(bbox[["xmin"]], bbox[["ymin"]], bbox[["xmax"]], bbox[["ymax"]]) |>
      addEasyButton(
        easyButton(
          icon = "fa-home",
          title = "Visa hela riket",
          onClick = JS("function(btn, map){ Shiny.setInputValue('reset_karta', true); }")
        )
      )
  })

  observe({
    doman <- if (any(!is.na(karta_data$antal_besok))) karta_data$antal_besok else 0
    pal <- colorNumeric(digidel_kartpalett, domain = doman, na.color = "#f1f1f1")

    etiketter <- lapply(paste0(
      "<b>", karta_data$lansnamn, "</b><br>",
      ifelse(is.na(karta_data$antal_besok), "Inga besök registrerade",
            paste0(formatera_tal(karta_data$antal_besok), " besök"))
    ), HTML)

    proxy <- leafletProxy("karta_digidel", data = karta_data) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        layerId     = ~lanskod,
        fillColor   = ~ifelse(is.na(antal_besok), "#f1f1f1", pal(antal_besok)),
        fillOpacity = 0.75,
        color       = "#555555",
        weight      = 0.7,
        label       = etiketter,
        highlightOptions = highlightOptions(weight = 2, color = "#444444", bringToFront = FALSE)
      ) |>
      addLegend(
        "bottomleft", pal = pal, values = karta_data$antal_besok,
        title    = "Antal besök",
        labFormat = labelFormat(big.mark = " "),
        className = "info legend kompakt-legend"
      ) |>
      addControl(
        HTML(paste0(
          urvalstext(),
          if (digidel_exempeldata) "<br><b>Exempeldata</b>" else ""
        )),
        position = "topright", className = "map-filter-text"
      ) |>
      addControl(
        HTML("<i class='fa fa-hand-pointer'></i> Klicka på ett län"),
        position = "bottomright", className = "map-klick-hint"
      )

    # Valt län behåller sin färg och markeras med en tjock mörk kontur med vit kant runt,
    # så att markeringen inte kan förväxlas med kartans färgskala. Konturen tar inte emot
    # klick, så ett nytt klick på länet når ytan under och avmarkerar.
    vald_sf <- karta_data[karta_data$lanskod %in% valt_lan(), ]
    if (nrow(vald_sf) > 0) {
      proxy |>
        addPolygons(data = vald_sf, fill = FALSE, color = "white", weight = 8, opacity = 1,
                    options = pathOptions(interactive = FALSE)) |>
        addPolygons(data = vald_sf, fill = FALSE, color = farg_vald_lan, weight = 4, opacity = 1,
                    options = pathOptions(interactive = FALSE),
                    label = vald_sf$lansnamn,
                    labelOptions = labelOptions(noHide = TRUE, direction = "top", className = "vald-etikett"))
    }
  })

  observeEvent(input$karta_digidel_shape_click, {
    kod <- input$karta_digidel_shape_click$id
    req(kod)
    valt_lan(if (identical(valt_lan(), kod)) NULL else kod)
  })

  observeEvent(input$reset_karta, valt_lan(NULL))

  # ---- Urval: hela riket eller ett valt län ----

  urval <- reactive({
    if (is.null(valt_lan())) digidel_data else dplyr::filter(digidel_data, lanskod == valt_lan())
  })

  urval_besok <- reactive({
    if (is.null(valt_lan())) digidel_besok else dplyr::filter(digidel_besok, lanskod == valt_lan())
  })

  urvalstext <- reactive({
    if (is.null(valt_lan())) "Hela riket" else digidel_alla_lan$lansnamn[digidel_alla_lan$lanskod == valt_lan()]
  })

  # ---- Nyckeltal ----

  output$nyckeltal <- renderUI({
    besok <- urval_besok()
    tid <- mean(besok$tid_min, na.rm = TRUE)

    tredje_varde <- if (is.null(valt_lan())) {
      paste0(dplyr::n_distinct(besok$lanskod), " av ", nrow(digidel_alla_lan))
    } else {
      paste0(formatera_tal(nrow(besok) / nrow(digidel_besok) * 100), " %")
    }
    tredje_etikett <- if (is.null(valt_lan())) "Län med besök" else "Andel av alla besök"

    div(class = "nyckeltal",
      div(class = "nyckeltal-ruta",
          div(class = "nyckeltal-varde", formatera_tal(nrow(besok))),
          div(class = "nyckeltal-etikett", paste0("Besök, ", urvalstext()))),
      div(class = "nyckeltal-ruta",
          div(class = "nyckeltal-varde", if (is.na(tid)) "–" else paste0(formatera_tal(tid), " min")),
          div(class = "nyckeltal-etikett", "Genomsnittlig tidsåtgång")),
      div(class = "nyckeltal-ruta",
          div(class = "nyckeltal-varde", tredje_varde),
          div(class = "nyckeltal-etikett", tredje_etikett))
    )
  })

  # ---- Svarsfördelning för vald fråga ----
  # Andelen räknas av antalet besök som fick frågan (inte antalet rader), så att frågor med
  # flera möjliga svar inte ger missvisande andelar.

  svar_data <- reactive({
    req(input$val_fraga)
    df <- dplyr::filter(urval(), fraga == input$val_fraga)
    bas <- dplyr::n_distinct(df$id)
    validate(need(bas > 0, "Inga svar på den här frågan i urvalet."))

    df |>
      dplyr::distinct(id, svar) |>
      dplyr::count(svar, name = "antal") |>
      dplyr::mutate(andel = antal / bas * 100, bas = bas) |>
      dplyr::arrange(dplyr::desc(antal))
  })

  output$diagram_svar <- renderGirafe({
    df <- svar_data()
    storlek <- diagram_storlek(session, "diagram_svar")

    df <- df |>
      dplyr::mutate(
        svar_vis = ifelse(is.na(svar), "Inget svar", svar),
        etikett  = paste0(svar_vis, "<br>", formatera_tal(antal), " besök (", formatera_tal(andel), " %)")
      )

    titel <- radbryt(paste0(input$val_fraga, " – ", urvalstext()), storlek$width)
    underrubrik <- paste0(
      "Andel av ", formatera_tal(df$bas[1]), " besök som fick frågan. Flera svar möjliga - ",
      "andelarna summerar inte nödvändigtvis till 100 %."
    )

    p <- ggplot(df, aes(x = reorder(svar_vis, antal), y = antal)) +
      geom_col_interactive(aes(tooltip = etikett, data_id = svar_vis), fill = "#158daf", width = 0.7) +
      coord_flip() +
      scale_y_continuous(labels = formatera_tal) +
      labs(x = NULL, y = "Antal besök",
           title = titel,
           subtitle = radbryt(underrubrik, storlek$width, storlek_pt = diagram_caption_storlek + 1, fet = FALSE),
           caption = KALLA_DIGIDEL) +
      tema_diagram()

    skapa_girafe(p, width = storlek$width, height = storlek$height)
  }) |>
    bindCache(input$val_fraga, valt_lan(), utdata_storlek(session, "diagram_svar"), cache = "app")

  output$tabell_text <- renderText({
    req(input$val_fraga)
    paste0("Svarsfördelning: ", input$val_fraga)
  })

  output$tabell_svar <- renderDT({
    df <- svar_data() |>
      dplyr::transmute(
        Svar        = ifelse(is.na(svar), "Inget svar", svar),
        Antal       = antal,
        `Andel (%)` = round(andel, 1)
      )
    datatable(df, rownames = FALSE, options = list(pageLength = 10, dom = "tip"))
  })
})
