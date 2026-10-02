source('global.R')

shinyUI(
  fluidPage(
    tags$head(
      tags$title(APP_TITEL),
      tags$link(rel = 'icon', type = 'image/x-icon', href = 'favicon.ico'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'regiondalarna_ruf.css'),
      tags$link(rel = 'stylesheet', type = 'text/css', href = paste0('app.css?v=', as.integer(file.mtime('www/app.css')))),
      rdshinyappar::telemetri_ui(telemetry)
    ),

    useShinyjs(),

    # ---- Header (matchar .rd-header i regiondalarna_ruf.css) --------------
    tags$div(
      class = 'rd-header',
      tags$div(class = 'rd-header__title', APP_TITEL),
      tags$a(
        class  = 'rd-header__right',
        href   = 'https://www.regiondalarna.se',
        target = '_blank',
        tags$img(src = 'logo_liggande_fri_vit.png', alt = 'Region Dalarna')
      )
    ),

    # ---- Innehåll ---------------------------------------------------------
    tabsetPanel(
      id = 'flikval',
      tabPanel('Karta och diagram',
        div(class = "karta-layout",
          uiOutput("nyckeltal"),
          fluidRow(
            # Vänster: kartan + nedladdningsknappar
            column(
              width = 4,
              leafletOutput("karta_digidel", height = "70vh"),
              div(class = "karta-knapp",
                  downloadButton("export_excel", "Hela datasetet", icon = icon("download")),
                  downloadButton("export_excel_urval", "Aktuellt urval", icon = icon("download"))
              )
            ),
            # Höger: frågeval överst, stapeldiagram och tabell under
            column(
              width = 8,
              div(class = "diagram-toolbar",
                  div(class = "val-fraga", selectInput("val_fraga", "Fråga", choices = NULL))
              ),
              div(class = "diagram-cell",
                  girafeOutput("diagram_svar", width = "100%", height = "100%")
              ),
              div(class = "tabell-rubrik", textOutput("tabell_text")),
              DTOutput("tabell_svar")
            )
          )
        )
      ),
      tabPanel('Om',
        div(class = "om-text",
          h3("Om appen"),
          p("Appen visar resultat från Digidel-enkäten, som besvaras av besökare som fått hjälp med en ",
            "digital fråga på biblioteket. Varje besök kan ge upphov till flera rader i underlaget, ",
            "eftersom flera frågor (t.ex. Fråga 1) tillåter mer än ett svar per besök."),
          p("Kartan visar antal registrerade besök per län. Klicka på ett län för att filtrera ",
            "diagrammet och tabellen till det länet - klicka igen för att återgå till hela riket."),
          p("Andelarna i diagrammet och tabellen räknas av antalet besök som fått den valda frågan, ",
            "inte av antalet rader - för frågor med flera möjliga svar summerar andelarna därför inte ",
            "nödvändigtvis till 100 %."),
          p(KALLA_DIGIDEL),
          if (digidel_exempeldata) p(strong("OBS: appen visar just nu slumpade exempeldata, inte riktiga enkätsvar."))
        )
      )
    ),

    # ---- Footer (matchar .rd-footer i regiondalarna_ruf.css) --------------
    tags$div(
      class = 'rd-footer',
      'Samhällsanalys, Region Dalarna · ',
      tags$a(
        href = 'mailto:samhallsanalys@regiondalarna.se',
        'samhallsanalys@regiondalarna.se'
      )
    )
  )
)
