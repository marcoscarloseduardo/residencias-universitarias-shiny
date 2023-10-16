
# 0. LIBRARY
#--------------------------------------------------------
libs <- c("shiny", "shinythemes", "readxl", "tmaptools", "stringi", "dplyr", "ggplot2", "plotly", "DT", "leaflet")

installed_libs <- libs %in% rownames(installed.packages())

if (any(installed_libs == FALSE)) {
  install.packages(libs[!installed_libs])
}

invisible(lapply(libs, library, character.only = TRUE))



# 1. Load Data
#--------------------------------------------------------

resi <- read_xlsx("data/resi.xlsx", na = "Nc")

resi$periodo_precio <- format(resi$periodo_precio, "%b%Y")

# Geocodificación de direcciones
latitudes <- numeric(length(resi$ubicacion))
longitudes <- numeric(length(resi$ubicacion))

#for (i in 1:length(resi$ubicacion)) {
#    geo_coded <-
#        geocode_OSM(stri_trans_general(
#            paste0(resi$ubicacion[i], ", Cordoba capital, provincia de cordoba,  Argentina"),
#            "Latin-ASCII"
#        ))
#    longitudes[i] <- geo_coded$coords[1]
#    latitudes[i] <- geo_coded$coords[2]
#}

resi$lat <- latitudes
resi$lon <- longitudes

resi <- resi |> mutate(id = seq.int(nrow(resi)))
                       
#saveRDS(resi, file = "data/resi.RDS")
resi <- readRDS(file = "data/resi.RDS")


# 3. Wrangling and Transformation data
#--------------------------------------------------------

# Crear tabla de residencias

img_uri <- function(x, titulo = "") {
  sprintf(
    paste0('<img src="%s" width="24" height="24"  title="', titulo, '" />'),
    knitr::image_uri(x)
  )
}

img_contacto <- function(x, titulo = "", url = "") {
  img_tag <-
    sprintf('<img src="%s" width="24" height="24" title="%s" />',
            knitr::image_uri(x),
            titulo)
  if (url != "") {
    img_tag <-
      sprintf('<a href="%s" target="_blank">%s</a>', url, img_tag)
  }
  return(img_tag)
}


hab_cols <- grep("^hab_", names(resi), value = TRUE)

p_cols <- grep("^p_", names(resi), value = TRUE)

# Modificar valor sí | no por ícono
apply_conditional_logic <- function(column) {
  case_when(column == 1 ~ img_uri("icons/si.png", "Sí"),
            TRUE ~ img_uri("icons/no.png", "No"))
}

# Modificar valor del tipo de residencia
resi <- resi %>%
  mutate(
    tipo = case_when(
      tipo == "Mixta" ~ img_uri("icons/mixto.png", "Mixta"),
      tipo == "Femenina" ~ img_uri("icons/female.png", "Sólo Mujeres"),
      tipo == "Masculina" ~ img_uri("icons/male.png", "Sólo Varones"),
      TRUE ~ NA
    )
  )

# Aplicar la lógica condicional a todas las columnas relevantes
resi <-
  resi %>% mutate(across(all_of(hab_cols), apply_conditional_logic))

resi <-
  resi %>% mutate(across(all_of(p_cols), ~ ceiling(. / 1000)))

logo = c(
  img_uri("icons/residencia.png", "Residencia"),
  img_uri("icons/direccion.png", "Dirección"),
  img_uri("icons/1p.png", "Hab. individual"),
  img_uri("icons/2p.png", "Hab. doble"),
  img_uri("icons/3p.png", "Hab. triple"),
  img_uri("icons/4p.png", "Hab, cuádruple"),
  img_uri("icons/precio1.png", "Valor hab. individual en miles de pesos"),
  img_uri("icons/precio2.png", "Valor hab. doble en miles de pesos"),
  img_uri("icons/precio3.png", "Valor hab. triple en miles de pesos"),
  img_uri("icons/precio4.png", "Valor hab. cuádruple en miles de pesos"),
  img_uri("icons/inscripcion.png", "Valor inscripción en miles de pesos"),
  img_uri("icons/actualizacion.png", "Vigencia valores"),
  img_uri("icons/tipo.png", "Admisión Varones, Mujeres o Mixta")
)

filter_vars <-
  c("nombre",
    "ubicacion",
    hab_cols,
    p_cols,
    "periodo_precio",
    "tipo")


# 4. UI side
#--------------------------------------------------------

ui <- fluidPage(
  titlePanel("Explorador de Residencias Universitarias en Córdoba, Argentina"),
  
  # side panel
  mainPanel(
    leafletOutput("map_resi"),
    dataTableOutput("tbl_resi"),
    width = 9
  ),
  
  # main panel
  sidebarPanel(
    h4("Servicios y Contacto"),
    htmlOutput("serv"),
    hr(),
    htmlOutput("contacto"),
    width = 3
  )
  
)

# 5. server side
#--------------------------------------------------------

  server <- function(input, output) {
    # tabla completa de residencias
    output$tbl_resi <- renderDataTable({
      DT::datatable(
        data = resi[, filter_vars],
        colnames = logo,
        escape = FALSE,
        selection = "single",
        options = list(
          autoWidth = TRUE,
          pageLength = 100,
          list(stateSave = TRUE),
          initComplete = JS(
            "function(settings, json) {",
            "$(this.api().table().node()).css('font-size', '14px');",
            "}"
          )
        )
      ) |>
        formatCurrency(columns = p_cols,
                       currency = "$",
                       digits = 0)
    })
    
    # residencia previamente seleccionada
    prev_row <- reactiveVal()
    
        # icono de la residencia seleccionada
    my_icon = makeAwesomeIcon(
      icon = "home",
      markerColor = "tomato1",
      iconColor = "white"
    )
    
    
    
    observeEvent(input$tbl_resi_rows_selected, {
      # Obtiene los índices de las filas visibles en la tabla
      resi_seleccionada <- resi[input$tbl_resi_rows_selected, ]
      
      # resaltar marcador en el mapa en base a la residencia seleccionada en la tabla
      proxy <- leafletProxy("map_resi")
      
      proxy %>%
        addAwesomeMarkers(
          popup = as.character(resi_seleccionada$nombre),
          layerId = as.character(resi_seleccionada$id),
          lng = resi_seleccionada$lon,
          lat = resi_seleccionada$lat,
          icon = my_icon
        )
      
      # devolver estilo al marcador anteriormente modificado
      if (!is.null(prev_row()))
      {
        proxy %>%
          addMarkers(
            popup = prev_row()$nombre,
            layerId = as.character(prev_row()$id),
            lng = prev_row()$lon,
            lat = prev_row()$lat
          )
      }
      # guardar fila actualmente seleccionada
      prev_row(resi_seleccionada)
      
      ### actualizar servicios
      # función para agregar el ícono en función del valor en la residencia seleccionada
      generar_icono_servicio <- function(texto, valor) {
        if (is.na(valor)) {
          return(paste0(img_uri("icons/question.png", "Desconocido"), texto))
        } else if (valor == 1) {
          return(paste0(img_uri("icons/si.png", "Sí"), texto))
        } else {
          return(paste0(img_uri("icons/no.png", "No"), texto))
        }
      }
      
      # Crear un mapeo de nombres de servicios legibles a nombres de variables en la tabla
      servicios_legibles <- c("Wifi", "Heladera", "Cocina", "Microondas", "Ventilador en dormitorio", "Calefactor", 
                              "Aire Acondicionado", "Lavarropas", "TV por cable", "Sábanas", 
                              "Encargado 24 hrs.", "Sala de estudio", "Living", "Comedor", "Patio o Terraza", "Piscina", 
                              "Servicio de Emergencias")
      variables_servicios <- c("ser_wifi", "ser_heladera", "ser_cocina", "ser_microonda", "ser_ventilador", 
                               "ser_calefactor", "ser_aa", "ser_lava", "ser_tv_cable", "ser_sabanas",
                               "ser_encargado_24hrs", "ser_sala_estudio", "ser_sala_living", "ser_sala_comedor",
                               "ser_patio_terraza", "ser_piscina", "ser_emergencia")
      
      mapeo_servicios <- data.frame(ServicioLegible = servicios_legibles, Variable = variables_servicios)
      
      # Actualizar servicios
      output$serv <- renderText({
        if (is.null(resi_seleccionada) || nrow(resi_seleccionada) == 0) {
          servicios_texto <- mapeo_servicios$ServicioLegible
        } else {
          servicios_texto <- lapply(1:nrow(mapeo_servicios), function(i) {
            servicio <- mapeo_servicios$ServicioLegible[i]
            variable <- mapeo_servicios$Variable[i]
            icono_servicio <- generar_icono_servicio(servicio, resi_seleccionada[[variable]])
            paste(icono_servicio)
          })
        }
        
        servicios_html <- paste(servicios_texto, collapse = "<br>")
        
        HTML(servicios_html)  # Para que se interprete como HTML en la salida
      })
      
      ### actualizar info de contacto
      # mapeo contacto legible con variable en tabla
      contacto_legibles <- c("Sitio web", "Teléfono", "WhatsApp", "E-mail", "Facebook", "Instagram")
      variables_contacto <- c("info_web", "info_telefono", "info_whatsapp", "info_email", "info_facebook", "info_ig")
      icons_contacto <- c("website", "phone", "whatsapp", "email", "facebook", "instagram")
      
      mapeo_contacto <- data.frame(ContactoLegible = contacto_legibles,
                                   Variable = variables_contacto,
                                   Icono = icons_contacto)
      
      output$contacto <- renderText({
        contacto_texto <- lapply(1:nrow(mapeo_contacto), function(i) {
          contacto <- mapeo_contacto$ContactoLegible[i]
          variable <- mapeo_contacto$Variable[i]
          icono <- paste0("icons/",mapeo_contacto$Icono[i], ".png")
          
          valor_variable <- resi_seleccionada[[variable]]
          if (is.na(valor_variable)) {
            valor_variable <- ""
          }
          
          icono_contacto <- img_contacto(x = icono, titulo = valor_variable, url = valor_variable)
          paste0(icono_contacto, "<span> ", valor_variable,"</span>")
        })
        
        contacto_html <- paste(contacto_texto, collapse = "<br>")
        
        HTML(contacto_html)  # Para que se interprete como HTML en la salida
      })
      
    })
    
    # actualizar mapa en función de las residencias visibles en la tabla
    output$map_resi <- renderLeaflet({
      resi_visibles <- resi[input$tbl_resi_rows_current,]
      
      map_update <- leaflet(data = resi_visibles) %>%
        addTiles() %>%
        addMarkers(popup = ~ nombre,
                   layerId = as.character(resi_visibles$id))
      
      map_update
      
    })
    
    # seleccionar residencia en la tabla en función del marcador clickeado en el mapa
    observeEvent(input$map_resi_marker_click, {
      clickId <- input$map_resi_marker_click$id
      resi_visibles <- resi[input$tbl_resi_rows_current,]
      
      dataTableProxy("tbl_resi") %>%
        selectRows(which(resi_visibles$id == clickId)) %>%
        selectPage(which(input$tbl_resi_rows_all == clickId) %/% input$tbl_resi_state$length + 1)
    })
    
  }

  # 6. App
  #--------------------------------------------------------
shinyApp(ui = ui, server = server)