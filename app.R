
# Analizador de bonos  (AL29/AL30/AL35/AL41/GD29/GD30/GD35/GD41)
# - Explora cotizaciones históricas
# - Simula una inversión entre fechas (compra/venta)
# - Calcula pagos (cupón + amortización), valor residual y riqueza acumulada


library(shiny)
library(readxl)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(scales)
library(plotly)
library(shinydashboard)

# FUNCIONES

# convierte texto numérico a numeric (separa valores de miles con coma) 
a_numero <- function(x) {
  x <- str_trim(x)
  x <- str_replace_all(x, ",", "")
  suppressWarnings(as.numeric(x))
}

# lee Excel de cotizaciones y estandariza columnas/formatos
# - Detecta formato de fecha (MDY o DMY)
# - Devuelve columnas: fecha (Date), cierre (numeric), bono (factor/char)

leer_precios <- function(ruta, bono) {
  datos <- read_excel(ruta, skip = 2, col_types = "text")
  datos <- dplyr::rename(datos, fecha = `Fecha Cotización`, cierre = Cierre)
  
  # Parseo de fecha: intenta mdy, si no, dmy
  fechas <- suppressWarnings(mdy(datos$fecha))
  if (sum(!is.na(fechas)) < length(datos$fecha) / 2) {
    fechas <- suppressWarnings(dmy(datos$fecha))
  }
  
  datos <- dplyr::mutate(datos,
                         fecha = fechas,
                         cierre = a_numero(cierre),
                         bono = bono)
  datos <- dplyr::filter(datos, !is.na(fecha), !is.na(cierre))
  return(datos)
}

#lee Excel de flujos y estandariza nombres/tipos
# - Columnas esperadas (insensible a tildes/espacios/punt.): 
#   fecha, amortizacion, cupon, flujototal, saldoinicial, saldofinal
# - Devuelve: fecha, amort, cupon, flujo_total, saldo_inicial, saldo_final
#   (numéricos y Date en 'fecha')
leer_flujos <- function(ruta) {
  datos <- read_excel(ruta)
  nombres <- str_to_lower(names(datos))
  nombres <- str_replace_all(nombres, "[[:punct:] ]", "")
  names(datos) <- nombres
  
  datos <- dplyr::rename(datos,
                         fecha = fecha,
                         amortizacion = amortizacion,
                         cupon = cupon,
                         flujo_total = flujototal,
                         saldo_inicial = saldoinicial,
                         saldo_final = saldofinal)
  
  datos <- dplyr::mutate(datos,
                         fecha = as.Date(fecha),
                         amortizacion = as.numeric(amortizacion),
                         cupon = as.numeric(cupon),
                         flujo_total = as.numeric(flujo_total),
                         saldo_inicial = as.numeric(saldo_inicial),
                         saldo_final = as.numeric(saldo_final))
  return(datos)
}

# obtiene el último precio disponible
precio_en_fecha <- function(datos, f) {
  feas <- datos$fecha[datos$fecha <= f]
  if (!length(feas)) return(list(fecha = NA, precio = NA))
  f_sel <- max(feas, na.rm = TRUE)
  p_sel <- datos$cierre[datos$fecha == f_sel][1]
  list(fecha = f_sel, precio = p_sel)
}

# SE CARGAN LOS DATOS
cot_al29 <- leer_precios("data/Cotizaciones/AL29_Cotizaciones_Historicas.xlsx", "AL29")
cot_gd29 <- leer_precios("data/Cotizaciones/GD29_Cotizaciones_Historicas.xlsx", "GD29")
cot_al30 <- leer_precios("data/Cotizaciones/AL30_Cotizaciones_Historicas.xlsx", "AL30")
cot_gd30 <- leer_precios("data/Cotizaciones/GD30_Cotizaciones_Historicas.xlsx", "GD30")
cot_al35 <- leer_precios("data/Cotizaciones/AL35_Cotizaciones_Historicas.xlsx", "AL35")
cot_gd35 <- leer_precios("data/Cotizaciones/GD35_Cotizaciones_Historicas.xlsx", "GD35")
cot_al41 <- leer_precios("data/Cotizaciones/AL41_Cotizaciones_Historicas.xlsx", "AL41")
cot_gd41 <- leer_precios("data/Cotizaciones/GD41_Cotizaciones_Historicas.xlsx", "GD41")

# Unifica todas las cotizaciones en un solo data frame
precios <- dplyr::bind_rows(cot_al29, cot_gd29,
                            cot_al30, cot_gd30,
                            cot_al35, cot_gd35,
                            cot_al41, cot_gd41)
# lista de flujos por vencimiento (clave "29", "30", "35", "41")
flujos_lista <- list(
  "29" = leer_flujos("data/Flujos/Flujos_29.xlsx"),
  "30" = leer_flujos("data/Flujos/Flujos_30.xlsx"),
  "35" = leer_flujos("data/Flujos/Flujos_35.xlsx"),
  "41" = leer_flujos("data/Flujos/Flujos_41.xlsx")
)

# Bonos disponibles y límites de fechas para inputs
bonos <- unique(precios$bono)
fmin <- min(precios$fecha, na.rm = TRUE)
fmax <- max(precios$fecha, na.rm = TRUE)

# UI
# Dos pestañas: Análisis histórico y Calculadora de bonos
ui <- dashboardPage(
  dashboardHeader(title = "Analizador de Bonos", disable = TRUE),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      tabBox(width = 12,
             # Pestaña 1: exploración histórica
             tabPanel("Análisis histórico",
                      selectizeInput("bonos_hist", "Bonos:", choices = bonos,
                                     selected = bonos[1:2], multiple = TRUE),
                      dateRangeInput("rango_hist", "Rango de fechas:",
                                     start = fmin, end = fmax, min = fmin, max = fmax),
                      plotlyOutput("grafico_hist", height = "350px"),
                      plotlyOutput("grafico_rend", height = "250px")
             ),
             # Pestaña 2: simulación de inversión
             tabPanel("Calculadora de bonos",
                      fluidRow(
                        box(width = 4,
                            # Entradas clave de la simulación
                            selectInput("bono_sim", "Bono:", choices = bonos),
                            dateInput("f_compra", "Fecha compra:", value = fmin),
                            dateInput("f_venta", "Fecha venta:", value = fmax),
                            numericInput("monto", "Monto invertido (ARS):", 100000, min = 10000),
                            # Tipo de cambio fijo para convertir pagos USD→ARS
                            numericInput("tc_fijo", "Tipo de cambio (ARS/USD):", value = 1000, min = 0),
                            actionButton("calc", "Calcular")
                        ),
                        box(width = 8,
                            plotlyOutput("grafico_sim", height = "300px"),
                            h4("Evolución acumulada (Precio + Pagos)"),
                            plotlyOutput("grafico_acumulado", height = "250px"),
                            h4("Pagos efectivamente cobrados"),
                            tableOutput("tabla_pagos"),
                            h4("Cronograma completo del bono"),
                            tableOutput("tabla_pagos_completo"),
                            h4("Resultado"),
                            fluidRow(
                              infoBoxOutput("precio_compra", width = 4),
                              infoBoxOutput("precio_venta", width = 4),
                              infoBoxOutput("valor_final", width = 4),
                              infoBoxOutput("ganancia", width = 4),
                              infoBoxOutput("rendimiento", width = 4)
                            )
                        )
                      )
             )
      )
    )
  ),
  skin = "blue"
)

# SERVER
server <- function(input, output, session) {
  
  #COTIZACIONES
 # filtra cotizaciones según bonos y rango de fechas elegidos
    
  datos_hist <- reactive({
    dplyr::filter(precios,
                  bono %in% input$bonos_hist,
                  fecha >= input$rango_hist[1],
                  fecha <= input$rango_hist[2])
  })
  # Gráfico de precios históricos por bono
  output$grafico_hist <- renderPlotly({
    datos <- datos_hist()
    gg <- ggplot(datos, aes(x = fecha, y = cierre, color = bono)) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(x = "Fecha", y = "Precio (ARS por 100 VN)")
    ggplotly(gg, tooltip = c("fecha","cierre","bono"))
  })
  # Gráfico de rendimientos (variación porcentual diaria)
  output$grafico_rend <- renderPlotly({
    datos <- datos_hist()
    datos <- dplyr::group_by(datos, bono)
    datos <- dplyr::mutate(datos, rendimiento = cierre / dplyr::lag(cierre) - 1)
    datos <- tidyr::drop_na(datos, rendimiento)
    gg <- ggplot(datos, aes(x = fecha, y = rendimiento, color = bono)) +
      geom_line() + theme_minimal() +
      scale_y_continuous(labels = scales::percent)
    ggplotly(gg, tooltip = c("fecha","rendimiento","bono"))
  })
  
  # helper precio al último hábil 
  precio_prev <- function(datos, fechas) {
    datos_orden <- arrange(datos, fecha)
    datos_sel <- select(datos_orden, fecha, cierre)
    ix <- findInterval(as.numeric(fechas), as.numeric(datos_sel$fecha))
    out <- rep(NA_real_, length(ix))
    ok <- ix > 0
    out[ok] <- datos_sel$cierre[ix[ok]]
    out
  }
  
  # CALCULADORA
  # dispara al presionar "Calcular" y arma todo el output de simulación
  resultado <- eventReactive(input$calc, {
    # Bono elegido y sus cotizaciones
    bono_sel <- input$bono_sim
    datos_bono <- dplyr::filter(precios, bono == bono_sel)
    
    # Obtiene flujos según vencimiento (clave "29","30","35","41")
    venc <- str_extract(bono_sel, "\\d+")
    flujos <- flujos_lista[[venc]]
    
    # Precio de compra/venta 
    pc <- precio_en_fecha(datos_bono, as.Date(input$f_compra))
    pv <- precio_en_fecha(datos_bono, as.Date(input$f_venta))
    req(!is.na(pc$precio), !is.na(pv$precio))
    
    # Valor Nominal comprado (precio cotiza por 100 VN)
    # VN = (monto en ARS / precio_compra) * 100
    VN <- (input$monto / pc$precio) * 100
    
    datos_ordenados <- arrange(datos_bono, fecha)
    
    # Pagos completos desde la compra 
    # flujo_total (USD por 100 VN) → escalar al VN comprado → convertir a ARS con tc_fijo
    pagos_base <- dplyr::filter(flujos, fecha > pc$fecha)
    pagos_base <- arrange(pagos_base, fecha)
    pagos_base <- dplyr::mutate(pagos_base,
                                pago_usd = flujo_total * (VN / 100),
                                pago_ars = pago_usd * input$tc_fijo,
                                pagos_acum = cumsum(pago_ars))
    # Precio (ARS por 100 VN) a usar en cada fecha de flujo
    cierre_pago <- precio_prev(datos_ordenados, pagos_base$fecha)
    
    # saldo_final_real: VN residual en unidades reales
    # valor_residual: valuación de ese VN residual al precio de la fecha de flujo
    # saldo_total: riqueza total = cash cobrado (pagos_acum) + valor_residual
    pagos_completos <- dplyr::mutate(pagos_base,
                                     saldo_final_real = (saldo_final / 100) * VN,
                                     valor_residual = (saldo_final_real / 100) * cierre_pago,
                                     saldo_total = pagos_acum + valor_residual)
    
    # VALOR DE VENTA
    
    # Se vende SOLO el VN residual a la fecha de venta
    ult <- dplyr::filter(flujos, fecha <= pv$fecha)
    ult <- arrange(ult, fecha)
    if (nrow(ult) > 0) {
      residuo_venta <- tail(ult, 1)$saldo_final
    } else {
      residuo_venta <- 100
    }
    VN_residual_venta <- (residuo_venta / 100) * VN
    valor_venta <- (VN_residual_venta / 100) * pv$precio
    
    # Pagos  cobrados hasta la fecha de venta
    pagos_cobrados <- dplyr::filter(pagos_completos, fecha <= pv$fecha)
    pagos_total <- sum(pagos_cobrados$pago_ars, na.rm = TRUE)
    
    #RESULTADO DE LA INVERSION
    valor_final <- valor_venta + pagos_total
    ganancia <- valor_final - input$monto
    rendimiento <- ifelse(input$monto > 0, ganancia / input$monto * 100, NA_real_)
    
    # GANANCIA 
    # Combina precio diario (sobre VN residual diario) + cash acumulado
    precios_ganancias <- dplyr::filter(datos_bono,
                                       fecha >= pc$fecha,
                                       fecha <= pv$fecha)
    precios_ganancias <- arrange(precios_ganancias, fecha)
    
    res_tbl <- arrange(flujos, fecha)
    res_tbl <- select(res_tbl, fecha, saldo_final)
    ix2 <- findInterval(as.numeric(precios_ganancias$fecha), as.numeric(res_tbl$fecha))
    saldo_pct_diario <- ifelse(ix2 == 0, 100, res_tbl$saldo_final[ix2])
    VN_diario <- (saldo_pct_diario / 100) * VN
    
    # riqueza = valor_precio (precio * VN_residual_diario) + cash acumulado por cupones
    riqueza_diaria <- dplyr::mutate(precios_ganancias,
                                    valor_precio = (VN_diario / 100) * cierre)
    pagos_sel <- select(pagos_cobrados, fecha, pago_ars)
    riqueza_diaria <- dplyr::left_join(riqueza_diaria, pagos_sel, by = "fecha")
    riqueza_diaria <- dplyr::mutate(riqueza_diaria,
                                    pago_ars = ifelse(is.na(pago_ars), 0, pago_ars),
                                    cash_acum = cumsum(pago_ars),
                                    riqueza = valor_precio + cash_acum)
    
    # Devuelve todos los objetos necesarios para los outputs
    list(
      pagos = pagos_cobrados,
      pagos_completos = pagos_completos,
      valor_final = valor_final,
      ganancia = ganancia,
      rendimiento = rendimiento,
      precio_compra = pc$precio,
      precio_venta = pv$precio,
      datos_bono = datos_bono,
      riqueza_diaria = riqueza_diaria
    )
  })
  
  # Gráfico de precio del bono elegido, con líneas de compra/venta
  output$grafico_sim <- renderPlotly({
    req(resultado())
    datos <- dplyr::filter(resultado()$datos_bono, bono == input$bono_sim)
    gg <- ggplot(datos, aes(x = fecha, y = cierre)) +
      geom_line(color = "blue") +
      geom_vline(xintercept = as.numeric(as.Date(input$f_compra)), linetype = "dashed") +
      geom_vline(xintercept = as.numeric(as.Date(input$f_venta)), linetype = "dashed") +
      theme_minimal()
    ggplotly(gg, tooltip = c("fecha","cierre"))
  })
  
  # Gráfico de saldo acumulada: precio*(VN residual) + pagos cobrados
  output$grafico_acumulado <- renderPlotly({
    req(resultado()$riqueza_diaria)
    gg <- ggplot(resultado()$riqueza_diaria, aes(x = fecha, y = riqueza)) +
      geom_line(color = "darkgreen", size = 1) +
      theme_minimal() +
      labs(x = "Fecha", y = "Valor cartera (ARS)",
           title = "Evolución: precio (sobre VN residual) + cupones cobrados")
    ggplotly(gg, tooltip = c("fecha","riqueza"))
  })
  
  # Tabla de pagos efectivamente cobrados (hasta fecha de venta)
  output$tabla_pagos <- renderTable({
    datos <- resultado()$pagos
    datos <- dplyr::mutate(datos,
                           Fecha = format(fecha, "%d/%m/%Y"),
                           PagoARS = round(pago_ars, 2),
                           Acumulado = round(pagos_acum, 2))
    dplyr::select(datos, Fecha, Amortizacion = amortizacion,
                  Cupon = cupon, `Pago ARS` = PagoARS, `Acumulado $` = Acumulado)
  })
  
  # Cronograma completo desde la compra: pagos por fecha + valuación residual
  output$tabla_pagos_completo <- renderTable({
    datos <- resultado()$pagos_completos
    datos <- dplyr::mutate(datos,
                           Fecha = format(fecha, "%d/%m/%Y"),
                           `Pago ARS` = round(pago_ars, 2),
                           `Pagos acumulados` = round(pagos_acum, 2),
                           `Saldo acumulado` = round(saldo_total, 2))
    dplyr::select(datos, Fecha, Amortizacion = amortizacion,
                  Cupon = cupon, `Flujo %` = flujo_total,
                  `Pago ARS`, `Pagos acumulados`, `Saldo acumulado`)
  })
  
  # resultados clave de la simulación
  output$precio_compra <- renderInfoBox({
    infoBox("Precio compra", paste0("$", round(resultado()$precio_compra,2)),
            icon = icon("shopping-cart"), color = "orange")
  })
  
  output$precio_venta <- renderInfoBox({
    infoBox("Precio venta", paste0("$", round(resultado()$precio_venta,2)),
            icon = icon("tags"), color = "yellow")
  })
  
  output$valor_final <- renderInfoBox({
    infoBox("Valor final", paste0("$", round(resultado()$valor_final,2), " ARS"),
            icon = icon("dollar"), color = "green")
  })
  
  output$ganancia <- renderInfoBox({
    infoBox("Ganancia", round(resultado()$ganancia,2),
            icon = icon("chart-line"), color = "blue")
  })
  
  output$rendimiento <- renderInfoBox({
    infoBox("Rendimiento %", paste0(round(resultado()$rendimiento,2),"%"),
            icon = icon("percent"), color = "purple")
  })
}

shinyApp(ui, server)
