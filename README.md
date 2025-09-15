# Analizador de Bonos – ShinyApp (AL29/AL30/AL35/AL41/GD29/GD30/GD35/GD41)

Este repositorio contiene una **aplicación Shiny** que:
- Explora **cotizaciones históricas** de bonos soberanos ARS (*ALxx*, *GDxx*).
- **Simula una inversión** entre fechas (compra/venta).
- Calcula **pagos** (cupón + amortización), **valor residual** y **riqueza acumulada**.


---

## 📂 Estructura del repo

```
/ REPOSITORIO
├─ app.R
├─ README.md
└─ data/
   ├─ Cotizaciones/
   │  ├─ AL29_Cotizaciones_Historicas.xlsx
   │  ├─ AL30_Cotizaciones_Historicas.xlsx
   │  ├─ AL35_Cotizaciones_Historicas.xlsx
   │  ├─ AL41_Cotizaciones_Historicas.xlsx
   │  ├─ GD29_Cotizaciones_Historicas.xlsx
   │  ├─ GD30_Cotizaciones_Historicas.xlsx
   │  ├─ GD35_Cotizaciones_Historicas.xlsx
   │  └─ GD41_Cotizaciones_Historicas.xlsx
   └─ Flujos/
      ├─ Flujos_29.xlsx
      ├─ Flujos_30.xlsx
      ├─ Flujos_35.xlsx
      └─ Flujos_41.xlsx
```

- **`app.R`**: código completo de la ShinyApp.
- **`data/Cotizaciones/*.xlsx`**: datos históricos descargados de InvertirOnline.
- **`data/Flujos/*.xlsx`**: cronogramas de pagos por vencimiento (29/30/35/41).


---

## 🔗 Fuente de cotizaciones – InvertirOnline (IOL)

La app utiliza archivos Excel exportados desde IOL. Página de ejemplo (GD41):
```
https://iol.invertironline.com/titulo/datoshistoricos?mercado=bcba&simbolo=GD41
```
Para otro bono, **reemplazar `GD41` por el símbolo deseado** (p. ej. `AL30`, `GD30`, etc.).  
Desde esa página se descarga el Excel de cotizaciones históricas y se guarda en `data/Cotizaciones/` con el nombre propuesto arriba.

---

## 🧩 Paquetes utilizados

```r
shiny, readxl, dplyr, ggplot2, lubridate, stringr, scales, plotly, shinydashboard
```

Instalación rápida:
```r
install.packages(c(
  "shiny","readxl","dplyr","ggplot2",
  "lubridate","stringr","scales","plotly","shinydashboard"
))
```

---

## ▶️ Cómo ejecutar

### Local
```r
shiny::runApp()   # estando en la raíz del repo
```

### Desde GitHub (evaluación docente)
```r
shiny::runGitHub("trabajo-final-sebafebre", "myvdd-2025-unr")
```

---

## 🧠 Lógica y flujo del `app.R` (resumen técnico)

1. **Funciones auxiliares**
   - `a_numero(x)`: convierte texto a numérico eliminando comas de miles.
   - `leer_precios(ruta, bono)`: lee y estandariza Excel de IOL (detecta fechas **MDY** o **DMY**; columnas clave `Fecha Cotización` y `Cierre`; crea columnas `fecha`, `cierre`, `bono`).
   - `leer_flujos(ruta)`: lee y estandariza cronogramas de pagos (renombra y tipa: `fecha`, `amortizacion`, `cupon`, `flujo_total`, `saldo_inicial`, `saldo_final`).
   - `precio_en_fecha(datos, f)`: busca el **último precio disponible** a una fecha dada.
   - `precio_prev(datos, fechas)`: helper para obtener el **precio vigente** en cada fecha de pago.

2. **Carga de datos**
   - Une las cotizaciones de **AL29/AL30/AL35/AL41** y **GD29/GD30/GD35/GD41**.
   - Crea `flujos_lista` con los cronogramas para vencimientos **29/30/35/41**.

3. **UI (`shinydashboard`)**
   - **Análisis histórico**: selecciona bonos + rango de fechas → gráficos de **precio** y **rendimiento diario**.
   - **Calculadora de bonos**: simula compra/venta, **tipo de cambio fijo** (ARS/USD) para convertir pagos USD→ARS, y muestra:
     - Gráfico de precio con marcas de compra/venta.
     - Gráfico de **evolución de riqueza** (precio sobre VN residual + cupones cobrados).
     - **Pagos efectivamente cobrados** (hasta venta) y **cronograma completo**.
     - **KPIs**: precio compra, precio venta, valor final, ganancia, rendimiento %.

4. **Cálculos clave (simulación)**
   - Precio de compra/venta (ARS por **100 VN**).
   - **VN comprado**: `VN = (monto_ARS / precio_compra) * 100`.
   - **Pagos** (`flujo_total` en USD por 100 VN) → escala por VN comprado → convierte a ARS con `tc_fijo`.
   - **VN residual** según `saldo_final` del flujo → **valor residual**: `(VN_residual/100) * precio`.
   - **Venta**: se vende solo el **VN residual** al precio de venta.
   - **Riqueza diaria**: valor del precio (sobre VN residual diario) + **cash acumulado** por cupones.

---

## 📑 Estructura de archivos de entrada y diccionario de variables

### 1) Cotizaciones (Excel de IOL)

- **Ubicación:** `data/Cotizaciones/*.xlsx`  
- **Lectura en el código:** `read_excel(..., skip = 2, col_types = "text")`  
- **Columnas que la app usa:** `Fecha Cotización` (→ `fecha`), `Cierre` (→ `cierre`).  
- **Otras columnas frecuentes** (pueden venir en el Excel y no son requeridas por la app, pero se listan para referencia):

| Columna original (IOL) | Descripción | Tipo esperado |
|---|---|---|
| Fecha Cotización | Día de la operación | Fecha (MDY o DMY) |
| Apertura | Precio de apertura | Numérica |
| Máximo | Precio máximo del día | Numérica |
| Mínimo | Precio mínimo del día | Numérica |
| **Cierre** | **Precio de cierre (ARS por 100 VN)** | **Numérica** |
| Cierre ajustado | Precio ajustado | Numérica |
| Volumen Monto | Monto negociado | Numérica |
| Volumen Nominal | Cantidad nominal negociada | Numérica/Entera |

> **Notas de limpieza**:  
> - Las celdas se leen como *texto* y se normalizan con `a_numero()` para tolerar separadores de miles con coma.  
> - La fecha se **auto-detecta**: intenta `mdy()` y, si menos del 50% parsea, intenta `dmy()`.

**Diccionario de variables derivadas (cotizaciones unificadas):**
| Variable | Descripción | Tipo |
|---|---|---|
| `fecha` | Fecha de cotización | `Date` |
| `cierre` | Precio de cierre (ARS por 100 VN) | `numeric` |
| `bono` | Identificador del bono (AL29, GD30, etc.) | `character/factor` |

---

### 2) Flujos (cronogramas de pagos por vencimiento)

- **Ubicación:** `data/Flujos/Flujos_29.xlsx`, `Flujos_30.xlsx`, `Flujos_35.xlsx`, `Flujos_41.xlsx`  
- **Lectura en el código:** `read_excel(ruta)` y **estandarización de nombres** a minúsculas sin tildes ni signos (`fecha`, `amortizacion`, `cupon`, `flujototal`, `saldoinicial`, `saldofinal`).  
- **Unidades**: los montos monetarios están **en USD por 100 VN**; los saldos son **porcentaje del VN** (0–100).

| Columna normalizada | Significado | Tipo | Unidad |
|---|---|---|---|
| `fecha` | Fecha del pago | `Date` | – |
| `amortizacion` | Pago de amortización por 100 VN | `numeric` | **USD** |
| `cupon` | Pago de cupón por 100 VN | `numeric` | **USD** |
| `flujo_total` | Suma amortización + cupón por 100 VN | `numeric` | **USD** |
| `saldo_inicial` | Saldo vivo antes del pago | `numeric` | **% del VN** |
| `saldo_final` | Saldo vivo luego del pago | `numeric` | **% del VN** |

> La app convierte `flujo_total` **USD → ARS** usando un **tipo de cambio fijo** definido por el usuario (`tc_fijo`).

---

## 📈 Variables y resultados de la simulación

| Variable | Descripción | Fórmula / Origen | Unidad |
|---|---|---|---|
| `VN` | Valor nominal comprado | `(monto_ARS / precio_compra) * 100` | VN |
| `pago_usd` | Pago escalado a VN | `flujo_total * (VN / 100)` | USD |
| `pago_ars` | Pago convertido a ARS | `pago_usd * tc_fijo` | ARS |
| `pagos_acum` | Cash acumulado | `cumsum(pago_ars)` | ARS |
| `saldo_final_real` | VN residual en unidades reales | `(saldo_final / 100) * VN` | VN |
| `valor_residual` | Valuación del VN residual | `(saldo_final_real / 100) * precio` | ARS |
| `saldo_total` | Riqueza total en fecha de pago | `pagos_acum + valor_residual` | ARS |
| `valor_venta` | Valuación al vender | `(VN_residual_venta/100) * precio_venta` | ARS |
| `valor_final` | Resultado total | `valor_venta + sum(pagos hasta venta)` | ARS |
| `ganancia` | Ganancia/pérdida | `valor_final - monto_ARS` | ARS |
| `rendimiento` | % de rendimiento | `ganancia / monto_ARS * 100` | % |

---

## 🧪 Gráficos y tablas que genera la app

- **Análisis histórico**  
  - Línea de **precios** por bono.  
  - Línea de **rendimientos diarios** por bono.  
- **Calculadora**  
  - Línea de precio del bono con **marcas de compra/venta**.  
  - Línea de **evolución de riqueza** (precio sobre VN residual + cupones).  
  - **Tabla de pagos cobrados** (hasta venta).  
  - **Cronograma completo** (pagos + valuación residual).  
  - **KPIs**: precio compra/venta, valor final, ganancia y rendimiento %.  

---
