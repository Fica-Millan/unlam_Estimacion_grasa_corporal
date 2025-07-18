# 🧠 Estimación del Porcentaje de Grasa Corporal con Modelos de Regresión

Trabajo Práctico — Fundamentos de Captura de Datos  
Especialización en Ciencia de Datos – UNLaM  
Grupo 10 | Año 2025  

## 📌 Descripción

Este proyecto tiene como objetivo desarrollar modelos estadísticos que permitan **predecir el porcentaje de grasa corporal** a partir de **variables antropométricas**. Utilizando técnicas
de **regresión lineal simple y múltiple**, se busca evaluar qué medidas corporales tienen mayor poder predictivo sobre el porcentaje de grasa, con el fin de construir un modelo explicativo robusto y 
aplicable en contextos clínicos o poblacionales.

El análisis se basa en el dataset `obesidad25`, el cual contiene mediciones reales de 260 individuos, incluyendo variables como peso, altura, cuello, pecho, abdomen, cadera, muslo y rodilla.

---

## 🧪 Tecnologías utilizadas

Este proyecto fue desarrollado en **R** para análisis estadístico, visualización y modelado. A continuación se listan las librerías utilizadas:

#### Visualización de datos
![ggplot2](https://img.shields.io/badge/ggplot2-DC5F00?style=flat&logo=r&logoColor=white)
![ggcorrplot](https://img.shields.io/badge/ggcorrplot-DC5F00?style=flat&logo=r&logoColor=white)
![GGally](https://img.shields.io/badge/GGally-DC5F00?style=flat&logo=r&logoColor=white)
![scales](https://img.shields.io/badge/scales-DC5F00?style=flat&logo=r&logoColor=white)
![ggfortify](https://img.shields.io/badge/ggfortify-DC5F00?style=flat&logo=r&logoColor=white)

#### Manipulación y preparación de datos
![dplyr](https://img.shields.io/badge/dplyr-198754?style=flat&logo=r&logoColor=white)
![tidyr](https://img.shields.io/badge/tidyr-198754?style=flat&logo=r&logoColor=white)
![tibble](https://img.shields.io/badge/tibble-198754?style=flat&logo=r&logoColor=white)
![glue](https://img.shields.io/badge/glue-198754?style=flat&logo=r&logoColor=white)
![readxl](https://img.shields.io/badge/readxl-198754?style=flat&logo=r&logoColor=white)

#### Reportes y tablas
![kableExtra](https://img.shields.io/badge/kableExtra-0D6EFD?style=flat&logo=r&logoColor=white)
![gt](https://img.shields.io/badge/gt-0D6EFD?style=flat&logo=r&logoColor=white)
![formattable](https://img.shields.io/badge/formattable-0D6EFD?style=flat&logo=r&logoColor=white)
![summarytools](https://img.shields.io/badge/summarytools-0D6EFD?style=flat&logo=r&logoColor=white)

#### Modelado y análisis estadístico
![MASS](https://img.shields.io/badge/MASS-6F42C1?style=flat&logo=r&logoColor=white)
![car](https://img.shields.io/badge/car-6F42C1?style=flat&logo=r&logoColor=white)
![broom](https://img.shields.io/badge/broom-6F42C1?style=flat&logo=r&logoColor=white)



---

## 🎯 Objetivos del Proyecto

- Realizar un **análisis exploratorio de datos** (EDA) para identificar patrones, distribuciones y outliers.
- Ajustar un modelo de **regresión lineal simple**, seleccionando la mejor variable predictora.
- Desarrollar modelos de **regresión múltiple**, con selección automática y manual de variables.
- Evaluar los supuestos clásicos del modelo lineal y detectar posibles problemas como multicolinealidad o observaciones influyentes.
- Generar **predicciones para nuevos casos** con sus respectivos intervalos de confianza y predicción.

---

## 📌 Archivos incluidos
- `TP_RL_grasa_corporal.pdf`: Informe final del trabajo práctico
- `TP_RL_grasa_corporal_script.R`: Código completo del análisis en R
- `TP_RL_grasa_corporal.Rmd`: Documento RMarkdown con visualizaciones interactivas y vistas en HTML

---

## 🔍 Principales hallazgos

- La variable **abdomen** mostró la mayor correlación con el porcentaje de grasa corporal (r = 0.80).
- El modelo simple `grc ~ abdomen` explicó el 64.5% de la variabilidad (R² ajustado = 0.644).
- El mejor modelo múltiple fue el seleccionado automáticamente por AIC (stepwise), incluyendo:  
  `altura + cuello + abdomen + cadera`  
  Este modelo presentó:
  - R² ajustado = **0.698**
  - Bajo error estándar residual = **4.537**
  - Cumplimiento razonable de supuestos clásicos

---

## 🛠️ Tecnologías utilizadas

- **Lenguaje:** R
- **Librerías:** `ggplot2`, `dplyr`, `car`, `GGally`, `MASS`, entre otras
- **Entorno:** RStudio + R Markdown

---

## 👩‍💻 Autores

Grupo 10 – Especialización en Ciencia de Datos – UNLaM  
- Yesica Fica Millán  
- Florencia Miranda Charca  
- Franco Petraroia  

---

## 📄 Licencia

Este proyecto se encuentra disponible solo con fines educativos. Para usos distintos, por favor contactar a los autores.

---

## 📬 Contacto

Para consultas, sugerencias o colaboraciones, podés escribirme por [LinkedIn](https://www.linkedin.com/in/yesica-fica-millan/) o abrir un [Issue](https://github.com/) en este repositorio.

