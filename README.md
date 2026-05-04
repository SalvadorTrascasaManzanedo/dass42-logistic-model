# DASS-42 Logistic Model

Modelo de regresión logística para el cribado de depresión severa en adultos jóvenes utilizando las subescalas del DASS-42 y predictores sociodemográficos.

## Objetivo

Desarrollar un modelo predictivo de depresión severa en jóvenes a partir de ansiedad, estrés, edad y nivel educativo.

## Base de datos

Los datos proceden del Open-Source Psychometrics Project. Tras la depuración inicial y la restricción de la muestra a participantes de 18 a 30 años, se trabajó con una muestra aproximada de 26.000 casos.

## Variable criterio

La variable dependiente fue depresión severa, dicotomizada a partir de la subescala de depresión del DASS-42:

- No severa: puntuación < 21
- Severa: puntuación ≥ 21

La clasificación quedó equilibrada: 50.87% no severa y 49.13% severa.
![Distribución de la variable criterio](gráficas/variable%20criterio%20distribución.png)

## Modelo

Se estimaron modelos de regresión logística binaria de forma jerárquica:

1. Modelo nulo
2. Modelo sintomático: ansiedad + estrés
3. Modelo ampliado: ansiedad + estrés + edad + educación + residencia
4. Modelos con interacciones

El modelo final seleccionado por parsimonia incluyó:

- Ansiedad
- Estrés
- Edad
- Educación
- Interacción estrés × educación

La variable residencia fue eliminada al no mejorar sustancialmente el ajuste.

## Interacción estrés × educación

La interacción muestra que la probabilidad de depresión severa aumenta con el estrés en ambos grupos educativos. La educación universitaria parece protectora en niveles bajos o medios de estrés, pero esta ventaja disminuye cuando el estrés es elevado.

![Interacción estrés con universidad](gráficas/interacción%20estrés%20con%20universidad.png)

## Resultados principales

El modelo mostró una mejora clara frente al modelo nulo:

- R² McFadden = .349
- R² Nagelkerke = .511
- Accuracy = 78.7%
- Sensibilidad = .778
- Especificidad = .795
- AUC = .870
![Curva ROC](gráficas/Curva%20ROC.png)


## Matriz de confusión

| Observado / Predicho | No severa | Severa |
|---|---:|---:|
| No severa | 10453 | 2701 |
| Severa | 2816 | 9874 |

El modelo clasificó correctamente 20.327 casos y cometió 5.517 errores de clasificación.

## Interpretación

Ansiedad y estrés fueron los principales predictores de depresión severa. La interacción estrés × educación mostró que la educación universitaria parece actuar como factor protector en niveles bajos o medios de estrés, pero esta ventaja disminuye cuando el estrés es elevado.

## Supuestos

El modelo mostró un cumplimiento general aceptable de los supuestos:

- Sin colinealidad problemática
- Independencia de errores aceptable
- Ligera infradispersión
- Indicios de no linealidad en edad y estrés

## Limitaciones

Este modelo tiene finalidad predictiva, no causal. Por tanto, debe interpretarse como una herramienta de cribado, no como diagnóstico clínico. Además, la muestra procede de un muestreo online no probabilístico, está restringida a jóvenes de 18 a 30 años y presenta desequilibrio por género.

Como línea futura, se recomienda realizar un estudio de potencia para equilibrar grupos de edad y género sin comprometer la capacidad estadística del modelo.

## Autor

Salvador Trascasa Manzanedo
