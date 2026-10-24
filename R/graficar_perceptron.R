#' Graficar clasificaciones y frontera del perceptron
#'
#' @param modelo Modelo entrenado con pesos, sesgo, medias y desviaciones.
#' @param X Matriz o data.frame con exactamente dos columnas numericas.
#' @param y Vector de etiquetas reales 0 y 1.
#' @param predicciones Vector de predicciones 0 y 1.
#' @return Invisiblemente una lista con grafica, datos y frontera.
#' @export
#' @importFrom ggplot2 ggplot aes geom_contour geom_point scale_fill_manual scale_shape_manual labs theme_minimal theme element_text
graficar_perceptron <- function(modelo, X, y, predicciones) {
  X <- as.matrix(X)
  if (!is.numeric(X) || ncol(X) != 2L || nrow(X) < 2L || any(!is.finite(X))) {
    stop("La grafica necesita X numerica con dos columnas y al menos dos filas.")
  }
  if (!is.list(modelo) || length(modelo$pesos) != 2L ||
      length(modelo$medias) != 2L || length(modelo$desviaciones) != 2L ||
      any(!is.finite(unlist(modelo[c("pesos", "sesgo", "medias", "desviaciones")]))) ||
      any(modelo$desviaciones <= 0)) {
    stop("El modelo debe tener parametros validos para dos variables.")
  }
  if (is.factor(y)) y <- as.character(y)
  if (is.factor(predicciones)) predicciones <- as.character(predicciones)
  y <- suppressWarnings(as.numeric(y))
  predicciones <- suppressWarnings(as.numeric(predicciones))
  if (length(y) != nrow(X) || length(predicciones) != nrow(X) ||
      anyNA(y) || anyNA(predicciones) ||
      !all(y %in% c(0,1)) || !all(predicciones %in% c(0,1))) {
    stop("y y predicciones deben contener 0 y 1 para cada fila de X.")
  }
  datos_grafica <- data.frame(
    horas = X[, 1], asistencia = X[, 2],
    Real = factor(y, levels = c(0, 1), labels = c("No aprueba", "Aprueba")),
    Prediccion = predicciones,
    Resultado = ifelse(y == predicciones, "Correcta", "Incorrecta")
  )
  rejilla <- expand.grid(
    horas = seq(min(X[,1]) - 0.5, max(X[,1]) + 0.5, length.out = 150),
    asistencia = seq(min(X[,2]) - 2, max(X[,2]) + 2, length.out = 150)
  )
  Xs <- scale(as.matrix(rejilla[, c("horas", "asistencia")]),
              center = modelo$medias, scale = modelo$desviaciones)
  rejilla$z <- as.vector(Xs %*% modelo$pesos + modelo$sesgo)
  grafica <- ggplot2::ggplot() +
    ggplot2::geom_contour(data = rejilla,
      ggplot2::aes(x = horas, y = asistencia, z = z),
      breaks = 0, color = "black", linewidth = 1.1) +
    ggplot2::geom_point(data = datos_grafica,
      ggplot2::aes(x = horas, y = asistencia, fill = Real, shape = Resultado),
      size = 3.5, color = "black", stroke = 0.8) +
    ggplot2::scale_fill_manual(values = c("No aprueba" = "#3498DB", "Aprueba" = "#2ECC71"),
                               name = "Resultado real") +
    ggplot2::scale_shape_manual(values = c("Correcta" = 21, "Incorrecta" = 24),
                                name = "Clasificacion") +
    ggplot2::labs(title = "Clasificacion con un perceptron simple",
                  subtitle = "La linea negra representa la frontera de decision",
                  x = "Horas de estudio", y = "Porcentaje de asistencia") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(plot.title = ggplot2::element_text(face = "bold", size = 16),
                   legend.position = "bottom")
  print(grafica)
  invisible(list(grafica = grafica, datos = datos_grafica, frontera = rejilla))
}
