####의존성 체크#### 
.bb_require <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(sprintf("패키지 '%s'가 필요합니다. install.packages('%s') 후 다시 실행하세요.", pkg, pkg), call. = FALSE)
  }
}
.bb_require("BB")


####학습 모델#### 
train_bb_lm <- function(data, target, predictors, l2 = 0, maxit = 20000, trace = FALSE) {
  X_raw <- as.matrix(data[, predictors, drop = FALSE])
  Xs    <- scale(X_raw)                         # train 기준 표준화
  center <- attr(Xs, "scaled:center")
  scalev <- attr(Xs, "scaled:scale"); scalev[scalev == 0] <- 1  # 분산 0 보호
  X <- cbind(1, Xs)                              # 절편 열
  y <- as.numeric(data[[target]])
  n <- nrow(X)
  
  # 비용/그래디언트 (MSE/2 + L2/2)
  fn <- function(b) { r <- drop(X %*% b - y); sum(r*r)/(2*n) + l2*sum(b[-1]^2)/2 }
  gr <- function(b) {
    g <- drop(t(X) %*% (X %*% b - y))/n
    g[-1] <- g[-1] + l2*b[-1]
    g
  }
  
  opt <- BB::spg(par = rep(0, ncol(X)), fn = fn, gr = gr,
                 control = list(maxit = maxit, trace = trace))

  
  structure(list(
    beta       = opt$par,             # 표준화 공간의 계수 (절편 포함)
    center     = center,              # 표준화 파라미터
    scale      = scalev,
    predictors = predictors,
    target     = target,
    l2         = l2,
    convergence= opt$convergence,
    value      = opt$value
  ), class = "bb_lm")
}

####예측 메서드####
predict.bb_lm <- function(object, newdata, ...) {
  Xn <- as.matrix(newdata[, object$predictors, drop = FALSE])
  Xs <- scale(Xn, center = object$center, scale = object$scale)
  X  <- cbind(1, Xs)
  drop(X %*% object$beta)
}

####계수 추출(원본 스케일로 환산)####
coef.bb_lm <- function(object, ...) {
  b  <- object$beta
  a  <- b[-1] / object$scale
  a0 <- b[1] - sum(b[-1] * object$center / object$scale)
  c(Intercept = a0, stats::setNames(a, object$predictors))
}
####Train 성능 요약####
train_metrics.bb_lm <- function(object, data, include_ic = TRUE) {
  y  <- data[[object$target]]
  yh <- predict(object, data)
  n  <- length(y)
  p  <- length(object$predictors)
  
  sse  <- sum((y - yh)^2)
  sst  <- sum((y - mean(y))^2)
  r2   <- 1 - sse/sst
  adjr2<- 1 - (1 - r2) * (n - 1) / (n - p - 1)
  mse  <- mean((y - yh)^2)
  rmse <- sqrt(mse)
  mae  <- mean(abs(y - yh))
  
  out <- list(R2 = r2, Adj_R2 = adjr2, RMSE = rmse, MAE = mae, SSE = sse)
  if (isTRUE(include_ic)) {
    out$AIC <- n * log(sse/n) + 2 * (p + 1)
    out$BIC <- n * log(sse/n) + log(n) * (p + 1)
  }
  class(out) <- "bb_lm_metrics"
  out
}

####잔차 반환 (진단용)####
residuals.bb_lm <- function(object, data, ...) {
  y  <- data[[object$target]]
  yh <- predict(object, data)
  drop(y - yh)
}