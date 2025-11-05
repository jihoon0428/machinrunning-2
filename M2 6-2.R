vif_values <- vif(result)

print(vif_values)

selected_predictors <- names(vif_values[vif_values < 7])
print(selected_predictors)

influencePlot(result, id.method="identify")

df=df[-c(121), ]
df=df[-c(381, 419, 366, 369, 373), ]