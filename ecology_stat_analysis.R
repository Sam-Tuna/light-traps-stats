library(readxl)
library(dplyr)
library(tidyr)
library(MASS)
library(ggplot2)
library(Hmisc)
library(plotly)

df <- read_excel("RawData_Ecology.xlsx", col_names = TRUE)

summary(df)

moon_season <- df %>% group_by(Night, MoonPhase, Season) %>%
  summarise(Count=sum(Count), .groups="drop")

boxplot(Count ~ MoonPhase, data=moon_season, ylab="Individuals per night", col=c("white", "#7f7f7f"))
fig <- plot_ly(moon_season, y = ~Count, color = ~MoonPhase, type = "box", colors = c("#7f7f7f", "black"), yaxis="Individuals captured"); fig
boxplot(Count ~ Season, data=moon_season, ylab="Individuals per night", col=c("#ff7f0e", "#9467bd", "#2ca02c", "#1f77b4"))
fig <- plot_ly(moon_season, y = ~Count, color = ~Season, type = "box", colors = c("#ff7f0e", "#9467bd", "#2ca02c", "#1f77b4") ); fig
fig <- plot_ly(moon_season, y = ~Count, color = ~Season, type = "violin", colors = c("#ff7f0e", "#9467bd", "#2ca02c", "#1f77b4") ); fig

df$Temp_min <- as.numeric(as.character(df$Temp_min))
df$Temp_max <- as.numeric(as.character(df$Temp_max))
df$Hum_min <- as.numeric(as.character(df$Hum_min))
df$Hum_max <- as.numeric(as.character(df$Hum_max))
df$Rain <- as.numeric(as.character(df$Rain))

summary(df)

df$Temp_avg <- (df$Temp_min + df$Temp_max) / 2
df$Hum_avg <- (df$Hum_min + df$Hum_max) / 2

vars <- df %>% group_by(Night, MoonPhase, Season, TrapType, Temp_avg, Hum_avg, Rain) %>% summarise(Count=sum(Count), .groups="drop")


nb_model <- glm.nb(Count ~ TrapType + MoonPhase + Temp_avg + Hum_avg + Rain + Season, data=vars)
print(summary(nb_model))

pivot <- xtabs(Count ~ ArthropodOrder + TrapType, data=df)
print(chisq.test(pivot))

print(cor(vars$Count, vars$Temp_avg, use="complete.obs"))
print(cor(vars$Count, vars$Hum_avg, use="complete.obs"))
print(cor(vars$Count, vars$Rain, use="complete.obs"))


boxplot(Count ~ TrapType, data=vars, main="Arthropod captured by trap type", ylab="Individuals per trap-night")
fig <- plot_ly(vars, y = ~Count, color = ~TrapType, type = "violin", colors = c("#1f77b4", "#7f7f7f", "#2ca02c", "#d62728")); fig
fig <- plot_ly(vars, y = ~Count, color = ~TrapType, type = "box", colors = c("#1f77b4", "#7f7f7f", "#2ca02c", "#d62728")); fig

p <- ggplot(vars, aes(y=vars$Count, x=vars$TrapType), main="Arthropod captured by trap type", ylab="Individuals per trap-night") + geom_violin()
p + stat_summary(fun.data=mean_sdl, mult=1, geom="pointrange", color="red")
       
mat <- as.matrix(addmargins(pivot, 0)[-nrow(pivot)-1, ]); image(t(mat[nrow(mat):1,]), axes=FALSE); box()

plot(vars$Temp_avg, vars$Count, main="Maximum temperature vs arthropod captures",
     xlab="Average Temperature (°C)", ylab="Individuals per trap-night")
abline(lm(Count ~ Temp_avg, data=vars))

season_trap <- vars %>% group_by(Season, TrapType) %>% summarise(Total=sum(Count), .groups="drop")
seasons <- unique(season_trap$Season)
fig <- plot_ly(season_trap, y = ~Total, color = ~TrapType, type = "bar", colors = c("#1f77b4", "#7f7f7f", "#2ca02c", "#d62728"),
               xaxis = list(title=seasons)); fig
