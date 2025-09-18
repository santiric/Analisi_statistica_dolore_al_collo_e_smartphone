mod_df$intensita_dolore_classi <- factor(mod_df$intensita_dolore_classi,
                                         levels = c("no_dolore", "dolore_basso", "dolore_medio", "dolore_alto"),
                                         ordered = TRUE)

mod_df$frequenza_dolore <- factor(
  mod_df$frequenza_dolore,
  levels = c("0 volte",
             "Sporadicamente (1-2 volte al mese)",
             "Frequentemente (almeno una volta a settimana)",
             "Quasi tutti i giorni"),
  ordered = TRUE
)


# Presenza del dolore -----------------------------------------------------

mod.logit.eta <- glm(Dolore_cervicale~sesso+altezza+I(altezza^2)+peso+età,family = binomial,data = mod_df)
summary(mod.logit.eta)

summary(mod_df$età) #range della variabile età


# Frequenza del dolore ----------------------------------------------------

library(VGAM)

mod.freq.eta <- vglm(frequenza_dolore~sesso+altezza+peso+età,
                     family = cumulative(parallel = TRUE),
                     data=mod_df)
summary(mod.freq.eta)

mod.freq.studio <- vglm(frequenza_dolore~sesso+altezza+peso+età+ore_studio_e_pc,
                            family = cumulative(parallel = TRUE),
                            data=mod_df)

summary(mod.freq.studio)


mod.freq.sport <- vglm(frequenza_dolore~sesso+altezza+peso+età+freq_sport_quant,
                           family = cumulative(parallel = TRUE),
                           data=mod_df)
summary(mod.freq.sport)


mod.freq.smt <- vglm(frequenza_dolore~sesso+altezza+peso+età+ore_smartphone_tablet,
                     family = cumulative(parallel = TRUE),
                     data=mod_df)
summary(mod.freq.smt)


mod.freq.altezza_disp <- vglm(frequenza_dolore~sesso+altezza+peso+età+altezza_dispositivo,
                              family = cumulative(parallel = TRUE),
                              data=mod_df)
mod.ridotto <- vglm(frequenza_dolore~sesso+altezza+peso+età,
                    family = cumulative(parallel = TRUE),
                    data=mod_df)
summary(mod.freq.altezza_disp)
anova(mod.ridotto,mod.freq.altezza_disp,type='I')



# Intensità del dolore ----------------------------------------------------

mod.int.eta <- vglm(intensita_dolore_classi~sesso+altezza+peso+età,
                    family = cumulative(parallel = TRUE),
                    data=mod_df)
summary(mod.int.eta)


mod.int.smt <- vglm(intensita_dolore_classi~sesso+altezza+peso+età+ore_smartphone_tablet,
                    family = cumulative(parallel = TRUE),
                    data=mod_df)
summary(mod.int.smt)


mod.int.altezza_disp <- vglm(intensita_dolore_classi~sesso+altezza+peso+età+altezza_dispositivo,
                             family = cumulative(parallel = TRUE),
                             data=mod_df)
summary(mod.int.altezza_disp)
mod.ridotto <- vglm(intensita_dolore_classi~sesso+altezza+peso+età,
                    family = cumulative(parallel = TRUE),
                    data=mod_df)
anova(mod.int.altezza_disp,mod.ridotto,type = 'I')



mod.int.studio <- vglm(intensita_dolore_classi~sesso+altezza+peso+età+ore_studio_e_pc,
                       family = cumulative(parallel = TRUE),
                       data=mod_df)
summary(mod.int.studio)





