library(haven)
library(data.table)

setwd("C:/Users/gabriela.silva/Desktop/faculdade/kairo")
data<-read_sav("tabulação dados tese efeito uau.sav")


dt<-as.data.table(data)
dt[,.N,by="SEXO"] # 36% mulheres, 64% homens
dt[,.N,by="IDADE"] # 90% jovens adultos, 10% meia idade
dt[,.N,by="LATERALIZAÇÃO"] # 88% destros, 12% canhotos

# Hipótese nula (H₀): Não há diferença na mediana do interesse antes e depois da experiência (ou seja, o deslocamento de localização é zero).
# Hipótese alternativa (H₁): Há diferença nas medianas (o deslocamento de localização é diferente de zero).

# não há evidência estatisticamente significativa de que o nível de interesse tenha mudado após a experiência, com base nesses dados.



# INTERESSADO ------------------------------------------------------------------
wilcox.test(dt$interessado, dt$INTERESSADO2, paired = TRUE)
t.test(dt$interessado, dt$INTERESSADO2, paired = TRUE)

dt_long <- dt %>%
  select(interessado, INTERESSADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(interessado, INTERESSADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência (Interesse)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(interessado,INTERESSADO2)]

# ANGUSTIADO -------------------------------------------------------------------
wilcox.test(dt$angustiado, dt$ANGUSTIADO2, paired = TRUE)
t.test(dt$angustiado, dt$ANGUSTIADO2, paired = TRUE)

dt_long <- dt %>%
  select(angustiado, ANGUSTIADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(angustiado, ANGUSTIADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Angústia)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(angustiado,ANGUSTIADO2)]

# EXCITADO (*) -----------------------------------------------------------------
wilcox.test(dt$excitado, dt$EXCITADO2, paired = TRUE)
t.test(dt$excitado, dt$EXCITADO2, paired = TRUE)

dt_long <- dt %>%
  select(excitado, EXCITADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(excitado, EXCITADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência (Excitação)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(excitado,EXCITADO2)]

# CHATEADO (*) -----------------------------------------------------------------
wilcox.test(dt$chateado, dt$CHATEADO2, paired = TRUE)
t.test(dt$chateado, dt$CHATEADO2, paired = TRUE)

dt_long <- dt %>%
  select(chateado, CHATEADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(chateado, CHATEADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência (Chateação)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(chateado,CHATEADO2)]

# FORTE ------------------------------------------------------------------------
wilcox.test(dt$forte, dt$FORTE2, paired = TRUE)
t.test(dt$forte, dt$FORTE2, paired = TRUE)

dt_long <- dt %>%
  select(forte, FORTE2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(forte, FORTE2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência (Força)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(forte,FORTE2)]

# CULPADO ----------------------------------------------------------------------
wilcox.test(dt$culpado, dt$CULPADO2, paired = TRUE)
t.test(dt$culpado, dt$CULPADO2, paired = TRUE)

dt_long <- dt %>%
  select(culpado, CULPADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(culpado, CULPADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Culpa)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(culpado,CULPADO2)]

# ASSUSTADO --------------------------------------------------------------------
wilcox.test(dt$assutado, dt$ASSUTADO2, paired = TRUE)
t.test(dt$assutado, dt$ASSUTADO2, paired = TRUE)

dt_long <- dt %>%
  select(assutado, ASSUTADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(assutado, ASSUTADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Susto)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(assutado,ASSUTADO2)]

# HOSTIL -----------------------------------------------------------------
wilcox.test(dt$hostil, dt$HOSTIL2, paired = TRUE)
t.test(dt$hostil, dt$HOSTIL2, paired = TRUE)

dt_long <- dt %>%
  select(hostil, HOSTIL2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(hostil, HOSTIL2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência (Hostilidade)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(hostil,HOSTIL2)]

# ENTUSIASMADO -----------------------------------------------------------------
wilcox.test(dt$entusiasmado, dt$ENTUSIASMADO2, paired = TRUE)
t.test(dt$entusiasmado, dt$ENTUSIASMADO2, paired = TRUE)

dt_long <- dt %>%
  select(entusiasmado, ENTUSIASMADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(entusiasmado, ENTUSIASMADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência (Entusiasmo)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(entusiasmo,ENTUSIASMO2)]