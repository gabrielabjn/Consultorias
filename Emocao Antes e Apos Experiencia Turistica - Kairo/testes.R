# INTERESSADO ------------------------------------------------------------------
library(tidyr)
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

dif<- dt$interessado-dt$INTERESSADO2
symmetry.test(dif)

wilcox.test(dt$interessado, dt$INTERESSADO2, paired = TRUE)
t.test(dt$interessado, dt$INTERESSADO2, paired = TRUE)
#SIGN.test(x = dt$interessado, y = dt$INTERESSADO2, alternative = "two.sided")

# Homem ------------------------------------------------------------------------
dt_m_long <- dt_m %>%
  select(interessado, INTERESSADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(interessado, INTERESSADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_m_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "#5D6532") +
  geom_point(size = 2, color = "#5D6532") +
  labs(title = "Mudança no interesse antes e depois da experiência (Interesse)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_m[,.N,keyby = .(interessado,INTERESSADO2)]

dif_m<- dt_m$interessado-dt_m$INTERESSADO2
symmetry.test(dif_m) # assimetrico

SIGN.test(x = dt_m$interessado, y = dt_m$INTERESSADO2, alternative = "two.sided")
# wilcox.test(dt_m$interessado, dt_m$INTERESSADO2, paired = TRUE)
# t.test(dt_m$interessado, dt_m$INTERESSADO2, paired = TRUE)

# Mulher ------------------------------------------------------------------------
dt_f_long <- dt_f %>%
  select(interessado, INTERESSADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(interessado, INTERESSADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_f_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color="purple") +
  geom_point(size = 2, color="purple") +
  labs(title = "Mudança no interesse antes e depois da experiência (Interesse)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_f[,.N,keyby = .(interessado,INTERESSADO2)]

dif_f<- dt_f$interessado-dt_f$INTERESSADO2
symmetry.test(dif_f) 

#SIGN.test(x = dt_f$interessado, y = dt_f$INTERESSADO2, alternative = "two.sided")
wilcox.test(dt_f$interessado, dt_f$INTERESSADO2, paired = TRUE)
t.test(dt_f$interessado, dt_f$INTERESSADO2, paired = TRUE)

# ANGUSTIADO -------------------------------------------------------------------

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

dif<- dt$angustiado-dt$ANGUSTIADO2
symmetry.test(dif) 

#SIGN.test(x = dt$angustiado, y = dt$ANGUSTIADO2, alternative = "two.sided")

wilcox.test(dt$angustiado, dt$ANGUSTIADO2, paired = TRUE)
t.test(dt$angustiado, dt$ANGUSTIADO2, paired = TRUE)

# Homem ------------------------------------------------------------------------

dt_m_long <- dt_m %>%
  select(angustiado, ANGUSTIADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(angustiado, ANGUSTIADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_m_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "#5D6532") +
  geom_point(size = 2, color = "#5D6532") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Angústia)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_m[,.N,keyby = .(angustiado,ANGUSTIADO2)]

dif_m<- dt_m$angustiado-dt_m$ANGUSTIADO2
symmetry.test(dif_m) # assimetrico

SIGN.test(x = dt_m$angustiado, y = dt_m$ANGUSTIADO2, alternative = "two.sided")

#wilcox.test(dt_m$angustiado, dt_m$ANGUSTIADO2, paired = TRUE)
t.test(dt_m$angustiado, dt_m$ANGUSTIADO2, paired = TRUE)

# Mulher -----------------------------------------------------------------------

dt_f_long <- dt_f %>%
  select(angustiado, ANGUSTIADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(angustiado, ANGUSTIADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_f_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "purple") +
  geom_point(size = 2, color = "purple") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Angústia)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_f[,.N,keyby = .(angustiado,ANGUSTIADO2)]

dif_f<- dt_f$angustiado-dt_f$ANGUSTIADO2
symmetry.test(dif_f) 

#SIGN.test(x = dt_f$angustiado, y = dt_f$ANGUSTIADO2, alternative = "two.sided")

wilcox.test(dt_f$angustiado, dt_f$ANGUSTIADO2, paired = TRUE)
t.test(dt_f$angustiado, dt_f$ANGUSTIADO2, paired = TRUE)

# EXCITADO (*) -----------------------------------------------------------------

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

dif<- dt$excitado-dt$EXCITADO2
symmetry.test(dif) 

# SIGN.test(x = dt$excitado, y = dt$excitado2, alternative = "two.sided")

wilcox.test(dt$excitado, dt$EXCITADO2, paired = TRUE)
t.test(dt$excitado, dt$EXCITADO2, paired = TRUE)

# Homem (*) --------------------------------------------------------------------

dt_m_long <- dt_m %>%
  select(excitado, EXCITADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(excitado, EXCITADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_m_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "#5D6532") +
  geom_point(size = 2, color = "#5D6532") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Excitação)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_m[,.N,keyby = .(excitado,EXCITADO2)]

dif_m<- dt_m$excitado-dt_m$EXCITADO2
symmetry.test(dif_m) 

#SIGN.test(x = dt_m$excitado, y = dt_m$excitado2, alternative = "two.sided")

wilcox.test(dt_m$excitado, dt_m$EXCITADO2, paired = TRUE)
t.test(dt_m$excitado, dt_m$EXCITADO2, paired = TRUE)

# Mulher (*) -------------------------------------------------------------------

dt_f_long <- dt_f %>%
  select(excitado, EXCITADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(excitado, EXCITADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_f_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "purple") +
  geom_point(size = 2, color = "purple") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Excitação)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_f[,.N,keyby = .(excitado,EXCITADO2)]

dif_f<- dt_f$excitado-dt_f$EXCITADO2
symmetry.test(dif_f) 

#SIGN.test(x = dt_f$excitado, y = dt_f$excitado2, alternative = "two.sided")
wilcox.test(dt_f$excitado, dt_f$EXCITADO2, paired = TRUE)
t.test(dt_f$excitado, dt_f$EXCITADO2, paired = TRUE)


# CHATEADO (*) -----------------------------------------------------------------
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

dif<- dt$chateado-dt$CHATEADO2
symmetry.test(dif) 

SIGN.test(x = dt$chateado, y = dt$CHATEADO2, alternative = "two.sided")
#wilcox.test(dt$chateado, dt$CHATEADO2, paired = TRUE)
t.test(dt$chateado, dt$CHATEADO2, paired = TRUE)


# Homem ------------------------------------------------------------------------

dt_m_long <- dt_m %>%
  select(chateado, CHATEADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(chateado, CHATEADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_m_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "#5D6532") +
  geom_point(size = 2, color = "#5D6532") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Chateação)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_m[,.N,keyby = .(chateado,CHATEADO2)]

dif_m<- dt_m$chateado-dt_m$CHATEADO2
symmetry.test(dif_m) #assimetrico

SIGN.test(x = dt_m$chateado, y = dt_m$CHATEADO2, alternative = "two.sided")
#wilcox.test(dt_m$chateado, dt_m$CHATEADO2, paired = TRUE)
t.test(dt_m$chateado, dt_m$CHATEADO2, paired = TRUE)

# Mulher (*) -------------------------------------------------------------------

dt_f_long <- dt_f %>%
  select(chateado, CHATEADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(chateado, CHATEADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_f_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "purple") +
  geom_point(size = 2, color = "purple") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Chateação)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_f[,.N,keyby = .(chateado,CHATEADO2)]

dif_f<- dt_f$chateado-dt_f$CHATEADO2
symmetry.test(dif_f) #assimetrico

SIGN.test(x = dt_f$chateado, y = dt_f$CHATEADO2, alternative = "two.sided")
#wilcox.test(dt_f$chateado, dt_f$CHATEADO2, paired = TRUE)
t.test(dt_f$chateado, dt_f$CHATEADO2, paired = TRUE)


# FORTE ------------------------------------------------------------------------

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

dif<- dt$forte-dt$FORTE2
symmetry.test(dif)

#SIGN.test(x = dt_m$chateado, y = dt_m$CHATEADO2, alternative = "two.sided")
wilcox.test(dt$forte, dt$FORTE2, paired = TRUE)
t.test(dt$forte, dt$FORTE2, paired = TRUE)

# Homem ------------------------------------------------------------------------
dt_m_long <- dt_m %>%
  select(forte, FORTE2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(forte, FORTE2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_m_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "#5D6532") +
  geom_point(size = 2, color = "#5D6532") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Força)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_m[,.N, keyby = .(forte, FORTE2)]

dif_m<- dt_m$forte-dt_m$FORTE2
symmetry.test(dif_m)

#SIGN.test(x = dt_m$chateado, y = dt_m$CHATEADO2, alternative = "two.sided")
wilcox.test(dt_m$forte, dt_m$FORTE2, paired = TRUE)
t.test(dt_m$forte, dt_m$FORTE2, paired = TRUE)

# Mulher -----------------------------------------------------------------------
dt_f_long <- dt_f %>%
  select(forte, FORTE2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(forte, FORTE2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_f_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "purple") +
  geom_point(size = 2, color = "purple") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Força)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_f[,.N, keyby = .(forte, FORTE2)]

dif_f<- dt_f$forte-dt_f$FORTE2
symmetry.test(dif_f)

#SIGN.test(x = dt_m$chateado, y = dt_m$CHATEADO2, alternative = "two.sided")
wilcox.test(dt_f$forte, dt_f$FORTE2, paired = TRUE)
t.test(dt_f$forte, dt_f$FORTE2, paired = TRUE)

# CULPADO ----------------------------------------------------------------------
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

dif<- dt$culpado-dt$CULPADO2
symmetry.test(dif)

#SIGN.test(x = dt_m$chateado, y = dt_m$CHATEADO2, alternative = "two.sided")
wilcox.test(dt$culpado, dt$CULPADO2, paired = TRUE)
t.test(dt$culpado, dt$CULPADO2, paired = TRUE)

# Homem ------------------------------------------------------------------------
dt_m_long <- dt_m %>%
  select(culpado, CULPADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(culpado, CULPADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_m_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "#5D6532") +
  geom_point(size = 2, color = "#5D6532") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Culpa)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_m[,.N, keyby = .(culpado, CULPADO2)]

dif_m<- dt_m$culpado-dt_m$CULPADO2
symmetry.test(dif_m)

#SIGN.test(x = dt_m$chateado, y = dt_m$CHATEADO2, alternative = "two.sided")
wilcox.test(dt_m$culpado, dt_m$CULPADO2, paired = TRUE)
t.test(dt_m$culpado, dt_m$CULPADO2, paired = TRUE)

# Mulher -----------------------------------------------------------------------
dt_f_long <- dt_f %>%
  select(culpado, CULPADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(culpado, CULPADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_f_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "purple") +
  geom_point(size = 2, color = "purple") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Culpa)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_f[,.N, keyby = .(culpado, CULPADO2)]

dif_f<- dt_f$culpado-dt_f$CULPADO2
symmetry.test(dif_f)

SIGN.test(x = dt_f$culpado, y = dt_f$CULPADO2, alternative = "two.sided")
#wilcox.test(dt_f$culpado, dt_f$CULPADO2, paired = TRUE)
t.test(dt_f$culpado, dt_f$CULPADO2, paired = TRUE)

# ASSUSTADO --------------------------------------------------------------------

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

dif<- dt$assutado-dt$ASSUTADO2
symmetry.test(dif)

#SIGN.test(x = dt_m$chateado, y = dt_m$CHATEADO2, alternative = "two.sided")
wilcox.test(dt$assutado, dt$ASSUTADO2, paired = TRUE)
t.test(dt$assutado, dt$ASSUTADO2, paired = TRUE)


# Homem ------------------------------------------------------------------------
dt_m_long <- dt_m %>%
  select(assutado, ASSUTADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(assutado, ASSUTADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_m_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "#5D6532") +
  geom_point(size = 2, color = "#5D6532") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Assustado)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_m[,.N, keyby = .(assutado, ASSUTADO2)]

dif_m<- dt_m$assutado-dt_m$ASSUTADO2
symmetry.test(dif_m)

#SIGN.test(x = dt_m$chateado, y = dt_m$CHATEADO2, alternative = "two.sided")
wilcox.test(dt_m$assutado, dt_m$ASSUTADO2, paired = TRUE)
t.test(dt_m$assutado, dt_m$ASSUTADO2, paired = TRUE)

# Mulher -----------------------------------------------------------------------
dt_f_long <- dt_f %>%
  select(assutado, ASSUTADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(assutado, ASSUTADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_f_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "purple") +
  geom_point(size = 2, color = "purple") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Assustado)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_f[,.N, keyby = .(assutado, ASSUTADO2)]

dif_f<- dt_f$assutado-dt_f$ASSUTADO2
symmetry.test(dif_f)

#SIGN.test(x = dt_m$chateado, y = dt_m$CHATEADO2, alternative = "two.sided")
wilcox.test(dt_f$assutado, dt_f$ASSUTADO2, paired = TRUE)
t.test(dt_f$assutado, dt_f$ASSUTADO2, paired = TRUE)

?wilcox.test
# HOSTIL -----------------------------------------------------------------

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

dif<- dt$hostil-dt$HOSTIL2
symmetry.test(dif) # assimetrico

SIGN.test(x = dt$hostil, y = dt$HOSTIL2, alternative = "two.sided")
#wilcox.test(dt$hostil, dt$HOSTIL2, paired = TRUE)
t.test(dt$hostil, dt$HOSTIL2, paired = TRUE)

# Homem ------------------------------------------------------------------------
dt_m_long <- dt_m %>%
  select(hostil, HOSTIL2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(hostil, HOSTIL2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_m_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "#5D6532") +
  geom_point(size = 2, color = "#5D6532") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Hostilidade)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_m[,.N, keyby = .(hostil, HOSTIL2)]

dif_m<- dt_m$hostil-dt_m$HOSTIL2
symmetry.test(dif_m) # assimetrico

SIGN.test(x = dt_m$hostil, y = dt_m$HOSTIL2, alternative = "two.sided")
#wilcox.test(dt_m$hostil, dt_m$HOSTIL2, paired = TRUE)
t.test(dt_m$hostil, dt_m$HOSTIL2, paired = TRUE)

# Mulher -----------------------------------------------------------------------
dt_f_long <- dt_f %>%
  select(hostil, HOSTIL2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(hostil, HOSTIL2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_f_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "purple") +
  geom_point(size = 2, color = "purple") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Hostilidade)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_f[,.N, keyby = .(hostil, HOSTIL2)]

dif_f<- dt_f$hostil-dt_f$HOSTIL2
symmetry.test(dif_f) # assimetrico

SIGN.test(x = dt_f$hostil, y = dt_f$HOSTIL2, alternative = "two.sided")
#wilcox.test(dt_f$hostil, dt_f$HOSTIL2, paired = TRUE)
t.test(dt_f$hostil, dt_f$HOSTIL2, paired = TRUE)


# ENTUSIASMADO (*) -------------------------------------------------------------

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

dt[,.N,keyby = .(entusiasmado,ENTUSIASMADO2)]

dif<- dt$entusiasmado-dt$ENTUSIASMADO2
symmetry.test(dif) 

SIGN.test(x = dt$entusiasmado, dt$ENTUSIASMADO2, alternative = "two.sided")
#wilcox.test(dt$entusiasmado, dt$ENTUSIASMADO2, paired = TRUE)
t.test(dt$entusiasmado, dt$ENTUSIASMADO2, paired = TRUE)

# Homem ------------------------------------------------------------------------
dt_m_long <- dt_m %>%
  select(entusiasmado, ENTUSIASMADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(entusiasmado, ENTUSIASMADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_m_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "#5D6532") +
  geom_point(size = 2, color = "#5D6532") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Entusiasmo)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_m[,.N, keyby = .(entusiasmado, ENTUSIASMADO2)]

dif_m<- dt_m$entusiasmado-dt_m$ENTUSIASMADO2
symmetry.test(dif_m) #assimetrico

SIGN.test(x = dt_m$entusiasmado, dt_m$ENTUSIASMADO2, alternative = "two.sided")
#wilcox.test(dt_m$entusiasmado, dt_m$ENTUSIASMADO2, paired = TRUE)
t.test(dt_m$entusiasmado, dt_m$ENTUSIASMADO2, paired = TRUE)

# Mulher (*) -------------------------------------------------------------------
dt_f_long <- dt_f %>%
  select(entusiasmado, ENTUSIASMADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(entusiasmado, ENTUSIASMADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_f_long, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5, color = "purple") +
  geom_point(size = 2, color = "purple") +
  labs(title = "Mudança no interesse antes e depois da experiência  (Entusiasmo)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt_f[,.N, keyby = .(entusiasmado, ENTUSIASMADO2)]

dif_f<- dt_f$entusiasmado-dt_f$ENTUSIASMADO2
symmetry.test(dif_f) 

#SIGN.test(x = dt_f$entusiasmado, dt_f$ENTUSIASMADO2, alternative = "two.sided")
wilcox.test(dt_f$entusiasmado, dt_f$ENTUSIASMADO2, paired = TRUE)
t.test(dt_f$entusiasmado, dt_f$ENTUSIASMADO2, paired = TRUE)


# ORGULHOSO ----------------------------------------------------------------------
dt_long_orgulhoso <- dt %>%
  select(orgulhoso, ORGULHOSO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(orgulhoso, ORGULHOSO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long_orgulhoso, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Orgulho)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(orgulhoso, ORGULHOSO2)]

# Teste de simetria para "orgulhoso"
dif_orgulhoso <- dt$orgulhoso - dt$ORGULHOSO2
symmetry.test(dif_orgulhoso)

# Testes de comparação para "orgulhoso"
wilcox.test(dt$orgulhoso, dt$ORGULHOSO2, paired = TRUE)
t.test(dt$orgulhoso, dt$ORGULHOSO2, paired = TRUE)
SIGN.test(x = dt$orgulhoso, dt$ORGULHOSO2, alternative = "two.sided")

# IRRITÁVEL ----------------------------------------------------------------------
dt_long_irritavel <- dt %>%
  select(irritável, IRRITÁVEL2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(irritável, IRRITÁVEL2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long_irritavel, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Irritabilidade)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(irritável, IRRITÁVEL2)]

# Teste de simetria para "irritável"
dif_irritável <- dt$irritável - dt$IRRITÁVEL2
symmetry.test(dif_irritável)

# Testes de comparação para "irritável"
SIGN.test(x = dt$irritável, dt$IRRITÁVEL2, alternative = "two.sided")
wilcox.test(dt$irritável, dt$IRRITÁVEL2, paired = TRUE)
t.test(dt$irritável, dt$IRRITÁVEL2, paired = TRUE)


# ALERTA ----------------------------------------------------------------------
dt_long_alerta <- dt %>%
  select(alerta, ALERTA2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(alerta, ALERTA2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long_alerta, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Alerta)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(alerta, ALERTA2)]

# Teste de simetria para "alerta"
dif_alerta <- dt$alerta - dt$ALERTA2
symmetry.test(dif_alerta)

# Testes de comparação para "alerta"
wilcox.test(dt$alerta, dt$ALERTA2, paired = TRUE)
t.test(dt$alerta, dt$ALERTA2, paired = TRUE)

# ENVERGONHADO ----------------------------------------------------------------------
dt_long_envergonhado <- dt %>%
  select(envergonhado, ENVERGONHADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(envergonhado, ENVERGONHADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long_envergonhado, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Vergonha)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(envergonhado, ENVERGONHADO2)]

# Teste de simetria para "envergonhado"
dif_envergonhado <- dt$envergonhado - dt$ENVERGONHADO2
symmetry.test(dif_envergonhado)

# Testes de comparação para "envergonhado"
wilcox.test(dt$envergonhado, dt$ENVERGONHADO2, paired = TRUE)
t.test(dt$envergonhado, dt$ENVERGONHADO2, paired = TRUE)


# INSPIRADO ----------------------------------------------------------------------
dt_long_inspirado <- dt %>%
  select(inspirado, INSPIRADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(inspirado, INSPIRADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long_inspirado, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Inspiração)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(inspirado, INSPIRADO2)]

# Teste de simetria para "inspirado"
dif_inspirado <- dt$inspirado - dt$INSPIRADO2
symmetry.test(dif_inspirado)

# Testes de comparação para "inspirado"
SIGN.test(x = dt$inspirado, dt$INSPIRADO2, alternative = "two.sided")
wilcox.test(dt$inspirado, dt$INSPIRADO2, paired = TRUE)
t.test(dt$inspirado, dt$INSPIRADO2, paired = TRUE)


# NERVOSO ----------------------------------------------------------------------
dt_long_nervoso <- dt %>%
  select(nervoso, NERVOSO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(nervoso, NERVOSO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long_nervoso, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Nervosismo)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(nervoso, NERVOSO2)]

# Teste de simetria para "nervoso"
dif_nervoso <- dt$nervoso - dt$NERVOSO2
symmetry.test(dif_nervoso)

# Testes de comparação para "nervoso"
SIGN.test(x = dt$nervoso, dt$NERVOSO2, alternative = "two.sided")
wilcox.test(dt$nervoso, dt$NERVOSO2, paired = TRUE)
t.test(dt$nervoso, dt$NERVOSO2, paired = TRUE)

# DETERMINADO ----------------------------------------------------------------------
dt_long_determinado <- dt %>%
  select(determinado, DETERMINADO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(determinado, DETERMINADO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long_determinado, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Determinação)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(determinado, DETERMINADO2)]

# Teste de simetria para "determinado"
dif_determinado <- dt$determinado - dt$DETERMINADO2
symmetry.test(dif_determinado)

# Testes de comparação para "determinado"
wilcox.test(dt$determinado, dt$DETERMINADO2, paired = TRUE)
t.test(dt$determinado, dt$DETERMINADO2, paired = TRUE)


# ATENTO ----------------------------------------------------------------------
dt_long_atento <- dt %>%
  select(atento, ATENTO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(atento, ATENTO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long_atento, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Atenção)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(atento, ATENTO2)]

# Teste de simetria para "atento"
dif_atento <- dt$atento - dt$ATENTO2
symmetry.test(dif_atento)

# Testes de comparação para "atento"
wilcox.test(dt$atento, dt$ATENTO2, paired = TRUE)
t.test(dt$atento, dt$ATENTO2, paired = TRUE)


# ANSIOSO ----------------------------------------------------------------------
dt_long_ansioso <- dt %>%
  select(ansioso, ANSIOSO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(ansioso, ANSIOSO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long_ansioso, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Ansiedade)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(ansioso, ANSIOSO2)]

# Teste de simetria para "ansioso"
dif_ansioso <- dt$ansioso - dt$ANSIOSO2
symmetry.test(dif_ansioso)

# Testes de comparação para "ansioso"
wilcox.test(dt$ansioso, dt$ANSIOSO2, paired = TRUE)
t.test(dt$ansioso, dt$ANSIOSO2, paired = TRUE)

# ATIVO ----------------------------------------------------------------------
dt_long_ativo <- dt %>%
  select(ativo, ATIVO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(ativo, ATIVO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long_ativo, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Atividade)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(ativo, ATIVO2)]

# Teste de simetria para "ativo"
dif_ativo <- dt$ativo - dt$ATIVO2
symmetry.test(dif_ativo)

# Testes de comparação para "ativo"
wilcox.test(dt$ativo, dt$ATIVO2, paired = TRUE)
t.test(dt$ativo, dt$ATIVO2, paired = TRUE)

# MEDO ----------------------------------------------------------------------
dt_long_medo <- dt %>%
  select(medo, MEDO2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(cols = c(medo, MEDO2),
               names_to = "momento", values_to = "resposta")

ggplot(dt_long_medo, aes(x = momento, y = resposta, group = id)) +
  geom_line(alpha = 0.5) +
  geom_point(size = 2) +
  labs(title = "Mudança no interesse antes e depois da experiência  (Medo)",
       x = "Momento", y = "Nível de interesse (Likert)") +
  theme_minimal()

dt[,.N,keyby = .(medo, MEDO2)]

# Teste de simetria para "medo"
dif_medo <- dt$medo - dt$MEDO2
symmetry.test(dif_medo)

# Testes de comparação para "medo"
wilcox.test(dt$medo, dt$MEDO2, paired = TRUE)
t.test(dt$medo, dt$MEDO2, paired = TRUE)
