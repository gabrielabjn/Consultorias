library(haven)
library(data.table)
library(lawstat)
library(BSDA)

setwd("C:/Users/gabriela.silva/Desktop/faculdade/kairo")
data<-read_sav("tabulação dados tese efeito uau.sav")


dt<-as.data.table(data)
dt[,.N,by="SEXO"] # 64% mulheres, 36% homens
dt[,.N,by="IDADE"] # 90% jovens adultos, 10% meia idade
dt[,.N,by="LATERALIZAÇÃO"] # 88% destros, 12% canhotos

# Hipótese nula (H₀): Não há diferença na mediana do interesse antes e depois da experiência (ou seja, o deslocamento de localização é zero).
# Hipótese alternativa (H₁): Há diferença nas medianas (o deslocamento de localização é diferente de zero).

# não há evidência estatisticamente significativa de que o nível de interesse tenha mudado após a experiência, com base nesses dados.

dt_f<-dt%>% filter(SEXO==2)
dt_m<-dt%>% filter(SEXO==1)

# teste wilcox paired samples (p. 44 apostila INP)

# INTERESSADO ------------------------------------------------------------------

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
