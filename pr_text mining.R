

# DEPENDENCIES ------------------------------------------------------------
library(ggplot2)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(dplyr)
library(textdata)


# DATA LOAD ----------------------------------------------------------

disneyland <- read_csv("disneyland.csv")
head(disneyland)

# 1 -------------------------------------------------------------

# Representación gráfica del número de reviews a lo largo del tiempo
disneyland %>%
  count(year_month) %>%
  ggplot(aes(x = year_month, y = n)) +
  geom_line() +
  scale_x_date(breaks = "1 year", date_labels = "%Y") +
  labs(
    title = "Reviews through the years",
    x = "",
    y = "Número de reviews"
  ) +
  theme_minimal()

#2 -------------------------------------------------------------

# AVERAGE:
mean(disneyland$rating)
#  4.234734

# STD:
sd(disneyland$rating)
#  1.050746


# 3 -------------------------------------------------------------
"""
3. Tokeniza las reviews de tal forma que cada token se corresponda 
con una palabra. 

Cuenta el número de veces que aparece cada palabra en cada parque de
Disneyland. Haz una tabla para cada parque en la que aparezcan las 5 
palabras más frecuentes y su frecuencia. 

¿El listado de palabras más frecuentes es útil para obtener información 
de las reviews?¿Crees que sería conveniente eliminar las stop words?
"""

data(stop_words)

# TOKENIZACIÓN
word_true = unnest_tokens(disneyland,word,review_text) #tokenización por palabras

# Contamos cuantas veces aparece cada palabra en cada parque, agrupando por el identificador del parque en cuestión
word_count <- word_true %>%
  group_by(branch) %>%
  count(word, sort = TRUE)

# Obtener las 5 palabras más frecuentes para cada parque

# PARQUE 1 (branch = 1)
parque_1 <- word_count %>%
  filter(branch == 1) %>% # seleccionamos los datos con branch = 1 para que solo salgan las 5 de ese parque
  slice_max(n = 5, order_by = n) %>%
  arrange(branch, desc(n)) # para que salgan las más repetidas
# Convertimos en un dataframe
masrepetidas_parque1 <- as.data.frame(parque_1)
# vemos la tabla: 
masrepetidas_parque1

# repetimos el proceso en el resto de parques, seleccionando el branch adecuado para cada uno

# PARQUE 2 (branch = 2)
parque_2 <- word_count %>%
  filter(branch == 2) %>% # seleccionamos los datos con branch = 1 para que solo salgan las 5 de ese parque
  slice_max(n = 5, order_by = n) %>%
  arrange(branch, desc(n)) # para que salgan las más repetidas
# Convertimos en un dataframe
masrepetidas_parque2 <- as.data.frame(parque_2)
# vemos la tabla: 
masrepetidas_parque2

# PARQUE 3 (branch = 3)
parque_3 <- word_count %>%
  filter(branch == 3) %>% # seleccionamos los datos con branch = 1 para que solo salgan las 5 de ese parque
  slice_max(n = 5, order_by = n) %>%
  arrange(branch, desc(n)) # para que salgan las más repetidas
# Convertimos en un dataframe
masrepetidas_parque3 <- as.data.frame(parque_3)
# vemos la tabla: 
masrepetidas_parque3


# vemos las 3 tablas seguidas para poder compararlas: 
masrepetidas_parque1
masrepetidas_parque2
masrepetidas_parque3

# 4 -----------------------------------------------------------------------

'''
Repite el apartado anterior pero eliminando las stop words 

(llama al conjunto de datos resultante disney_words). 

¿Ahora se puede extraer información útil o sigue sin ser suficiente? 

Por ejemplo, a la vista del listado de las 5 palabras más frecuentes, 

¿podrías saber con qué parque se corresponde cada indicador de la variable 
branch?

'''

# TOKENIZACIÓN
word_true = unnest_tokens(disneyland,word,review_text) #tokenización por palabras
disney_words = anti_join(word_true,stop_words, by = "word") #cruce con las stopwords

# Contamos cuantas veces aparece cada palabra en cada parque, agrupando por el identificador del parque en cuestión
word_count <- disney_words %>%
  group_by(branch) %>%
  count(word, sort = TRUE)

# Obtener las 5 palabras más frecuentes para cada parque

# PARQUE 1 (branch = 1)
parque_1 <- word_count %>%
  filter(branch == 1) %>% # seleccionamos los datos con branch = 1 para que solo salgan las 5 de ese parque
  slice_max(n = 5, order_by = n) %>%
  arrange(branch, desc(n)) # para que salgan las más repetidas
# Convertimos en un dataframe
masrepetidas_parque1 <- as.data.frame(parque_1)
# vemos la tabla: 
masrepetidas_parque1

# repetimos el proceso en el resto de parques, seleccionando el branch adecuado para cada uno

# PARQUE 2 (branch = 2)
parque_2 <- word_count %>%
  filter(branch == 2) %>% # seleccionamos los datos con branch = 1 para que solo salgan las 5 de ese parque
  slice_max(n = 5, order_by = n) %>%
  arrange(branch, desc(n)) # para que salgan las más repetidas
# Convertimos en un dataframe
masrepetidas_parque2 <- as.data.frame(parque_2)
# vemos la tabla: 
masrepetidas_parque2

# PARQUE 3 (branch = 3)
parque_3 <- word_count %>%
  filter(branch == 3) %>% # seleccionamos los datos con branch = 1 para que solo salgan las 5 de ese parque
  slice_max(n = 5, order_by = n) %>%
  arrange(branch, desc(n)) # para que salgan las más repetidas
# Convertimos en un dataframe
masrepetidas_parque3 <- as.data.frame(parque_3)
# vemos la tabla: 
masrepetidas_parque3


# vemos las 3 tablas más seguidas para poder compararlas: 
masrepetidas_parque1
masrepetidas_parque2
masrepetidas_parque3


#  5 -------------------------------------------------------------

# aplicamos el TF-IDF y lo representamos gráficamente 
disney_words %>%
  count(branch, word) %>%
  bind_tf_idf(word, branch, n) %>%
  arrange(desc(tf_idf)) %>%
  group_by(branch) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = as.factor(branch))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(branch~., ncol = 3, scales = "free") +
  labs(title = "TF-IDF value for each park",
       x = "",
       y = NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#  6 -------------------------------------------------------------

# aplicamos el TF-IDF pero teniendo en cuenta que neceistamos ver otras palabras
disney_branch3 <- disney_words %>%
  filter(branch == 3)
disney_branch3 %>%
  count(rating, word) %>%
  filter(n > 20) %>%
  bind_tf_idf(word, rating, n) %>%
  arrange(desc(tf_idf)) %>%
  group_by(rating) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, rating), fill = as.factor(rating))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(rating~., ncol = 3, scales = "free") +
  labs(title = "TF-IDF value for every word and rating",
       x = "",
       y = NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#  7 -------------------------------------------------------------

disney_ngrams <- disneyland %>%
  unnest_tokens(word, review_text, token = "ngrams", n = 3)

disney_words = anti_join(disney_ngrams,stop_words, by = "word") #cruce de las stopwords


disney_branch3 <- disney_words %>%
  filter(branch == 3)
disney_branch3 %>%
  count(rating, word) %>%
  filter(n > 20) %>%
  bind_tf_idf(word, rating, n) %>%
  arrange(desc(tf_idf)) %>%
  group_by(rating) %>%
  slice_max(tf_idf, n = 5) %>%
  ungroup() %>%
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, rating), fill = as.factor(rating))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(rating~., ncol = 3, scales = "free") +
  labs(title = "TF-IDF value",
       x = "",
       y = NULL) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#  8 -------------------------------------------------------------

get_sentiments("afinn")
disney_sentiment <- disneyland %>%
  unnest_tokens(word, review_text) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(review_id, rating) %>%
  summarise(sentiment = sum(value))

head(disney_sentiment)


#  9  ------------------------------------------------------------

disney_sentiment %>%
  filter(sentiment < 100,
         sentiment > -50) %>%
  ggplot(aes(x = rating, y = sentiment)) +
  geom_boxplot(aes(group = rating)) +
  geom_hline(yintercept = 0, color = "firebrick")


# 10  -----------------------------------------------------------

#Almacenamos el nº de reviews ordenado de mayor a menor
country_count <- disneyland %>%
  count(reviewer_location, sort = TRUE)

# Gráfico con 10 países con más reviews
# Eje x se representa el país y eje y el número de reviews
ggplot(data = country_count[1:10, ], aes(x = reviewer_location, y = n)) +
  geom_bar(stat = "identity", fill = "firebrick") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Country", y = "Number of Reviews", title = "Top 10 Countries with Most Reviews")

#La parte del código "axis.text.x = element_text(angle = 90, hjust = 1)" 
# cambiar la orientación de las etiquetas del eje x en un ángulo de 90 
# grados en sentido antihorario. Además, hjust = 1 las pone en el centro .


# 'geom_col()` es utilizada para crear gráficos de barras en los cuales
# establece la altura de cada barra mediante una variable en el eje Y.
# La opción `fill` es utilizada para establecer el color de relleno de 
#cada barra. 'geom_col()` es utilizada para crear gráficos de barras en 
#los cuales se establece la altura de cada barra mediante una variable en 
# el eje Y. La opción `fill` es utilizada para establecer el color de 
# relleno de cada barra. se establece la altura de cada barra mediante
# una variable en el eje Y. La opción `fill` es utilizada para establecer
# el color de relleno de cada barra.

