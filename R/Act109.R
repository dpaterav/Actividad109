library(nycflights13)
library(tidyverse)

nycflights13::flights

fly_1 <- flights

#'This function takes the user's selection and prints the selected response.
#'@param seleccion user option (entero)
#'@return Prints the selected answer.
#'@examples
#'retrieve_answer(1)
#'retrieve_answer(5)

retrieve_answer <- function(seleccion){
  seleccion <- readline(prompt = " Ingrese un número entre 1 y 6: ")
  seleccion <- as.integer(seleccion)


  if (seleccion >= 1 && seleccion <= 6)
  {
    cat("Usted ha seleccionado:")
  }
  else
  {
    cat("Intentelo denuevo./n")
    retrieve_answer()
  }

  if(seleccion == 1L){
    cat("Exercice 5.2.4, Item 1 and 2. \n")
    ej_1 <- filter(flights, arr_delay >= 120)
    ej_2 <- filter(fly_1 , dest == "IAH" | dest == "HOU")

    cat("Item 1.\n")
    print(ej_1[1:15])
    cat("Item 2. \n")
    print(ej_2[1:15])
  }

  if(seleccion==2){
    cat("Exercice 5.3.1, Item 1,2,3,4. \n")
    ej_31 <- arrange(fly_1, desc(is.na(dep_time)))
    f_ret <- arrange(fly_1, desc(dep_delay))#Delayed flights

    ej_33 <- head(arrange(fly_1, air_time))
    f_rap <- head(arrange(flights, desc(distance / air_time)))#Faster flights

    f_lng <- arrange(flights, desc(distance))#Longest flight
    f_crt <- arrange(flights, distance) #Shortest flight

    cat("Item 1.\n")
    print(f_ret[1:15])
    cat("Item 2. \n")
    print(f_rap[1:15])
    cat("Item 3. \n")
    print(f_lng[1:15])
    cat("Item 4. \n")
    print(f_crt[1:15])
  }

  if(seleccion==3){
    cat("Exercice 5.4.1, Item 2,3,4. \n")
    ej_32 <- select(fly_1, year, month, day, year, year)
    ej_32 <- select(fly_1, arr_delay, everything())

    vars <- c("year", "month", "day", "dep_delay", "arr_delay")
    ej_33 <- select(fly_1, any_of(vars))

    ej_34 <- select(fly_1, contains("TIME"))

    cat("Item 2. \n")
    print(ej_32)
    cat("Item 3. \n")
    print(ej_33)
    cat("Item 4. \n")
    print(ej_34)
  }

  if(seleccion==4){
    cat("Exercice 5.5.2, Item 1,2. \n")
    f_time <- mutate(fly_1,
                     dep_time_mins = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
                     sched_dep_time_mins =(sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440)
    f_airtime <-
      mutate(fly_1,
             dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
             arr_time = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
             air_time_diff = air_time - arr_time + dep_time)

    cat("Item 1.\n")
    print(f_time)
    cat("Item 2. \n")
    print(f_airtime)
  }

  if(seleccion==5){
    cat("Exercice 5.6.7, Item 1. \n")
    f_3 <- mutate(on_time = !is.na)

                  cat("Item 1.\n")
                  print(f_3)

  }
  if(seleccion==6){
    cat("Exercice 5.7.1, Item 2. \n")
    fly_1 %>%
      filter(!is.na(tailnum)) %>%
      mutate(on_time = !is.na(arr_time) & (arr_delay <= 0)) %>%
      group_by(tailnum) %>%
      summarise(on_time = mean(on_time), n = n()) %>%
      filter(min_rank(on_time) == 1)

    fly_1 %>%
      filter(!is.na(arr_delay)) %>%
      group_by(tailnum) %>%
      summarise(arr_delay = mean(arr_delay), n = n()) %>%
      filter(n >= 20) %>%
      filter(min_rank(desc(arr_delay)) == 1)

    cat("Item 2.\n")
    print(fly_1)
  }
}



