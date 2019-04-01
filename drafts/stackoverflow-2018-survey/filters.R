role_choices <- list("Back-end developer", 
                     "Full-stack developer", 
                     "Data or business analyst",
                     "Front-end developer",
                     "Database administrator")


roles_chosen <- c("Back-end developer", "Database administrator")



language_choices <- list("Python", 
                         "SQL", 
                         "Bash/Shell",
                         "HTML",
                         "JavaScript",
                         "CSS",
                         "R",
                         "Java",
                         "C++",
                         "C#")

languages_chosen <- c("Python","SQL","R")
languages_unchosen <- unlist(language_choices[-which(language_choices %in% languages_chosen)])

role_chosen <- which(role_choices[unlist(lapply(role_choices, function(x) x %in% chosen))] == TRUE)

str(unlist(lapply(role_choices, function(x) x %in% chosen)))

#role_not_chosen <- 

languages_chosen
languages_unchosen
  
  
paste0("^(?=.*",paste(languages_unchosen, sep = "",collapse = ")(?=.*"),").*$")

## 
  
chosen

chosen

paste(languages_chosen,sep="",collapse = ")(")
paste0("[(",paste(languages_chosen,sep="",collapse = ")("),")]")

these_only <-
us_ds %>%
  filter(grepl(paste0("^(?=.*",paste(languages_chosen, sep = "",collapse = ")(?=.*"),").*$"), LanguageWorkedWith, perl = TRUE),
         !grepl(paste0("(",paste(languages_unchosen,sep="",collapse=")|("),")"),LanguageWorkedWith, perl = TRUE))
        

languages_unchosen
these_only$LanguageWorkedWith[3]

these_only %>%
  group_by(YearsCodingProf) %>%
  summarise(mean_sal = mean(ConvertedSalary,na.rm=TRUE),
            count = n())

these_only %>%
  select(LanguageWorkedWith) %>%
  View()

paste0("^(?=.*\b",paste(languages_chosen, sep = "",collapse = "\b)(?=.*\b"),"\b).*$")

^(?=.*\bjack\b)(?=.*\bjames\b).*$


a_list <- "CSS;R;Java"
grepl("(Java)|(R)|(CSS)", a_list)

these_only <-
  us_ds %>%
filter(sum(languages_chosen %in% language_choices) == length(languages_chosen),
       sum(languages_unchosen %in% language_choices) == length(languages_unchosen))


languages_unchosen

us_ds$LanguageWorkedWith[2]

paste0("(",paste(languages_unchosen,sep="",collapse=")|("),")"),LanguagesWorkedWith

sum(languages_chosen %in% language_choices) == length(languages_chosen)
sum(languages_unchosen %in% language_choices) == length(languages_unchosen)
