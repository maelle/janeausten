library("ggplot2")
library("dplyr")

# Cool package with books
library("janeaustenr")
# Load one book
data("prideprejudice")
book <- prideprejudice

# packages for named entity recognition
library("openNLP")
library("NLP")
library("openNLPmodels.en")

# Identification of locations
# 
# sent_token_annotator <- Maxent_Sent_Token_Annotator()
# word_token_annotator <- Maxent_Word_Token_Annotator()
# annotatorLocation <- Maxent_Entity_Annotator(language = "en", kind = "location")
# whereVector <- NULL
# for (abstract in book){
#     text <- abstract
#     text <- as.String(text)
#     
#     a2 <- annotate(text, list(sent_token_annotator, word_token_annotator))
#     where <- annotatorLocation(text, a2)
#     if(length(where)>0){
#       print(text[where])
#       whereVector <- c(whereVector, text[where])
#     }
# }
# 
# # Identification of persons
# annotatorPerson <- Maxent_Entity_Annotator(language = "en", kind = "person")
# personVector <- NULL
# for (abstract in book){
#   text <- abstract
#   text <- as.String(text)
#   
#   a2 <- annotate(text, list(sent_token_annotator, word_token_annotator))
#   person <- annotatorPerson(text, a2)
#   if(length(person)>0){
#     print(text[person])
#     personVector <- c(personVector, text[person])
#   }
# }
# 
# save(whereVector, file = "whereVector.RData")
# save(personVector, file = "personVector.RData")

load("whereVector.RData")
load("personVector.RData")

# now in the locations we don't want persons
whereVector <- whereVector[!(whereVector %in% unique(personVector))]
unique(whereVector)
unique(personVector)

# Charlotte is misclassified!
personVector <- c(personVector, whereVector[whereVector == "Charlotte"])
whereVector <- whereVector[whereVector != "Charlotte"]

# I can also find frequencies of each name


# for the plot I'll filter persons with more than 10 occurences
dataNames <- tbl_df(data.frame(person = personVector)) 
notRare <- dataNames %>%
  group_by(person) %>%
  summarize(count = n()) %>%
  filter(count >= 10)
dataNames <- dataNames %>%
  filter(person %in% notRare$person)

# Plots
ggplot(dataNames) +
  geom_bar(aes(x = person), fill = "salmon")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 75, hjust = 1)) +
  labs( x="Person name",
        y = "Count",
      title= "Frequency of person names in Jane Austen\'s Pride and Prejudice",
      subtitle="Analysis based on Named Entity Recognition, except for Charlotte which had to be added by hand.\n Only names with more than 10 occurences are considered. \n There are wrong names but the recognition seems ok.")
ggsave(file = "people.PNG", width = 12, height = 8)

dataPeople <- data.frame(place = whereVector)
ggplot(dataPeople) +
  geom_bar(aes(x = place), fill = "salmon")+
  theme(text = element_text(size=20),
        axis.text.x = element_text(angle = 75, hjust = 1))+
  labs( x="Location name",
        y = "Count",
        title= "Frequency of location names in Jane Austen\' Pride and Prejudice",
        subtitle="Analysis based on Named Entity Recognition, except for Charlotte which had to be removed by hand.")
ggsave(file = "names.PNG", width = 12, height = 8)

