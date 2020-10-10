#Odczyt dokuemtnów 

#wybranie danych osobowych

#podział na sagmenty 

#Izolowanie informacji z poszczególnych tekstów



#setwd("R/cv/")
dirname <- file.path(".")
library(pdftools)
library(tm)
library(reticulate)
library(textclean)

library(topicmodels)

files <- list.files(path = dirname, pattern = "pdf$")

#opinions <- lapply(files, function(cvfile){
  
  #return  (pdf_text('./cv/'. cvfile))
#} )


opinions <- lapply(files, pdf_text)
#pierwszy problem śmiecowe znaki z pdfów
print(opinions[3])

corp <- Corpus(URISource(files),
               readerControl = list(reader = readPDF, language = "pl"))

meta(corp[[1]])
writeLines(strwrap(corp[[1]]))




library(googleLanguageR)

text <- "to administer medicince to animals is frequently a very difficult matter, and yet sometimes it's necessary to do so"
## translate British into Danish
gl_translate(text, target = "pl")$translatedText







#dodanie własnej funkcji do stemowania pobranie listy 
#niestety nie znalazłem słownika stemmera dla poskiego, inne rozwiązanie szukanie metod innuych


#test funkcji stemmera z python
use_python("/usr/lib/python3.8")
py_run_string("import sys")
py_run_string("print(sys.version)")
#py_run_string("python -m pystempel --default-pip")
py_run_string("print(sys.path)")
py_run_string("from stempel import StempelStemmer")
py_run_string("stemmer = StempelStemmer.polimorf()")
py_run_string("for word in ['książka', 'książki', 'książkami', 'książkowa', 'książkowymi']:  print(stemmer.stem(word))")


stem_list <- function(term) {
  
  termnew <<- term

  changed <<- lapply(term, function(text) {
    if (is.null(text)) {
        return("")
    }
    if (text == "NULL") {
      return("")
    }
    if (text != ""  && !is.null(text)) {
        val <- py$stemmer$stem(text)
        if (is.null(val)) {
         return("")
        }
        return(val)
    } else {
      return(text)
      
    }
  })
  return(as.character(changed)) 

  

}

#trzeba było doistalować pakiet snowball dla poskich znaków, lub zmienic słownik z którego korzysta stopwords

#?stopwords_getlanguages
stw <- stopwords::stopwords("pl", source = "stopwords-iso")
opinions.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(removePunctuation = TRUE,
                                          stopwords = stw,
                                          tolower = TRUE,
                                          stemming = FALSE,
                                          removeNumbers = TRUE,
                                          stripWhitespace = TRUE,
                                          bounds = list(global = c(2, Inf)))) 



ft <- findFreqTerms(opinions.tdm, lowfreq = 1, highfreq = Inf)
#as.matrix(opinions.tdm[ft,])
ft.tdm <- as.matrix(opinions.tdm[ft,])
#plot(opinions.tdm)
listOfWords <- sort(apply(ft.tdm, 1, sum), decreasing =  FALSE)
listOfWords

plot(opinions.tdm, corThreshold = 0.1, weighting = TRUE)





#powiązania miedzy słowami:

findAssocs(opinions.tdm, c("praca"), .40) 


findAssocs(opinions.tdm, c("umiejętność"), .30) 
findAssocs(opinions.tdm, c("programista"), .40) 
findAssocs(opinions.tdm, c("doświadczenie"), .40) 

findAssocs(opinions.tdm, c("umiejętności"), .40) 


my_words <- c("php", "java", "python","js", "css", "programista","html","c","php", "delphi", "docker","photohop")
dtm_programisci <- DocumentTermMatrix(corp, control=list(dictionary = my_words))
plot(dtm_programisci, corThreshold = 0.1, weighting = TRUE)

#wukres
wf <- data.frame(word=names(listOfWords), freq=listOfWords)   

library(ggplot2)   

plot <- ggplot(subset(wf, listOfWords>30), aes(word, freq))    
plot <- plot + geom_bar(stat="identity")   
plot <- plot + theme(axis.text.x=element_text(angle=45, hjust=1))   
plot


#Wizualizacja klastra

library(cluster)   
d <- dist(t(dtm_programisci), method="euclidian")   
fit <- hclust(d=d, method="centroid")   
fit  
plot.new()
plot(fit, hang=-1)
roups <- cutree(fit, k=4)   
rect.hclust(fit, k=4, border="red")# draw dendogram with red borders around the 5 clusters






dataOfSkils <- read.csv("cleaned_related_skills.csv", as.is = FALSE, header= TRUE,  sep = ",",  quote = "")
str(dataOfSkils)


dataOfSkils$state <- apply(dataOfSkils[,1:11], 1,  function(x) {
     for (cel in x)  {
       if (cel %in% c("php", "java", "python","js", "css", "programmer","html","c","php", "delphi", "docker")) {
         return("programmer")
       }
       if (cel %in% c("adobe", "photoshop")) {
         return("graphics")
       }
       return ("seller")
     }
       
})

#nie udało mi sie jeszcze uruchomić modleu i stworzyć DTM i dobrze ponadawać kalasy wierszom.



library(e1071)
library(mlr)
repeating_sequence=rep.int(seq_len(100), 100)
skill_dataset <- dataOfSkils[repeating_sequence,] #powtrzam 100 razy losowanie, losowego indeksu po 100

task = makeClassifTask(data = skill_dataset, target = "state")

#Initialize the Naive Bayes classifier
selected_model = makeLearner("classif.naiveBayes")

#Train the model
NB_mlr = train(selected_model, task)

#Read the model learned  
NB_mlr$learner.model

#Predict on the dataset without passing the target feature
predictions_mlr = as.data.frame(predict(NB_mlr, newdata = skill_dataset[,1:3]))



#Pierwszys problem w zasadzie mam pojedyńcze słowa ale nic to nie mówi w jakim są kontekście i jak ułożyć je w informacje z danej sekcji


