### Inauguaral speeches
main.page <- read_html(x = "http://www.presidency.ucsb.edu/inaugurals.php")
# Get link URLs
# f.speechlinks is a function for extracting links from the list of speeches. 
inaug=f.speechlinks(main.page)
#head(inaug)
#as.Date(inaug[,1], format="%B %e, %Y")
inaug=inaug[-nrow(inaug),] # remove the last line, irrelevant due to error.

#### Nomination speeches
main.page=read_html("http://www.presidency.ucsb.edu/nomination.php")
# Get link URLs
nomin <- f.speechlinks(main.page)
#head(nomin)
#
#### Farewell speeches
main.page=read_html("http://www.presidency.ucsb.edu/farewell_addresses.php")
# Get link URLs
farewell <- f.speechlinks(main.page)
#head(farewell)

inaug.list=read.csv("../data/inauglist.csv", stringsAsFactors = FALSE)
nomin.list=read.csv("../data/nominlist.csv", stringsAsFactors = FALSE)
farewell.list=read.csv("../data/farewelllist.csv", stringsAsFactors = FALSE)

speech.list=rbind(inaug.list, nomin.list, farewell.list)
speech.list$type=c(rep("inaug", nrow(inaug.list)),
                   rep("nomin", nrow(nomin.list)),
                   rep("farewell", nrow(farewell.list)))
speech.url=rbind(inaug, nomin, farewell)
speech.list=cbind(speech.list, speech.url)

# Loop over each row in speech.list
speech.list$fulltext=NA
for(i in seq(nrow(speech.list))) {
        text <- read_html(speech.list$urls[i]) %>% # load the page
                html_nodes(".displaytext") %>% # isloate the text
                html_text() # get the text
        speech.list$fulltext[i]=text
        # Create the file name
        filename <- paste0("../data/fulltext/", 
                           speech.list$type[i],
                           speech.list$File[i], "-", 
                           speech.list$Term[i], ".txt")
        sink(file = filename) %>% # open file to write 
                cat(text)  # write the file
        sink() # close the file
}

speech1=paste(readLines("../data/fulltext/SpeechDonaldTrump-NA.txt", 
                        n=-1, skipNul=TRUE),
              collapse=" ")
speech2=paste(readLines("../data/fulltext/SpeechDonaldTrump-NA2.txt", 
                        n=-1, skipNul=TRUE),
              collapse=" ")
speech3=paste(readLines("../data/fulltext/PressDonaldTrump-NA.txt", 
                        n=-1, skipNul=TRUE),
              collapse=" ")

Trump.speeches=data.frame(
        President=rep("Donald J. Trump", 3),
        File=rep("DonaldJTrump", 3),
        Term=rep(0, 3),
        Party=rep("Republican", 3),
        Date=c("August 31, 2016", "September 7, 2016", "January 11, 2017"),
        Words=c(word_count(speech1), word_count(speech2), word_count(speech3)),
        Win=rep("yes", 3),
        type=rep("speeches", 3),
        links=rep(NA, 3),
        urls=rep(NA, 3),
        fulltext=c(speech1, speech2, speech3)
)

speech.list=rbind(speech.list, Trump.speeches)

sentence.list=NULL
for(i in 1:nrow(speech.list)){
        sentences=sent_detect(speech.list$fulltext[i],
                              endmarks = c("?", ".", "!", "|",";"))
        if(length(sentences)>0){
                emotions=get_nrc_sentiment(sentences)
                word.count=word_count(sentences)
                # colnames(emotions)=paste0("emo.", colnames(emotions))
                # in case the word counts are zeros?
                emotions=diag(1/(word.count+0.01))%*%as.matrix(emotions)
                sentence.list=rbind(sentence.list, 
                                    cbind(speech.list[i,-ncol(speech.list)],
                                          sentences=as.character(sentences), 
                                          word.count,
                                          emotions,
                                          sent.id=1:length(sentences)
                                    )
                )
        }
}

sentence.list=
        sentence.list%>%
        filter(!is.na(word.count))

sel.comparison <- c()
for(i in 1:length(unique(speech.list$President))){
        sel.comparison[i] <- unique(speech.list$File)[i]
}
sel.comparison <- sel.comparison[-c(59:61)]