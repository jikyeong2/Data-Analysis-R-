library(dplyr)
library(stringr)
library(httr)
library(rvest)

# importing books.csv
#harry potter만 뽑는 경우 상대적으로 오류가 적어 trycatch 대신 Sys.sleep(1)을 사용하였습니다. 
#스크래핑을 할 때마다 순위 변동이 종종 일어나서 모든 페이지 내에 있는 책이
#수집되지 않은 경우도 있습니다.
#모든 페이지에 20개의 책이 있지 않기 때문에 약 1900여개의 책 정보가 수집되었습니다. 
#(ex. 뒤로 갈수록 17~19 개의 책만 나타남)

main_url<-'https://www.goodreads.com'
url<-'https://www.goodreads.com/search?page=1&qid=2rCmfj8jnA&query=harry+potter&tab=books&utf8=%E2%9C%93'

title <- NULL
author <- NULL
rate <- NULL
rate_count <- NULL
review_count<-NULL
review <- NULL

pages<-c(1:1)
as.integer(pages)
for(i in pages){
  
  link<-paste('https://www.goodreads.com/search?page=',i,
              '&qid=2rCmfj8jnA&query=harry+potter&tab=books&utf8=%E2%9C%93',sep="")
  link_list<-read_html(link) %>% html_nodes('a.bookTitle') %>% html_attr('href')
    
  for(j in 1:length(link_list)){
    
    link_paragraph<-read_html(paste(main_url,link_list[j],sep=""))
    
  #title
  link_title<-link_paragraph %>% html_nodes('h1#bookTitle.gr-h1.gr-h1--serif') %>% 
    html_text(T) %>% str_trim
  title<-c(title,link_title)
  
  #author
  link_author<-link_paragraph %>% html_nodes('#bookAuthors span') %>%
    html_nodes('a.authorName') %>% html_text(T) %>% toString()
  author<-c(author,link_author)
  
  #rate
  link_rate<-link_paragraph %>% html_nodes('div#bookMeta') %>% 
    html_nodes('span[itemprop="ratingValue"]') %>% 
    html_text(T)
  link_rate<-as.integer(link_rate)
  rate<-c(rate, link_rate)
  
  #rate_count
  link_rate_count<-link_paragraph %>% html_nodes('div#bookMeta') %>% 
    html_nodes('a.gr-hyperlink') %>% 
    html_nodes('meta[itemprop="ratingCount"]') %>% html_attr('content')
  link_rate_count<-as.integer(link_rate_count)
  rate_count<-c(rate_count, link_rate_count)
  
  #review_count
  link_review_count<-link_paragraph %>% html_nodes('div#bookMeta') %>% 
    html_nodes('a.gr-hyperlink') %>% 
    html_nodes('meta[itemprop="reviewCount"]') %>% html_attr('content')
  link_review_count<-as.integer(link_review_count)
  review_count<-c(review_count,link_review_count)
  
  #review
  link_review<-link_paragraph %>% html_nodes('div#descriptionContainer') %>% 
    html_nodes('span') %>% html_text %>% str_trim
  link_review<-link_review[1]
  link_review<-gsub('\\s+',' ',link_review)
  review<-c(review,link_review)
  cat(i,"-page",j, "-th book\n")
  Sys.sleep(1)
  
  }

}

books<-data.frame(title,author,rate,rate_count,review_count,review)

View(books)
save(books, file = "books")
write.csv(books, file = "books.csv")

#multibooks

# changing keyword
key_word<-c("harry potter","the chronicles of narnia","the lord of the rings")
url<-'https://www.goodreads.com/search?page=1&qid=2rCmfj8jnA&query=harry+potter&tab=books&utf8=%E2%9C%93'

link1<-modify_url(url,query=(list(query = key_word[1])))
link2<-modify_url(url,query=(list(query = key_word[2])))
link3<-modify_url(url,query=(list(query = key_word[3])))
total_link<-c(link1, link2, link3)
total_link

#importing multibooks
#오류를 줄이기 위해 tryCatch 문을 사용하였습니다. 
#각 키워드마다 가지고 있는 페이지 수가 달라 각 그 숫자를 넘어가면 
#'invalid conversion, skip the page/book'이 나타납니다. 
#또한 스크래핑을 할 때마다 순위 변동이 종종 일어나서 모든 페이지 내에 있는 책이
#수집되지 않은 경우도 있습니다. 

main_url<-'https://www.goodreads.com'
first_url<-'https://www.goodreads.com/search?page='
mid_url<-'&qid=2rCmfj8jnA&query='
last_url<-'&tab=books&utf8=%E2%9C%93'


multibooks<-data.frame()
nbooks<-1

pages<-c(1:100)
as.integer(pages)
keywords<-c("harry+potter","the+chronicles+of+narnia","the+lord+of+the+rings")

for(c in keywords){
  
  for(a in pages){
    
    tryCatch({
      
      web<-paste(first_url,a, mid_url ,c, last_url ,sep="")
      web_list<-read_html(web) %>% html_nodes('a.bookTitle') %>% html_attr('href')
      
      for(b in 1:length(web_list)){
        
        cat("Processing",b,"-th book of ", a, "-th page ", "of-", c, "\n", sep="")
        
        tryCatch({    
          web_paragraph<-read_html(paste(main_url,web_list[b],sep=""))
          
          #title
          tryCatch({
            web_title<-web_paragraph %>% html_nodes('h1#bookTitle.gr-h1.gr-h1--serif') %>% 
              html_text(T) %>% str_trim
          }, error = function(e){web_title<-NULL})
          
          #author
          tryCatch({
            web_author<-web_paragraph %>% html_nodes('#bookAuthors span') %>%
              html_nodes('a.authorName') %>% html_text(T) %>% toString()
          }, error = function(e){web_author<-NULL})
          
          #rate
          tryCatch({
            web_rate<-as.integer(web_rate<-web_paragraph %>% html_nodes('div#bookMeta') %>% 
                                   html_nodes('span[itemprop="ratingValue"]') %>% 
                                   html_text(T))}, error = function(e){web_rate<-NULL})
          
          #rate_count
          tryCatch({
            web_rate_count<-as.numeric(web_rate_count<-web_paragraph %>% html_nodes('div#bookMeta') %>% 
                                         html_nodes('a.gr-hyperlink') %>% 
                                         html_nodes('meta[itemprop="ratingCount"]') %>% 
                                         html_attr('content'))}, error = function(e){web_rate_count<-NULL})
          
          #review_count
          tryCatch({
            web_review_count<-as.numeric(web_review_count<-web_paragraph %>% html_nodes('div#bookMeta') %>% 
                                           html_nodes('a.gr-hyperlink') %>% 
                                           html_nodes('meta[itemprop="reviewCount"]') %>% 
                                           html_attr('content'))}, error = function(e){web_review_count<-NULL})
          
          #review
          tryCatch({
            web_review<-web_paragraph %>% html_nodes('div#descriptionContainer') %>% 
              html_nodes('span') %>% html_text %>% str_trim
            web_review<-web_review[1]
            web_review<-gsub('\\s+',' ',web_review)}, error = function(e){web_review<-NULL})
          
          multibooks[nbooks,1]<-web_title
          multibooks[nbooks,2]<-web_author
          multibooks[nbooks,3]<-web_rate
          multibooks[nbooks,4]<-web_rate_count
          multibooks[nbooks,5]<-web_review_count
          multibooks[nbooks,6]<-web_review
          nbooks <- nbooks + 1
          
        }, error = function(e){print("Invalid conversion, skip the book")})
      }
    }, error = function(e){print("Invalid conversion, skip the page")})
  }
}

names(multibooks)<-data.frame("title","author","rate","rate_count","review_count","review")

View(multibooks)
save(multibooks, file = "multibooks")
write.csv(multibooks, file = "multibooks.csv")
