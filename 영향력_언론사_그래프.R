library(dplyr)
library(rvest)
library(ggplot2)

# 2017년도 1월부터~ 12월까지 네이버 기사 랭킹에 올라온 기사의 정보를 수집한다.

days_range <- seq(from=as.Date('2017-06-01'), to=as.Date("2017-12-31"), by='days')

#수집한 내용들은 데이터프레임에 담는다.
news_list_text_data = data.frame()
news_list_photo_data = data.frame()

# 정해진 날짜를 하루단위로 정보를 가져온다,
for ( i in seq_along(days_range) ){
  print (i)
  news_arrange_list_date = days_range[i];
  news_arrange_list_url = 'http://news.naver.com/main/history/mainnews/list.nhn'
  news_list_date_page_url = paste(news_arrange_list_url,news_arrange_list_date,sep="?date=")
  
  news_list_date_page = read_html(news_list_date_page_url)
  
  # 링크를 타고가면 위쪽에 사진과 함께 나와있는 기사와 아래쪽에 글로 되어있는 기사가 있다.
  # 이 둘을 따로 수집해서 합친다.
  news_list_date_page_photo_count = strsplit(
    news_list_date_page %>% html_nodes('.edit_history_list .eh_navi .eh_page') %>% html_text(trim = TRUE),
    '/')[[1]][2]
  
  news_list_date_page_text_count = strsplit(
    news_list_date_page %>% html_nodes('.mtype_list_wide .eh_navi .eh_page') %>% html_text(trim = TRUE),
    '/')[[1]][2]
  
  # 먼저 위쪽에 나와있는 사진과 함꼐 있는 기사의 정보를 크롤링한다.
  for(i in 1:news_list_date_page_photo_count){
    news_list_photo_url = 'http://news.naver.com/main/history/mainnews/photoTv.nhn'
    news_list_photo_full_url<-paste(news_list_photo_url,news_arrange_list_date,sep="?date=")
    news_list_photo_full_url<-paste(news_list_photo_full_url,1,sep="&page=")
    
    news_list_photo_part = read_html(news_list_photo_full_url, encoding = 'EUC-KR')
    news_list_photo_part_item = news_list_photo_part %>% html_nodes('.edit_history_lst li')
    
    for(y in 1:length(news_list_photo_part_item)){
      
      news_list_photo_item_link = iconv( news_list_photo_part_item[y] %>% html_node('a') %>% html_attr('href'), 'EUC-KR', 'UTF-8')
      news_list_photo_item_title = news_list_photo_part_item[y] %>% html_nodes('.eh_tit') %>% html_text()
      news_list_photo_item_press = news_list_photo_part_item[y] %>% html_nodes('.eh_by') %>% html_text()

      temp = data.frame("title"=news_list_photo_item_title, "item_press"=news_list_photo_item_press,
                        "link"=news_list_photo_item_link)
      # 기사의 제목, 언론사, 링크정보를 데이터프레임에 저장한다.
      news_list_photo_data = rbind(news_list_photo_data,temp)
    }
  }
  
  
  for(e in 1:news_list_date_page_text_count){
    news_list_text_url = 'http://news.naver.com/main/history/mainnews/text.nhn'
    news_list_text_full_url<-paste(news_list_text_url,news_arrange_list_date,sep="?date=")
    news_list_text_full_url<-paste(news_list_text_full_url,e,sep="&page=")
    
    news_list_text_part = read_html(news_list_text_full_url, encoding = 'EUC-KR')
    news_list_text_part_item = news_list_text_part %>% html_nodes('.mlist2 li')
    
    for(h in 1:length(news_list_text_part_item)){
      
      news_list_text_item_link = news_list_text_part_item[h] %>% html_node('a') %>% html_attr('href')
      news_list_text_item_title = news_list_text_part_item[h] %>% html_nodes('a') %>% html_text(trim = TRUE)
      news_list_text_item_press = news_list_text_part_item[h] %>% html_nodes('.writing') %>% html_text()
      temp =data.frame("title"=news_list_text_item_title,
                       "item_press"=news_list_text_item_press,"link"=news_list_text_item_link)
            news_list_text_data = rbind(news_list_text_data,temp)
    }
  }
}

# 가져온 정보들을 엑셀파일에 저장한다.
write.xlsx(unique_news,"news_list_data.xlsx")

# 사진기사와 글기사의 정보를 합친다.
unique_news = rbind(news_list_photo_data,news_list_text_data)

# 언론사의 영향력을 확인하기 위해 어떤 언론사가 있는지 확인한다.
unique_news_press = unique(unique_news$item_press) 
unique_news_num = rep(0,length(unique_news_press))

# 언론사의 기사수를 센다.
for(i in 1:length(unique_news_press)){
  unique_news_num[i]=length(grep(unique_news_press[i],unique_news$item_press))
}

# 언론사 이름과 기사수를 담아서 그래프로 표현한다.
news = data.frame(press =unique_news_press,news_num= unique_news_num)

ggplot(news, aes(x=news_num, y=reorder(press, news_num))) +
  geom_segment(aes(yend=press), xend=0, colour="grey50",size=1) +
  geom_point(size=3, colour="red")+
  theme_bw() +
  theme(panel.grid.major.y = element_blank(),
        legend.position=c(1, 0.55),
        legend.justification=c(1, 0.5))+
  labs(x="네이버 기사수", y="언론사 이름")+
  ggtitle("언론사별 네이버 랭킹 뉴스 기사 수")
  
total_news_num = sum(news$news_num)
percent_num = rep(0,length(news$news_num))
for(i in 1:length(news$news_num)){
  percent_num[i] = news$news_num[i]/total_news_num*100
}

news$percent = percent_num

# 파이차트로 나타낼 때 표현하기 편하게 상위 10개를 제외한 나머지 언론사를 묶어서 표현한다.
copy_news = news[1:10,]
n = length(news$news_num)
others = data.frame("press"="기타",news_num=sum(news$news_num[11:n]),percent=sum(news$percent[11:n]))
copy_news = rbind(copy_news,others)
ggplot(data=copy_news, aes(x="", y=news_num, fill=press)) +
  labs(y="뉴스 개수",x = " ")+
    geom_bar(stat="identity", width=1) + 
    coord_polar(theta="y") + 
    ggtitle("언론사별 네이버 기사 지분율")


