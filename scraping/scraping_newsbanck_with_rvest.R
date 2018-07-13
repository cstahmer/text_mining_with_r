#Loading the rvest package
library('rvest')
library('httr')
library('XML')

#Specifying the url for desired website to be scrapped
url <- 'http://infoweb.newsbank.com/resources/search/nb?p=AWNB&b=results&action=search&t=country%3AUSA%21USA&fld0=alltext&val0=dissent&bln1=AND&fld1=alltext&val1=protest&sort=YMD_date%3AD'

#Reading the HTML code from the website
webpage <- read_html(url)



# running as a session
session <- html_session("http://infoweb.newsbank.com/resources/search/nb?p=AWNB&b=results&action=search&t=country%3AUSA%21USA&fld0=alltext&val0=dissent&bln1=AND&fld1=alltext&val1=protest&sort=YMD_date%3AD", set_cookies(
  'sugar'='Q60L51EMMTUyNDE0OTE2MC43MjIxMzQ6MToxMzoxNjkuMjM3Ljc1Ljky'
  , 'mp_abbec5ec04749d00a93656609a9af3dc_mixpanel'='%7B%22distinct_id%22%3A%20%22162da9d93998cd-0d2c230cc45665-33697b07-13c680-162da9d939a4e7%22%2C%22%24search_engine%22%3A%20%22google%22%2C%22%24initial_referrer%22%3A%20%22https%3A%2F%2Fwww.google.com%2F%22%2C%22%24initial_referring_domain%22%3A%20%22www.google.com%22%2C%22ip%22%3A%20%2263.157.110.98%22%2C%22%24browser%22%3A%20%22Firefox%22%2C%22%24os%22%3A%20%22Mac%20OS%20X%22%2C%22%24device%22%3A%20%22%22%2C%22%24referrer%22%3A%20%22https%3A%2F%2Fwww.newsbank.com%2F%22%2C%22%24referring_domain%22%3A%20%22www.newsbank.com%22%7D'
  , '_gid'='GA1.3.1793231829.1524086183'
  , '_ga'='GA1.3.1451833754.1524086183'
  , 'has_js'='1'
  , 'SESSf5a897cf3b8f2872d0e6fe6c381635b0'='0bdHuuMdp2-t4tw8Idixe7cT5FybRx0k07eIKUJNtGk'
  , 'JServSessionIdinfoweb'='gvratevvd2.JS61b'
  , '_gid'='GA1.2.1793231829.1524086183'
  , '_ga'='GA1.2.1451833754.1524086183'))

links <- html_nodes(session, "li .last a")
linksText <- html_nodes(session, ".doc-action")
linksText <- html_nodes(session, "a[data-action='print']")


pdffile <- read_html("http://infoweb.newsbank.com/resources/doc/print?p=AWNB&amp;docrefs=news/16B3BF360C480B00")
fileconts <- html_nodes(pdffile, "body")
filecontstwo <- html_nodes(pdffile, "html")
filecontstextOne <- html_text(fileconts)
filecontstextTwo <- html_text(filecontstwo)
filecontfinal <- paste(filecontstextOne, filecontstextTwo, sep="\n")

write(filecontfinal, file="/Users/cstahmer/Desktop/test_paper_output.pdf")


               
               