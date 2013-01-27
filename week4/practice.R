#Colors
#grDevices package에 colorRamp, colorRampPalette
#red와 blue로 파레트를 만듬. 0~1사이 숫자로 color값을 가져옴. 
pal<-colorRamp(c("red","blue"))
pal(0)
pal(1)
pal(0.5)

#colorRampPalette는 pal에 넘겨준 숫자만큼 파레트로 만든 색들을 넘김. 색들은 시퀀셜하게 정해진다..
pal <- colorRampPalette(c("red","yellow"))
pal(2)
pal(10)

#RColorBrewer Package는 Sequential, Diverging, Qualitative 세가지 타입의 파레트를 제공
#colorRamp()또는 colorRampPalette()를 같이 써서 색 생성.
library(RColorBrewer)
cols<-brewer.pal(3,"BuGn")
cols
pal<-colorRampPalette(cols)
image(volcano,col=pal(20))

#smoothScatter
x<-rnorm(10000)
y<-rnorm(10000)
smoothScatter(x,y)

#transparency
plot(x,y,col=rgb(0,0,0,0.2),pch=19)
#


#Dates and Times

x<-as.Date("1970-01-01")
x

unclass(x)

unclass(as.Date("1970-01-02"))

#POSIXct와 POSIXlt는 TIME을 표현하는 class
#POSIXct는 unclass하면 정수 숫자
#POSIXlt는 unclass하면 year, week, day 등등 이름을 가진 list. $으로 그 값들 호출.

#generic functions
#weekdays()
#months()
#quarters()

x<-Sys.time()
x

p<- as.POSIXlt(x)
names(unclass(p))

p$sec

x<-Sys.time()
unclass(x)

x$sec

p<-as.POSIXlt(x)
p$sec

datestring<-c("January 10, 2012 10:40", "December 9, 2012")
x<strptime(datestring, "%B %d, %Y %H:%M")
x

class(x)

?strptime?s

x<-as.Date("2012-01-01")
y<-strptime("9 Jan 2011 11:34:21", "%d %b %Y %H:%M:%S")
x-y

x<-as.POSIXlt(x)
x-y

#keep track leap years, leap seconds, daylight savings, and time zones.
x <- as.Date("2012-03-01")
y <- as.Date("2012-02-28")
x-y

x<-as.POSIXct("2012-10-25 01:00:00")
y<-as.POSIXct("2012-10-25 06:00:00", tz="GMT")
y-x

#grep, grepl
#grep은 패턴과 일치하는 line integer arrary 리턴
homicides <- readLines("homicides.txt")
length(grep("iconHomicideShooting", homicides))
#value TRUE이면 Character arrary 리턴
grep("^New", state.name, value = TRUE)
#grepl 은 logical vector 리턴
grepl("^New", state.name)

#regexpr, gregexper
#regexpr는 시작 index 정보 뿐만아니라 끝 index까지의 길이 정보도 줌.
regexpr("<dd>[F|f]ound(.*)</dd>", homicides[1:10])
substr(homicides[1], 177, 177 + 93 - 1)

#greedy방식 쓰지말기
regexpr("<dd>[F|f]ound(.*?)</dd>", homicides[1:10])
substr(homicides[1], 177, 177 + 33 - 1)

#substr대신에 regmatches써서 regexr 값 넘겨주기
r <- regexpr("<dd>[F|f]ound(.*?)</dd>", homicides[1:5])
regmatches(homicides[1:5], r)

#sub, gsub
#sub는 패턴과 일치하는 첫번째 부분만 replace
#gsub는 패턴과 일치하는 모든 부분 replace

x <- substr(homicides[1], 177, 177 + 33 - 1)
sub("<dd>[F|f]ound on |</dd>", "", x)
gsub("<dd>[F|f]ound on |</dd>", "", x)

#regexec
#regexpr처럼 작동. 하지만 괄호 부분의 indices도 제공
regexec("<dd>[F|f]ound on (.*?)</dd>", homicides[1])
substr(homicides[1], 177, 177 + 33 - 1)
substr(homicides[1], 190, 190 + 15 - 1)

r <- regexec("<dd>[F|f]ound on (.*?)</dd>", homicides)
m <- regmatches(homicides, r)
dates <- sapply(m, function(x) x[2])
dates <- as.Date(dates, "%B %d, %Y")
hist(dates, "month", freq = TRUE)

#class와 methods
#S3, S4 class가 있음. S3 오래되고 dirty함. 하지만 쉽고 빠름. S4 formal함 새거.
#method는 generic function의 특정 class의 object의 implementation. generic은 computation을 안함. 해당 class의 method로 넘겨주는 기능.

#generic function mean을 implementate한 class보기
methods("mean")

#Polygon Class
setClass("polygon",
         representation(x = "numeric",
                        y = "numeric"))

setMethod("plot", "polygon",
          function(x, y, ...) {
            plot(x@x, x@y, type = "n", ...)
            xp <- c(x@x, x@x[1])
            yp <- c(x@y, x@y[1])
            lines(xp, yp)
          })

showMethods("plot")

p <- new("polygon", x = c(1, 2, 3, 4), y = c(1, 2, 3, 1))
plot(p)