library(stringr);library(ggplot2);library(circlize)

vektor <- c(passwords)



counter <- str_count(vektor[["passwords"]], pattern="Z")

#büyük harfler için hesaplamalar
countAA<-sort(counter, decreasing = T);sum(countAA)
countBB<-sort(counter, decreasing = T);sum(countBB)
countCC<-sort(counter, decreasing = T);sum(countCC)
countDD<-sort(counter, decreasing = T);sum(countDD)
countEE<-sort(counter, decreasing = T);sum(countEE)
countFF<-sort(counter, decreasing = T);sum(countFF)
countGG<-sort(counter, decreasing = T);sum(countGG)
countHH<-sort(counter, decreasing = T);sum(countHH)
countII<-sort(counter, decreasing = T);sum(countII)
countJJ<-sort(counter, decreasing = T);sum(countJJ)
countKK<-sort(counter, decreasing = T);sum(countKK)
countLL<-sort(counter, decreasing = T);sum(countLL)
countMM<-sort(counter, decreasing = T);sum(countMM)
countNN<-sort(counter, decreasing = T);sum(countNN)
countOO<-sort(counter, decreasing = T);sum(countOO)
countPP<-sort(counter, decreasing = T);sum(countPP)
countQQ<-sort(counter, decreasing = T);sum(countQQ)
countRR<-sort(counter, decreasing = T);sum(countRR)
countSS<-sort(counter, decreasing = T);sum(countSS)
countTT<-sort(counter, decreasing = T);sum(countTT)
countUU<-sort(counter, decreasing = T);sum(countUU)
countVV<-sort(counter, decreasing = T);sum(countVV)
countWW<-sort(counter, decreasing = T);sum(countWW)
countXX<-sort(counter, decreasing = T);sum(countXX)
countYY<-sort(counter, decreasing = T);sum(countYY)
countZZ<-sort(counter, decreasing = T);sum(countZZ)

#uniq hesaplamalar
countUnlem<-sort(counter, decreasing = T);sum(countUnlem)
countTýrnak<-sort(counter, decreasing = T);sum(countTýrnak)
countArtý<-sort(counter, decreasing = T);sum(countArtý)
countEksi<-sort(counter, decreasing = T);sum(countEksi)
countYüzde<-sort(counter, decreasing = T);sum(countYüzde)
countVe<-sort(counter, decreasing = T);sum(countVe)
countEsittir<-sort(counter, decreasing = T);sum(countEsittir)
countEt<-sort(counter, decreasing = T);sum(countEt)
countBuyuktur<-sort(counter, decreasing = T);sum(countBuyuktur)
countKucuktur<-sort(counter, decreasing = T);sum(countKucuktur)



#rakamlar için hesaplamalar
count0<-sort(counter, decreasing = T);sum(count0)
count1<-sort(counter, decreasing = T);sum(count1)
count2<-sort(counter, decreasing = T);sum(count2)
count3<-sort(counter, decreasing = T);sum(count3)
count4<-sort(counter, decreasing = T);sum(count4)
count5<-sort(counter, decreasing = T);sum(count5)
count6<-sort(counter, decreasing = T);sum(count6)
count7<-sort(counter, decreasing = T);sum(count7)
count8<-sort(counter, decreasing = T);sum(count8)
count9<-sort(counter, decreasing = T);sum(count9)

counter <- str_count(vektor[["passwords"]], pattern="z")
#küçük harfler için hesaplamalar
countA<-sort(counter, decreasing = T);sum(countA)
countB<-sort(counter, decreasing = T);sum(countB)
countC<-sort(counter, decreasing = T);sum(countC)
countD<-sort(counter, decreasing = T);sum(countD)
countE<-sort(counter, decreasing = T);sum(countE)
countF<-sort(counter, decreasing = T);sum(countF)
countG<-sort(counter, decreasing = T);sum(countG)
countH<-sort(counter, decreasing = T);sum(countH)
countI<-sort(counter, decreasing = T);sum(countI)
countJ<-sort(counter, decreasing = T);sum(countJ)
countK<-sort(counter, decreasing = T);sum(countK)
countL<-sort(counter, decreasing = T);sum(countL)
countM<-sort(counter, decreasing = T);sum(countM)
countN<-sort(counter, decreasing = T);sum(countN)
countO<-sort(counter, decreasing = T);sum(countO)
countP<-sort(counter, decreasing = T);sum(countP)
countQ<-sort(counter, decreasing = T);sum(countQ)
countR<-sort(counter, decreasing = T);sum(countR)
countS<-sort(counter, decreasing = T);sum(countS)
countT<-sort(counter, decreasing = T);sum(countT)
countU<-sort(counter, decreasing = T);sum(countU)
countV<-sort(counter, decreasing = T);sum(countV)
countW<-sort(counter, decreasing = T);sum(countW)
countX<-sort(counter, decreasing = T);sum(countX)
countY<-sort(counter, decreasing = T);sum(countY)
countZ<-sort(counter, decreasing = T);sum(countZ)
sum(unique(counter))

#barchart {a-----z}
barplot(harf_degerleri, xlab="Characters",ylab = "Values", main="[a-z] barchart", names.arg = chars, col="orange", ylim=c(1,1200000))
#barchart {0-----9}
barplot(rakam_degerleri, xlab="Numbers",ylab = "Values", main="[0-9] barchart", names.arg = numbers, col="pink", ylim=c(1,1500000))
#barchar {uniuqe}
barplot(uniq, xlab="Unique chars.",ylab = "Values", main="[unique] barchart", names.arg = uniq_names, col="grey", ylim=c(1,15000))
# büyük harf toplam -> 594064 küçük harf toplam ->9346518

buyuk_harf_topam_sayý <- 594064
kucuk_harf_toplam_sayý <- 9346518

toplam_char_sayý <- buyuk_harf_topam_sayý+kucuk_harf_toplam_sayý

#grafiðe gönderilecek vektör x parametresi  
harf_degerleri = c(sum(countA),sum(countB),sum(countC),sum(countD),sum(countE),sum(countF),sum(countG),sum(countH),sum(countI),sum(countJ),sum(countK),sum(countL),sum(countM),sum(countN),sum(countO),sum(countP),sum(countQ),sum(countR),sum(countS),sum(countT),sum(countU),sum(countV),sum(countW),sum(countX),sum(countY),sum(countZ))
chars = c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z")

rakam_degerleri = c(sum(count0),sum(count1),sum(count2),sum(count3),sum(count4),sum(count5),sum(count6),sum(count7),sum(count8),sum(count9))
numbers = c("0","1","2","3","4","5","6","7","8","9")

uniq = c(sum(countUnlem),sum(countTýrnak),sum(countEksi),sum(countYüzde),sum(countVe),sum(countEsittir),sum(countEt),sum(countBuyuktur),sum(countKucuktur))
uniq_names = c("!","'","-","%","&","=","@",">","<")


upper_chars=c(sum(countAA),sum(countBB),sum(countCC),sum(countDD),sum(countEE),sum(countFF),sum(countGG),sum(countHH),sum(countII),sum(countJJ),sum(countKK),sum(countLL),sum(countMM),sum(countNN),sum(countOO),sum(countPP),sum(countQQ),sum(countRR),sum(countSS),sum(countTT),sum(countUU),sum(countVV),sum(countWW),sum(countXX),sum(countYY),sum(countZZ))
#piechart
pie(harf_degerleri, labels = chars, main="[a-z pie chart]")

#histogram unique
#hist(uniq, breaks = "uniq_names" ,xlab = "Unique chars.", ylab = "Values ", main = paste("Histogram of Unique chars."))


harf_degerleri_total <- sum(harf_degerleri)
upper_chars_total <- sum(upper_chars)


pie_chart_values <-c(harf_degerleri_total,upper_chars_total)

colors <- c("#009E73","#E69F00")
pie(pie_chart_values,main="upper-lower chars.", col=colors, labels=labelss)
label1 <-paste(kucuk_harf_yuzde,"% lower chars.")
label2 <-paste(buyuk_harf_yuzde,"% upper chars.")
labelss <-c(label1,label2)
kucuk_harf_yuzde <- round((100*kucuk_harf_toplam_sayý)/toplam_char_sayý)
buyuk_harf_yuzde <- round((100*buyuk_harf_topam_sayý)/toplam_char_sayý)
# pie chart ex.
library(scales)


df <- data.frame(
  group = c(harf_degerleri_total),
  value = c(upper_chars_total)
)

library(ggplot2)
# Barplot
bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie


library(plotly)

USPersonalExpenditure <- data.frame("Categorie"=rownames(USPersonalExpenditure), USPersonalExpenditure)
data <- USPersonalExpenditure[,c('Categorie', 'X1960')]

p <- plot_ly(pie_chart_values, labels = labelss <-c(label1,label2), values = ~X1960, type = 'pie') %>%
  layout(title = 'United States Personal Expenditures by Categories in 1960',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



#a<-as.data.frame(table(counter));
#vdf_pass <- as.vector(passwords)
#char_pass <- as.character(vdf_pass)

#png(file = "histogram.png")
#hist(data, xlab ="Weight",col = "yellow",border = "blue")
#dev.off()
