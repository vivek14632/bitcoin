
install.packages('Rfacebook',dependencies = T)
library('Rfacebook')

#create a favebook app and add http://localhost:1410/ to your app
fb_oauth <- fbOAuth(app_id="***",
                    app_secret="***")


fb_page <- getPage(page="narendramodi", token=fb_oauth,n=1000,feed = T,reactions = T)

#emotions
fb_page$love_count
fb_page$haha_count
fb_page$wow_count
fb_page$sad_count
fb_page$angry_count



fb_page$message
fb_page$likes_count

par(mfrow=c(3,1))


plot(log(fb_page$likes_count),type="l")
plot(log(fb_page$shares_count),col="red",type = "b")
plot(log(fb_page$comments_count),col="red",type = "b")


cor(fb_page$shares_count,fb_page$comments_count)
summary(lm(fb_page$shares_count~
             fb_page$likes_count+
             fb_page$comments_count+
             fb_page$love_count+
           fb_page$haha_count+
           fb_page$wow_count+
           fb_page$sad_count+
           fb_page$angry_count))

library('foreign')
library('MASS')
install.packages('RFGLS',dependencies = T)
library('RFGLS')
summary(fgls(fb_page$shares_count~
             fb_page$likes_count+
             fb_page$comments_count+
             fb_page$love_count+
             fb_page$haha_count+
             fb_page$wow_count+
             fb_page$sad_count+
             fb_page$angry_count))


fb_page$likes_count[1]
fb_page$love_count[1]+
fb_page$haha_count[1]+
fb_page$wow_count[1]+
fb_page$sad_count[1]+
fb_page$angry_count[1]


fb_page$love_count[1]
  fb_page$haha_count[1]
  fb_page$wow_count[1]
  fb_page$sad_count[1]
  fb_page$angry_count[1]

