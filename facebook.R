
install.packages('Rfacebook',dependencies = T)
library('Rfacebook')

#create a favebook app and add http://localhost:1410/ to your app
fb_oauth <- fbOAuth(app_id="***",
                    app_secret="****")

save(fb_oauth, file="fb_oauth")
load("fb_oauth")
me <- getUsers("me", token=fb_oauth)



dd=getFriends(token=fb_oauth)
fb_page <- getPage(page="GreenBenchBrewing", token=fb_oauth,n=1000)
fb_page$likes_count
plot(fb_page$likes_count[1:100],type="b")
lines(fb_page$comments_count[1:100],col="blue")
lines(fb_page$shares_count[1:100],col="red")

summary(lm(fb_page$shares_count~fb_page$likes_count+fb_page$comments_count))


