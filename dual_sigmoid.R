dual_sigmoid <- function(x,
    inflect, height_left, height_right, scale_left
){
  inflect <- inflect[1]
  height_left <- height_left[1]
  height_right <- height_right[1]
  scale_left <- scale_left[1]
  ifelse(x < inflect,
    2*height_left*plogis((x-inflect)/scale_left),
    height_left-height_right+
      2*height_right*plogis(height_left/height_right*(x-inflect)/scale_left)
  )
}
pdf('truth_curve_dsigmoid.pdf', width=8, height=5)
dual_sigmoid_midpoint <- function(
    inflect, height_left, height_right, scale_left
){
  inflect <- inflect[1]
  height_left <- height_left[1]
  height_right <- height_right[1]
  scale_left <- scale_left[1]
  mid <- (height_left + height_right)/2
  ifelse(height_left >= height_right,
    qlogis(mid/2/height_left, location = inflect, scale = scale_left),
    qlogis((mid+height_right-height_left)/2/height_right,
           location = inflect, scale = scale_left*height_right/height_left)
  )
}
{
  h1=95;h2=5;sc=5;
  inflect=65*2-dual_sigmoid_midpoint(65,h1,h2,sc)
  curve(dual_sigmoid(x,inflect,h1,h2,sc),from=40,to=90,
        main='Curves with Different Asymmetry and Shape',
        xlab='Age',ylab='Progression%',col='red',n=1001
  )
  abline(v=inflect,col='red',
         lty=3)
}
{
  h1=50;h2=50;sc=5;
  inflect=65*2-dual_sigmoid_midpoint(65,h1,h2,sc)
  curve(dual_sigmoid(x,inflect,h1,h2,sc),from=40,to=90,
        add=TRUE,col='blue',n=1001
  )
  abline(v=inflect,col='blue',
         lty=3)
}
{
  h1=70;h2=30;sc=5;
  inflect=65*2-dual_sigmoid_midpoint(65,h1,h2,sc)
  curve(dual_sigmoid(x,inflect,h1,h2,sc),from=40,to=90,
        add=TRUE,col='darkgreen',n=1001
  )
  abline(v=inflect,col='darkgreen',
         lty=3)
}
{
  curve(50*(plogis((x-65)/3,-4)+plogis((x-65)/3,+4)),from=40,to=90,
        add=TRUE,col='orange',n=1001
  )
}
abline(h=c(0,50,100), lty=2, col='black')
dev.off()