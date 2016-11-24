# Seq2Logo
This small R package help user to draw PWM logo for just one sequence. I wrote this package to use PWM logos in convolutional neural network. Please use the commands below to install and use. It utilizes SeqLogo, a R package to genereate logo from PWM.   

```.{r}
devtools::install_github ("satyausc/Seq2Logo") 
seq = "AACCTTGG"
Seq2Logo::generateSeqLogoForAsequence(seq, "pwm.png")
```

