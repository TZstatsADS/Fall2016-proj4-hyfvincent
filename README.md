# Project: Words 4 Music

### [Project Description](doc/Project4_desc.md)

![image](http://cdn.newsapi.com.au/image/v1/f7131c018870330120dbe4b73bb7695c?width=650)

Term: Fall 2016

+ [Data link](https://courseworks2.columbia.edu/courses/11849/files/folder/Project_Files?preview=763391)-(**courseworks login required**)
+ [Data description](doc/readme.html)
+ Contributor's name: Yifei Hu 
+ Projec title: Music association rule mining of lyrics and songs
+ Project summary: Assign Latent Dirichlet allocation model (LDA) on the frequencies table of the lyrics, choose the number of topic you want and then get the log of conditional probabilities of the words in the dictionary given each topic and the conditional probabilities of the topics given each song. Use multinomial logistic regression to get probabilities assigned to each topic of each song. And use these probabilities and conditional probabilities of the words in the dictionary given each topic to get the probabilities of each word in the dictionary appearing in the song.

	
Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── lib/
├── data/
├── doc/
├── figs/
└── output/
```

Please see each subfolder for a README file.
