# Using NLP to Study for Jeopardy!

## Background

One of the pastimes I picked up over the pandemic was watching *Jeopardy!*. It started with old episodes on Netflix, but once taping of new episodes resumed, I became a regular viewer in time to catch the recent legends like Matt Amodio and Amy Schneider.

As all *Jeopary!* enthusiasts are prone to do, I thought to myself, "I bet I could do that if I put my mind to it." I had enough sense to know I'd get crushed on regular *Jeopardy!*, but I decided to take the test for the college tournament. I ended up scoring well enough to qualify for a follow-up test, which I in turn did well enough on to be invited to a "live" (held over Zoom) audition for the college tournament. Making it to this stage meant that I'd be in the pool of potential contestants for the next 12-18 months. All there was left to do was wait for "The Call," which may or may not ever come.

Making it to that point was both exciting and terrfying. If making it past the first test made it easy to feel good about all the things I knew, getting to be a phone call away from actually being on *Jeopardy!* made it easy to fixate on all the things I knew nothing about. I didn't know much poetry and hadn't read much Shakespeare. I could name most of the "Best Picture" winners from my lifetime but would draw a blank on any from before about 2000. If there was a chance of actually getting to be a contestant, I'd have to do some serious practice.

Around the same time, I came across a Reddit thread linking to this Medium article by Colin Davy: [How I Won Jeopardy With Data Science](https://colindavy.medium.com/how-i-won-jeopardy-with-data-science-c2e9b52a1958). In the article, Davy describes getting the "The Call" to appear on *Jeopardy!* with only about two weeks to prepare. Rather than reading through past questions one-by-one on [J! Archive](j-archive.com), Davy used Python to scrape 400,000 past questions and answers from the site. He then identified the most common answers to questions in areas he as weakest in (e.g. the Bible) and used natural language processing (NLP) to strip each question down to its most important words. Those words could then form word clouds, each of which would be associated with a common answer. This way, Davy could learn what words and topics to associate with different answers that often come up on the show. For example, this approach taught him that any clue referencing a "wise king" was probably referring to King Solomon.

I thought this approach was clever, and as a Statistics major with some coding and data science experience, I thought I could replicate it pretty easily. Although my background included more R than Python, I was still able to write a script that generated word clouds for topics commonly occuring on College *Jeopardy!*. I unfortunately never actually got The Call, but this project still gave me good exposure to NLP and vastly expanded my reserves of random knowledge.

## My Approach

I started by downloading a dataset of almost 350,000 past questions from regular *Jeopardy!* and specialty tournaments. To make my studying more efficient, I figured I could look only at topics that commonly come up in college tournaments, topics that lean more heavily towards school subjects like "American History" or "Books & Authors." Using R, I found the nearly 2,500 categories that have ever appeared in college tournaments and sorted them from most to least common. Then, I took the most common categories, looked in the full 350,000-question dataset for every time each category has appeared, and found the most common answers to questions in each category. For example, the most common topics to ask about in American history are Texas, the Louisiana Purchase, and Cuba.

Once I had a list of common answers in a category, my script looped through each answer and found every question that's ever been asked it, cleaned the text of each question, and held onto the most useful words and phrases associated with each topic. The text cleaning step included removing numbers, punctuation, and trailing/leading whitespace; converting every word to lower-case; and removing common uninformative words like "a," "an," and "the."

Once the text of each question was clean, the script counted up how often a word appeared in questions about each topic. I then sorted each word from most to least common. For example, some the most common words in clues about Benjamin Franklin are "American," "Pennsylvania," and "newspaper."

Remember that the goal of this project was to learn a lot of new information about unfamiliar trivia topics as efficiently as possible. Getting lists of the words associated with each common topic technically accomplished this goal, but the lists weren't very easy on the eyes and probably were'nt themost conducive to absorbing information. Instead, like Colin Davy, I turned each of these lists into a word cloud. Each cloud displays the most common words and makes the most common words the biggest and the least common words the smallest. In addition, the words are colored for better visibility. This is the word cloud for Benjamin Franklin:

<p align = "center">
<img src = https://github.com/jonah916/Jeopardy-NLP/blob/main/wordcloud_benjamin%20franklin.png>
</p>

Right away, we can see that Ben Franklin was an American statesman who lived in Philadelphia. A quick google for "ben franklin printer" tells us that he was a successful printer and publisher, founding the nation's first newspaper chain, the *Pennsylvania Gazette*. With just a few seconds of looking at the word cloud, we learned what *Jeopardy!* considers to be the most important things associated with Benjamin Franklin.

I used my script to repeat this process for more than a dozen of the categories I knew the least about, which produced over 1,000 word clouds for common topics. In addition, I wanted a convenient way to study the "flashcards" I was making, so I decided to make my word clouds compatible with the flashcard software [Anki](https://apps.ankiweb.net/). Uploading the clouds made it easy to study topics like "Islands" and "Poets & Poetry" and figure out which categories I was making progress on and which needed more work.

## Reflections

This project was good practice working with text data. It encouraged me to read up on NLP, different methods of cleaning text, and best practices for getting the most out of non-numeric data.

It also gave me a chance to practice writing flexible code that could adapt to different needs. My final script was basically just a single function that takes two major inputs: the name of a category to look at (or, technically, a group of category names as defined by a regex string) and a list of uninformative words to ignore; for example, the word "poet" in the category "Poets & Poetry."

The only thing I would have done differently would have been to challenge myself to scrape the data directly from J! Archive using R's Rvest package. I made a few initial attempts to scrape the data, but ultimately decided it would be easier to just use a pre-made dataset someone had posted on Kaggle.

## Acknowledgements

I owe credit for basically this entire project to Colin Davy. He was the sole source of inspiration for it, having been both creative enough to use this approach in his own *Jeopardy!* training and gracious enough to share his story online. If a recruiter ever notices me because of this project, I'll Venmo him for a nice dinner. 
