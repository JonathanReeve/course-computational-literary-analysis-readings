---
title: Introduction to Computational Literary Analysis, Summer 2022
author: Jonathan Reeve
date: 2022-07-05
---

Welcome! Here you'll find all the course information for Introduction to Computational Literary Analysis, a course taught at UC-Berkeley, every summer since 2018. 

## Course Details 

  - DIGHUM 150C: Digital Humanities and Text and Language Analysis
  - Summer Session D, 2022 (5 July – 12 August). Online-only this year.
  - Instructor: Jonathan Reeve
  - [Course description via UC-Berkeley](https://classes.berkeley.edu/content/2022-summer-dighum-150c-001-wbl-001)
  - Lecture videos posted asynchronously, Mondays – Thursdays. (Find links in the course schedule below.) 
  - Discussion meetings: Monday–Thursday, at 12:00 noon Berkeley time, on Zulip. 
      - [(12:00 noon in Berkeley, 15:00 in New York)](https://time.is/compare/1900_6_July_2022_in_UTC/Berkeley/New_York/Beijing)
  - Open labs: Fridays, 10:00 to 11:00 Berkeley time. 
      - [(10:00 in Berkeley, 13:00 in New York)](https://time.is/compare/1700_6_July_2022_in_UTC/Berkeley/New_York/Beijing)
      - [Synchronous, here on Jitsi.](https://meet.jit.si/ComputationalLiteraryAnalysis)
  - Email address: jonathan.reeve@columbia.edu (but please communicate with me on Zulip whenever practical)
  - Course chatroom: https://icla2022.zulipchat.com/
  - Course website and course readings: https://icla2022.jonreeve.com
  - Course repository: https://github.com/JonathanReeve/course-computational-literary-analysis

[Get Started!](#getting-started)

## Description

This course is an introduction to computational literary analysis, which presumes no background in programming or computer science. We will cover many of the topics of an introductory course in natural language processing or computational linguistics, but center our inquiries around literary critical questions. We will attempt to answer questions such as:

  - What are the characteristic speech patterns of the narrators in Wilkie Collins's *The Moonstone*?
  - What words are most frequently used to describe Katherine Mansfield's female characters?
  - Which novels of the nineteenth century are the most similar to each other? Which are the most different?

The course will teach techniques of text analysis using the Python programming language. Special topics to be covered include authorship detection (stylometry), topic modeling, and word embeddings. Literary works to be read and analyzed will be Wilkie Collins's *The Moonstone*, Katherine Mansfield's *The Garden Party and Other Stories*, and James Joyce's *Dubliners*.

## Objectives

Although this course is focused on the analysis of literature, and British literature in particular, the skills you will learn may be used to computationally analyze any text. These are skills transferable to other areas of the digital humanities, as well as computational linguistics, computational social science, and the computer science field of natural language processing. There are also potential applications across the humanistic disciplines—history, philosophy, art history, and cinema studies, to name a few. Furthermore, text- and data-analysis skills are widely desired in today's world. Companies like Google and Facebook, for instance, need ways to teach computers to understand their users' search queries, documents, and sometimes books. The techniques taught in this course help computers and humans to understand language, culture, and human interactions. This deepens our understanding of literature, of our fellow humans, and the world around us.

## Prerequisites

This course presumes no prior knowledge of programming, computer science, or quantitative disciplines. Those with programming experience, however, won't find this boring: the level of specialization is such that only the first few weeks cover the basics.

## Course Structure

Although this is usually a classroom-taught course, due to the global pandemic, this course is taught online-only, for the moment. This will require a lot of adaptation from everyone, and it won't be easy. That said, I'll be trying my best to make this course flexible, and doable from different timezones.

### Lecture Videos

In place of in-person lectures, I'll post lecture videos. Each video is between 30-70 minutes long, and is required viewing. Please watch the lecture videos before coming to discussion sections, so that we can all discuss it synchronously. Links will be posted to this syllabus. **Please resist the urge to watch lecture videos too far in advance, since they may change as I revise the course content.**

### Discussion Sections

In place of in-person classroom dialogue and activities, we'll hold discussion sections online, using Zulip, at <https://icla2022.zulipchat.com/>. Zulip is a text-based chat platform, with email-like threading. You can use it to join an existing discussion thread, or create a new one.

Attendance in these discussions is required. If you need to participate asynchronously, for whatever reason, just let me know in advance (on Zulip). Participating asynchronously means that you still engage in our conversations, just at a later point in the day.

As in a traditional classroom, some days you will want to speak (i.e., write in the chatroom) more than others, and that's fine. But please say something thoughtful **at least once per class**. This way there is a record of your participation.

### Course Communication 

Feel free to chime in on the course chat throughout the week, with any questions or comments you might have. I'll usually be there once every couple of days. Please use the public channels for any course-related questions you have, unless they are of a private nature (e.g., grades), in which case please message me privately on Zulip, as I will answer faster there than through email. Discussion about specific textual passages might be better placed in annotations, in the margins of the text, using our annotation platform. See [Annotations](#annotations), below. 

### Labs

These are synchronous videoconferences that happen every week, on Friday, from 12:00--13:00, Berkeley time, [here on Jitsi](https://meet.jit.si/ComputationalLiteraryAnalysis). They are less formal than the discussion sections, and an ideal place to come and chat about the readings and/or programming assignments in real time. I recommended you attempt the homework assignments before coming, so that you can ask any questions you have about them during the lab. You're also welcome to join and just quietly work for the hour. I won't take attendance, but these labs are strongly recommended. 

## Getting Started

To get set up for this course, you will need:

  - Access to a computer that runs Linux, MacOS, or Windows.
  - An Internet connection. I've tried my best to make our course software work as globally as possible, but if you're attending class remotely, from a country that has restricted Internet, you might want to look into setting up a VPN, either through the university, or through a private provider. Please get in touch as soon as possible if you run into any connectivity issues.

Now that we have that, let's get started! First, let's set up a couple of accounts:

1.  Fill out [this short course survey](https://jonreeve.typeform.com/to/m7TpZ6YT), so I can keep track of who's who.
2.  [Create a GitHub account](https://github.com/signup?user_email=&source=form-home-signup). Unless you're already well-established there, **please use your real name (or English name / preferred name, etc) as your username, and add a picture of yourself**.
3.  Use that account to log into [our Zulip chatroom](https://icla2022.zulipchat.com/). (Click "sign up," then "sign up with GitHub.")
4.  Introduce yourself to everyone in the chatroom.
5.  Sign up for a user account on [hypothes.is](https://hypothes.is), our annotation platform.
6.  Download and install *[Anaconda](https://www.anaconda.com/products/individual)*, a Python distribution, which contains a lot of useful data science packages.

## Extra Resources

You will likely need some extra help at some point, either for the literary aspect of the course, or the technological aspect. Don't worry. That's totally normal. Here are a few resources:

### Programming Resources

If you want some extra help, or want to read a little more about some of the things we're doing, there are plenty of resources out there. If you want a second opinion about a question, or have questions that we can't answer in the chatroom, a good website for getting help with programming is [StackOverflow](https://stackoverflow.com). Also, the Internet is full of Python learning resources. One of my favorites is [CodeCademy](https://codecademy.com), which has a game-like interactive interface, badges, and more. There's also the fantastic interactive textbook [How to Think Like a Computer Scientist](https://runestone.academy/runestone/books/published/thinkcspy/index.html), which is the textbook for *Computing in Context,* the introduction to Python at Columbia's Computer Science department.

Resources related to text analysis include, but are by no means limited to:

  - [The NLTK Book](http://www.nltk.org/book/)
  - [My introduction to text analysis tutorial](https://github.com/JonathanReeve/dataviz-workshop-2017)
  - [My advanced text analysis tutorial with SpaCy](https://github.com/JonathanReeve/advanced-text-analysis-workshop-2017)

A colleague and I have also put together a few guides for beginning programming: 

 - [Common Error Messages and How to Fix Them](https://github.com/JonathanReeve/course-computational-literary-analysis/blob/master/Resources/errors.md)
 - [A glossary of frequently used Python terminology](https://github.com/JonathanReeve/course-computational-literary-analysis/blob/master/Resources/frequently_used_terms.md)

### Literary Resources

If you're feeling like you need some help catching up with literary-critical terminology, or traditions of scholarship, here is a list of useful reference volumes, some of which are available online:

- The Broadview and Norton Critical Editions [listed below](#readings).
- [Abrams, *A Glossary of Literary Terms*](https://clio.columbia.edu/catalog/8603870)
- [A Companion to the Victorian Novel](https://clio.columbia.edu/catalog/10624213)
- [The Cambridge Companion to Modernism](https://clio.columbia.edu/catalog/9315439?counter=2)

## Requirements

Coursework falls into three categories:

  - Daily Annotations and Discussions (40% of final grade)
    - Thus: 20% annotations, 20% class discussions. 
  - Homework Assignments (30% of final grade)
  - Final Project (30% of final grade)

And of course, there are three course readings: one novel and two short story collections. **Reading these closely is crucial**: this will allow you to contextualize your quantitative analyses, and will prepare you for the close reading tasks of the final paper.

### Readings

All readings are provided in digital form on the course website. They are one novel and several short stories: 

  - [Wilkie Collins, *The Moonstone*](texts/moonstone.html)
  - [Katherine Mansfield, *The Garden Party and Other Stories*](texts/garden-party.html)
  - [James Joyce, *Dubliners*](texts/dubliners.html)

If you prefer to read on paper, or to supplement your reading with background information and critical articles, I highly recommend the Broadview and Norton Critical Editions. They are full of interesting essays and explanatory notes.

  - Wilkie Collins, *The Moonstone*, Broadview Edition
      - [Available as paperback, pdf, or epub at Broadview Press](https://broadviewpress.com/product/the-moonstone/#tab-description)
  - Katherine Mansfield, *The Garden Party and Other Stories*, in *Katherine Mansfield's Selected Stories*, Norton Critical Edition
      - [Available as *Katherine Mansfield's Selected Stories*, in paperback from Norton Critical Editions](http://books.wwnorton.com/books/webad.aspx?id=11871)
  - James Joyce, *Dubliners*, Norton Critical Edition
      - [Available as paperback from Norton Critical Editions](http://books.wwnorton.com/books/webad.aspx?id=10295)

### Annotations

For each reading assignment, please write **2-3 annotations to our editions of the text**, using [hypothes.is](http://hypothes.is). Links are provided below. You'll have to sign up for a hypothes.is account first. As above, please use your real name as your username, so I know who you are. You may write about anything you want, but it will help your final project to think about ways in which computational analysis might help you to better understand what you observe in the text. Good annotations are:

  - Concise (think: a long tweet)
  - Well-written (although not too formal)
  - Observant (rather than evaluative)

You may respond to another student's annotation for one or two of your annotations, if you want. Just make your responses equally as thoughtful.

### Homework

Four short homework assignments, of around 10 questions each, will be assigned, and are due the following week, on Monday, before our discussion starts. Jupyter notebook templates for each will be provided. **Since we'll review the homework answers at the beginning of each week, late work cannot be accepted**. Please submit homework assignments to me via email—jonathan.reeve@columbia.edu.

Feel free to consult with others, on Zulip, for hints or directions for homework problems. Just don't share any answers verbatim, and make sure that your work is ultimately your own.

### Final Project

The final project should be a literary argument, presented in the form of a short academic paper, created from the application of one or more of the text analysis techniques we have learned toward the analysis of a text or corpus of your choosing. Should you choose to work with a text or corpus other than the ones we've discussed in class, please clear it with me beforehand. Your paper should be a single Jupyter notebook, including prose in Markdown, code in Python, in-text citations, and a bibliography. A template will be provided. The length, not including the code, should be about 2,000 to 3,000 words (I provide [a script you can use to count your words](https://github.com/JonathanReeve/course-computational-literary-analysis/blob/master/Resources/wordcount.py)). You're allowed a maximum of three figures, so produce plots selectively.

During the final week of class, we'll have final project presentations. Your paper isn't required to be complete by then, but you'll be expected to speak about your project for 4 minutes. Consider it a conference presentation.

Final papers will be evaluated according to the:

  - Quality of the literary critical argument presented
  - Quality of the close readings of the text or corpus
  - Quality of the Python text analysis
  - Literary interpretation of the results
  - Integration of the computational analysis with the literary argument

As with homework, please submit these on CourseWorks, or email them to me if you don't have access to CourseWorks. You may optionally submit your final project to the course git repository, making it public, for a 5% bonus.

# Schedule

Note: this schedule is subject to some change, so please check the course website for the most up-to-date version.

## Week 1: Introduction to Python for Text Analysis

- Text: Wilkie Collins, *The Moonstone*
- Tools: Python (Anaconda)

### Unit 1.1. Tuesday, 2022-07-05: Course intro.

  - [Lecture 0: introduction.](https://www.youtube.com/watch?v=yji1aO-NELc)
  - Motivation: what is possible with computational literary analysis?
  - Assignment: please follow all steps in [the "Getting Started" section](#getting-started)

### Unit 1.2. Wednesday, 2022-07-06: Installing Python. Python 2 v. 3. Jupyter. Strings.

  - [Lecture 1: Getting Started](https://www.youtube.com/watch?v=gvGqxGyZBPs) 
  - Reading: [*The Moonstone*, First Period, Through Chapter VII](/texts/moonstone.html#prologue)

### Unit 1.3. Thursday, 2022-07-07: Working with strings, lists, and dictionaries.

  - [Lecture 2: String Methods](https://www.youtube.com/watch?v=g9vOlQ7kiTo) 
  - Reading: [First Period, Through Chapter XI](/texts/moonstone.html#chapter-vii)
  - [Homework 1 assigned. Due Monday, 19:00 UTC (Noon, Berkeley time).](https://github.com/JonathanReeve/course-computational-literary-analysis/blob/master/Homework/Yourname-HW1.ipynb)

## Week 2: Basic Text Analysis

 - Text: *The Moonstone*, Continued
 - Tools: Natural Language ToolKit (NLTK)

### Unit 2.1. Monday, 2022-07-11 

  - [Lecture 3: Lists, Dictionaries, If Statements](https://www.youtube.com/watch?v=EUwFQTEBjPU)
  - Reading: [First Period, Complete.](/texts/moonstone.html#chapter-xi)
  - **Homework 1 due**

### Unit 2.2. Tuesday, 2022-07-12

  - [Lecture 4: Files](https://www.youtube.com/watch?v=WONFR_aSw40) 
  - Reading: [Second Period, First Narrative (Miss Clack)](/texts/moonstone.html#first-narrative)

### Unit 2.3. Wednesday, 2022-07-13

  - [Lecture 5: the NLTK](https://www.youtube.com/watch?v=uuAka56Cm44) 
  - Reading: [Second Period, Second Narrative (Mr. Bruff)](/texts/moonstone.html#second-narrative)

### Unit 2.4. Thursday, 2022-07-14

  - Reading: [Second Period, Third Narrative (Mr. Blake)](/texts/moonstone.html#third-narrative)
  - [Lecture 6: Stems and Lemmas](https://www.youtube.com/watch?v=a1IIuSmPkUQ) 
  - [Homework 2 assigned.](https://github.com/JonathanReeve/course-computational-literary-analysis/blob/master/Homework/Yourname-HW2.ipynb)

## Week 3: Word Frequency Analyses

 - Text: *The Moonstone* and Katherine Mansfield, *The Garden Party and Other Stories*
 - Tools: Scikit-Learn, Pandas

### Unit 3.1 2022-07-18: Pandas and distinctive words.

  - **Homework 2 due**
  - Text: Second Period, Fourth and Fifth Narratives
  - [Lecture 7: Types and Tokens](https://www.youtube.com/watch?v=HhQA__FKxj4)
  - [Lecture 8: Pandas for Word Frequency Analysis. Distinctive words.](https://www.youtube.com/watch?v=frIUMKXBtZI)

### Unit 3.2 2022-07-19: N-grams and narrative-time analysis. 

  - Text: *The Moonstone*, Complete.
  - [Lecture 9: Narrative Time Analysis and N-Grams](https://www.youtube.com/watch?v=jMN1XbMRE6s)

### Unit 3.3 2022-07-20: WordNet and WordNet-based text analysis. Part-of-speech analyses.

  - [Texts: "The Garden Party"](https://icla2022.jonreeve.com/texts/garden-party.html#2-the-garden-party)
  - [Lecture 10: WordNet and WordNet-Based Analysis](https://youtu.be/jKN5HVUGYao)

### Unit 3.4 2022-07-21: Downloading, using, and iterating over corpora.

  - [Texts: "The Daughters of the Late Colonel"](https://icla2022.jonreeve.com/texts/garden-party.html#3-the-daughters-of-the-late-colonel)
  - [Lecture 11: POS cont'd. Corpora.](https://youtu.be/esuj7jOK2lE)
  - [Homework 3 assigned. Due Monday, 7-26 at noon Berkeley time.)](https://github.com/JonathanReeve/course-computational-literary-analysis/blob/master/Homework/Yourname-HW3.ipynb)

## Week 4: Linguistic Techniques I

Text: Katherine Mansfield, *The Garden Party and Other Stories*
Tools: NLTK, SpaCy

### Unit 4.1 2022-07-25: Review of Week 3 and Homework 3. Corpus vectorization with Scikit-Learn. TF-IDF. Stylometry.

  - **Homework 3 due**
  - [Texts: "The Young Girl"](https://icla2022.jonreeve.com/texts/garden-party.html#5-the-young-girl)
  - [Lecture 12: Corpora continued. Scikit-learn.](https://www.youtube.com/watch?v=szJU_q7hnkk)

### Unit 4.2 2022-07-26: Comparative stylometry. Corpus-DB.

  - [Texts: "Marriage à la Mode"](https://icla2022.jonreeve.com/texts/garden-party.html#7-marriage-a-la-mode)
  - [Lecture 13: Stylometry, Corpus-DB](https://www.youtube.com/watch?v=oNZiviyHVbE)

### Unit 4.3 2022-07-27: Stylometry, continued.

  - [Texts: "Her First Ball"](https://icla2022.jonreeve.com/texts/garden-party.html#10-her-first-ball)
  - [Lecture 14: Stylometry Cont'd.](https://www.youtube.com/watch?v=43_Egqq0hsQ)

### Unit 4.4 2022-07-28: Topic modeling with LDA. Quote parsing.

  - [Texts: "An Ideal Family"](https://icla2022.jonreeve.com/texts/garden-party.html#14-an-ideal-family)
  - [Homework 4 assigned. Due Monday by 19:00 UTC (noon in Berkeley).](https://github.com/JonathanReeve/course-computational-literary-analysis/blob/master/Homework/Yourname-HW4.ipynb)
  - [Lecture 15: Topic Modeling](https://www.youtube.com/watch?v=5LKWD3fWyb0)

## Week 5: Linguistic Techniques II

Text: James Joyce, *Dubliners*
Tools: SpaCy

### Unit 5.1 2022-08-01: Review of Week 4 and Homework 4. Using SpaCy. Named entity recognition.

  - **Homework 4 due**
  - [Texts: "The Sisters," "An Encounter"](https://icla2022.jonreeve.com/texts/dubliners.html#the-sisters)
  - [Lecture 17: SpaCy and Named Entity Recognition](https://www.youtube.com/watch?v=OYHBujzwaGw)

### Unit 5.2 2022-08-02: Intro to final project. Sentiment analysis. Macro-etymological analysis.

  - [Texts: "Araby", "Eveline"](https://icla2022.jonreeve.com/texts/dubliners.html#araby)
  - [Lecture 18: Sentiment Analysis and Macro-Etymological Analysis](https://www.youtube.com/watch?v=eYJMOwhNT70)

### Unit 5.3 2022-08-03: Sentence structure analysis using SpaCy.

  - [Texts: "The Boarding House"](https://icla2022.jonreeve.com/texts/dubliners.html#the-boarding-house)
  - [Lecture 19: Sentence Structure Analysis Using SpaCy]()

### Unit 5.4 2022-08-04: Social Network Analysis

  - [Texts: "Clay"](https://icla2022.jonreeve.com/texts/dubliners.html#clay)
  - [Lecture 20: Social Network Analysis.]()

## Week 6: Advanced Topics

Tools: Scikit-Learn, SpaCy

### Unit 6.1 2022-08-08: About the Final Project

### Unit 6.2 2022-08-09: Extras: TEI XML.

 - Lecture 21: Extras: TEI XML

### Unit 6.3 2022-08-10: Extras: Metadata APIs.

  - [Lecture 20: Social Network Analysis](https://us.tv/videos/watch/f0d5cd5c-e162-4c0b-ac4a-1c59bc36c1f0)
  - Reading: ["The Boarding House"](https://icla2022.jonreeve.com/texts/dubliners.html#the-boarding-house)
  - Reading: ["Clay"](https://icla2022.jonreeve.com/texts/dubliners.html#clay)

### Unit 6.4 2022-08-11: Final project presentations. Wrap-up.

 - Final project presentations due by 19:00 UTC. 
 
### 2022-08-12: Final open lab.

### 2022-08-13: Final projects due.

 - [Final project notebooks due, via email, by the end of the day.]
