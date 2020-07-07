---
title: Introduction to Computational Literary Analysis, Summer 2020
author: Jonathan Reeve
date: 2020-07-06
---

Welcome! Here you'll find all the course information for Introduction to Computational Literary Analysis, a course taught at UC-Berkeley in summer 2018, 2019, and now 2020.

## Course Details 

  - DIGHUM 150C: Digital Humanties and Text and Language Analysis
  - Summer Session D, 2020 (6 July – 14 August). Online-only this year.
  - Instructor: Jonathan Reeve
  - [Course description via UC-Berkeley](https://classes.berkeley.edu/content/2020-summer-dighum-150c-001-sem-001)
  - Lectures posted asynchronously, Mondays – Thursdays, around 12:00 UTC.
  - Discussion Section A, Mondays, Wednesdays: 19:00 UTC on Zulip
      - [(12:00 noon in Berkeley, 15:00 in New York)](https://time.is/compare/1900_6_July_2020_in_UTC/Berkeley/New_York/Beijing)
  - Discussion Section B, Tuesdays, Thursday: 1:00 UTC on Zulip
      - [(18:00 in Berkeley, 21:00 in New York)](https://time.is/compare/100_6_July_2020_in_UTC/Berkeley/New_York/Beijing)
  - Open labs: Fridays, 19:00-21:00 UTC
      - Synchronous, in a videoconference room to be announced
  - Email address: jonathan.reeve@columbia.edu
  - Course chatroom: https://cla.zulipchat.com/
  - Course website and course readings: https://icla2020.jonreeve.com
  - Course repository: https://gitlab.com/digitalhumanitiesatberkeley/computational-literary-analysis

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

This course presumes no prior knowledge of programming, computer science, or quantitative disciplines. Those with programming experience, however, won't find this boring: the level of specialization is such that only the first week covers the basics.

## How this Course is Structured

Although this is usually a classroom-taught course, and is usually taught on UC-Berkeley's campus, due to the global pandemic, this course is taught online-only this year. This will require a lot of adaptation from everyone, and it won't be easy. That said, I'll be trying my best to make this course available to those in timezones other than Berkeley's. I myself will be teaching from New York City, and so my own timezone is slightly different, too.

### Lecture Videos

In place of in-person lectures, I'll post lecture videos, every day from Mondays to Thursdays. Each is around 45-60 minutes each, and is required viewing. Please watch lecture videos before coming to discussion sections. Links will be posted to this syllabus.

### Discussion Sections

In place of in-person classroom dialogue and activities, we'll hold discussion sections online, using Zulip, at <https://cla.zulipchat.com/>. Zulip is a text-based chat platform, with email-like threading. You can use it to join an existing discussion thread, or create a new one.

I'll be on Zulip every day, for one hour each day:

  - Discussion Section A, Mondays, Wednesdays: 19:00–20:00 UTC on Zulip
  - Discussion Section B, Tuesdays, Thursdays: 1:00–2:00 UTC on Zulip

Try to pick a section that you can attend synchronously (that is, in real-time), and participate in the other asynchronously (on your own time, at your convenience). If you can't attend either of these in real time, please let me know on Zulip.

Discussion about the texts themselves, if they are specific to a particular passage, might be better placed in annotations, in the margins of the text, using our annotation platform. See *Annotations*," below.

### Open Labs

Although not required, these are informal, synchronous videoconferences that happen every week, on Friday. They're a good time and virtual place to come with your homework questions, or just to hang out and work on your own, or in groups. At Berkeley last summer, we'd just invite everyone to the D-Lab, and have pizza, and hang out and code. This year, you'll have to bring your own pizza, sadly. But we can still code together, exchange coding tips, and talk about the readings. We'll also have guests from other courses. Everyone is welcome.

## Getting Started

To get set up for this course, you will need:

  - A computer that runs Linux, MacOS, or Windows.
  - An Internet connection. I've tried my best to make our course software work as globally as possible, but if you live in a country that has restricted Internet, you might want to look into setting up a VPN, either through Berkeley, or using a private provider.

Now that we have that, let's get started! First, let's set up a couple of accounts:

1.  Fill out [this short course survey](https://jonreeve.typeform.com/to/fZZbj6CS), so I can keep track of who's who.
2.  [Create a GitLab account](https://gitlab.com/users/sign_in). Unless you're already well-established there, **please use your real name (or English name / preferred name, etc) as your username**.
3.  Use that account to log into [our Zulip chatroom](https://cla.zulipchat.com/). (Click "sign up," then "sign up with GitLab.")
4.  Introduce yourself to everyone in the chatroom.
5.  Sign up for a user account on [hypothes.is](https://hypothes.is), our annotation platform.
6.  Download and install *[Anaconda](https://www.anaconda.com/products/individual)*, a Python distribution, which contains a lot of useful data science packages.

## Course Communication

Of course, the best place to ask first is in the course chatroom, Zulip: <http://cla.zulipchat.com>. Feel free to start a new topic there for any questions you might have, especially those that you think might be able to be answered by other students. Check out what's happening there as often as you can, and ask any questions you have there, first. You'll probably want to sign up for Zulip with a GitLab username, so make yourself an account there if you don't already have one. Unless you're already well established on GitLab, please use your real name as your GitLab/Zulip username. (Mine is JonathanReeve, for example.)

## Extra Resources

If you want some extra help, or want to read a little more about some of the things we're doing, there are plenty of resources out there. If you want a second opinion about a question, or have questions that we can't answer in the chatroom, a good website for getting help with programming is [StackOverflow](https://stackoverflow.com). Also, the Internet is full of Python learning resources. One of my favorites is [CodeCademy](https://codecademy.com), which has a game-like interactive interface, badges, and more. If you like a good puzzle, and like being challenged, there's also the older [Python Challenge](http://pythonchallenge.com).

Resources related to text analysis include, but are by no means limited to:

  - [The NLTK Book](http://www.nltk.org/book/)
  - [My introduction to text analysis tutorial](https://github.com/JonathanReeve/dataviz-workshop-2017)
  - [My advanced text analysis tutorial with SpaCy](https://github.com/JonathanReeve/advanced-text-analysis-workshop-2017)

## Requirements

Coursework falls into three categories:

  - Daily Annotations (30% of final grade)
  - Weekly Homework (40% of final grade)
  - Final Project (30% of final grade)

And of course, there are three course readings: one novel and two short story collections. Reading these closely will help you to contextualize the quantitative analyses, and will prepare you for the close reading tasks of the final paper.

### Readings

All readings are provided in digital form on the course website. They are one entire novel and a few selected short stories: 

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

For each reading assignment, please write 3-4 annotations to our editions of the text, using [hypothes.is](http://hypothes.is). Links are provided below. You'll have to sign up for a hypothes.is account first. As above, please use your real name as your username, so I know who you are. You may write about anything you want, but it will help your final project to think about ways in which computational analysis might help you to better understand what you observe in the text. Good annotations are:

  - Concise (think: a long tweet)
  - Well-written (although not too formal)
  - Observant (rather than evaluative)

You may respond to another student's annotation for one or two of your annotations, if you want. Just make your responses equally as thoughtful.

### Homework

Four short homework assignments, of 3-15 questions each, will be assigned weekly, and are due on Monday the following week, before our discussion starts (19:00 UTC). Jupyter notebook templates for each will be provided. Since we'll review the homework answers at the beginning of each week, late work cannot be accepted. There will be no homework due on the Monday of the last week, to give you more time to work on your final projects.

Submit homework to me at my email address, jonathan.reeve@columbia.edu.

### Final Project

The final project should be a literary argument, presented in the form of a short academic paper, created from the application of one or more of the text analysis techniques we have learned toward the analysis of a text or corpus of your choosing. Should you choose to work with a text or corpus other than the ones we've discussed in class, please clear it with me beforehand. Your paper should be a single Jupyter notebook, including prose in Markdown, code in Python, in-text citations, and a bibliography. A template will be provided. The length, not including the code, should be about 1500 words. You're allowed a maximum of three figures, so produce plots selectively. A word count function will be provided in the Jupyter notebook template.

During the final week of class, we'll have final project presentations. Your paper isn't required to be complete by then, but you'll be expected to speak about your project for about 5-7 minutes. Consider it a conference presentation.

Final papers will be evaluated according to the:

  - Quality of the literary critical argument presented
  - Quality of the close readings of the text or corpus
  - Quality of the Python text analysis
  - Literary interpretation of the results
  - Integration of the computational analysis with the literary argument

As with homework, please email me your final projects. You may optionally submit your final project to the course git repository, making it public, for a 5% bonus.

# Schedule

Note: this schedule is subject to some change, so please check the course website for the most up-to-date version.

## Week 1: Introduction to Python for Text Analysis

- Text: Wilkie Collins, *The Moonstone*
- Tools: Python (Anaconda)

### Unit 1.1 \<2020-07-06\>: Course intro.

  - [Lecture: introduction.](https://us.tv/videos/watch/242aa789-de52-4318-a0a6-6568f5623e52)
  - Motivation: what is possible with computational literary analysis?

### Unit 1.2 \<2020-07-07\>: Installing Python. Python 2 v. 3. Jupyter. Strings.

  - [Lecture 2: Getting Started](https://us.tv/videos/watch/ac6f30c0-1336-4ea0-8463-83f34855e58f)
  - Reading: [*The Moonstone*, First Period, Through Chapter VII](/texts/moonstone.html#prologue)

### Unit 1.3 \<2020-07-08\>: Working with strings, lists, and dictionaries.

  - Reading: [First Period, Through Chapter XI](/texts/moonstone.html#chapter-vii)

### Unit 1.4 \<2020-07-09\>: Python basics, continued. Homework 1 assigned.

  - Reading: [*The Moonstone*, Through Chapter XVII](/texts/moonstone.html#chapter-vii)

## Week 2: Basic Text Analysis

Text: *The Moonstone*, Continued
Tools: Natural Language ToolKit (NLTK)

### Unit 2.1 \<2020-07-13\>: Review of Week 1 and Homework 1.

  - Text: First Period, Complete.
  - **Homework 1 due**

### Unit 2.2 \<2020-07-14\>: Working with words. Tokenization techniques. Lemmatizers.

  - Text: Second Period, First Narrative

### Unit 2.3 \<2020-07-15\>: Basic text statistics with the NLTK. Type / token ratios. Loops, functions, and other control structures.

  - Text: Second Period, Second Narrative

### Unit 2.4: \<2020-07-16\>: More text statistics. Concordances, collocations, dispersion plots.

  - Text: Second Period, Third Narrative

## Week 3: Word Frequency Analyses

Text: *The Moonstone* and Katherine Mansfield, *The Garden Party and Other Stories*
Tools: Scikit-Learn, Pandas

### Unit 3.1 \<2020-07-20\>: Review of Week 2 and Homework 2. Numpy, Pandas, and narrative time.

  - **Homework 2 due**
  - Text: Second Period, Fourth and Fifth Narratives

### Unit 3.2 \<2020-07-21\>: N-grams and part-of-speech analyses.

  - Text: *The Moonstone*, Complete

### Unit 3.3 \<2020-07-22\>: WordNet and WordNet-based text analysis.

  - Texts: "The Garden Party"

### Unit 3.4 \<2020-07-23\>: Downloading, using, and iterating over corpora.

  - Texts: "The Daughters of the Late Colonel,"

## Week 4: Linguistic Techniques I

Text: Katherine Mansfield, *The Garden Party and Other Stories*
Tools: NLTK, SpaCy

### Unit 4.1 \<2020-07-27\>: Review of Week 3 and Homework 3. Corpus vectorization with Scikit-Learn. TF-IDF. Stylometry.

  - **Homework 3 due**
  - Texts: "The Young Girl"

### Unit 4.2 \<2020-07-28\>: Comparative stylometry. Corpus-DB.

  - Texts: "Marriage à la Mode"

### Unit 4.3 \<2020-07-29\>: Stylometry, continued.

  - Texts: "Her First Ball"

### Unit 4.4 \<2020-07-30\>: Topic modeling with LDA. Quote parsing.

  - Texts: "An Ideal Family,"

## Week 5: Linguistic Techniques II

Text: James Joyce, *Dubliners*
Tools: SpaCy

### Unit 5.1 \<2020-08-03\>: Review of Week 4 and Homework 4. Using SpaCy. Named entity recognition.

  - **Homework 4 due**
  - Texts: "The Sisters," "An Encounter"

### Unit 5.2 \<2020-08-04\>: Intro to final project. Sentiment analysis. Macro-etymological analysis.

  - Texts: "Araby", "Eveline"

### Unit 5.3 \<2020-08-05\>: Sentence structure analysis using SpaCy.

  - Texts: "The Boarding House,"

### Unit 5.4 \<2020-08-06\>: Extras: TEI XML, APIs

  - Texts: "Clay"

## Week 6: Advanced Topics

Text: James Joyce, *Dubliners*
Tools: Scikit-Learn, SpaCy

### Unit 6.1 \<2020-08-10\>: Review of Week 5. Writing tips.

### Unit 6.2 \<2020-08-11\>: Extras: Social Network Analysis Example

### Unit 6.3 \<2020-08-12\>: Final project presentations.

### Unit 6.4 \<2020-08-13\>: Final project presentations continued. Wrap-up.

### \<2020-08-14\>: Final open lab.

### \<2020-08-15\>: Final projects due.
