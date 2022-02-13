
## Background and Introduction

[Professor George Miller](http://www.cogsci.princeton.edu/~geo/) of the [Cognitive Science Laboratory](http://www.cogsci.princeton.edu/) of [Princeton University](http://www.princeton.edu/) directed the development a lexicographic database called [WordNet](http://clarity.princeton.edu:80/~wn/).

Princeton maintains a server by which the WordNet database can be [browsed](http://www.cogsci.princeton.edu/~wn/w3wn.html) via the World Wide Web.

The WordNet database is implemented as a set of [text files](data-file-format.text). [Mark Nahabedian](http://www.ai.mit.edu/people/naha/naha.html) (naha@mit.edu) has developed an interface to this database written in [Common Lisp](http://www.cs.cmu.edu:8001/Web/Groups/AI/html/cltl/cltl2.html). This software provides an interface by which Common Lisp programs can access lexicographic data from WordNet.

## 2 Min Overview

#### Some Exported Functions

A complete list can be found in [packages.lisp](packages.lisp) with more documentation [here](https://github.com/phoe/wordnet).

_wordnet_

- synsets-containing-word/s
- get-synonyms
- get-antonyms
- synset-words
- part-of-speech
- cached-index-lookup
- index-entry-synsets

NOTE 1: A point of note is the `part-of-speech`. Arguments with this name can take the value `:noun`, `:adjective`, `:verb` or `:adverb`.

NOTE 2: I am not the author - these examples and explanations are all that I understand. There is more documentation available at the [original repo](https://github.com/phoe/wordnet#data-representation) - this is a forked one. Further, I have made part-of-speech as an optional argument in `synsets-containing-word/s`, and `get-synonyms`; the former function is named `synsets-containing-words` in the original repo.

#### Class Hierarchy

```lisp
(wordnet-object ;; private
  (wordnet-synset-entry
    (wordnet-noun-entry
     wordnet-verb-entry
     wordnet-adjective-entry
     wordnet-adverb-entry)
  (wordnet-index-entry)
  (wordnet-pointer))
```


#### Examples


```lisp
    CL-USER> (ql:quickload 'wordnet)
    (WORDNET)

    CL-USER> (use-package :wordnet)
    T

    CL-USER> (setq dog-synsets (synsets-containing-word/s "dog" :noun)
    ;; The first argument could be :dog or "dog" as well
    ;; The second argument is optional, if given, it could be :verb, :adverb or :adjective
    (#<WORDNET-NOUN-ENTRY dog domestic_dog Canis_familiaris  {1001CB36A3}>
     #<WORDNET-NOUN-ENTRY frump dog  {1001CB7FD3}>
     #<WORDNET-NOUN-ENTRY dog  {1001CBAAF3}>
     #<WORDNET-NOUN-ENTRY cad bounder blackguard dog hound heel  {1001CBF193}>
     #<WORDNET-NOUN-ENTRY frank frankfurter hotdog hot_dog dog wiener wienerwurst weenie    {1001CC4373}>
     #<WORDNET-NOUN-ENTRY pawl detent click dog  {1001CC8053}>
     #<WORDNET-NOUN-ENTRY andiron firedog dog dog-iron  {1001CCB733}>)

    CL-USER> (get-synonyms :joy) 
    ;; an optional argument specifying part-of-speech can also be supplied
    (("joy" 0) ("joyousness" 0) ("joyfulness" 0) ("joy" 0) ("delight" 0) 
     ("pleasure" 0))

    CL-USER> (get-antonyms 'joy :noun)
    ;; part-of-speech is not optional here 
    ("sorrow")

    ;;; ======== The above functions are implemented in examples.lisp ==========
    ;; The list of the exported functions can be found in packages.lisp.
    

    CL-USER> (synset-words (first dog-synsets))
    (("dog" 0) ("domestic_dog" 0) ("Canis_familiaris" 0))

    CL-USER> (part-of-speech (first dog-synsets))
    :NOUN

    CL-USER> (setq dog (cached-index-lookup 'dog :noun))
    #<WORDNET-INDEX-ENTRY NOUN dog {100298AD33}>    
    
    CL-USER> (setq dog-synsets (index-entry-synsets dog))
    (#<WORDNET-NOUN-ENTRY dog domestic_dog Canis_familiaris  {100291AAB3}>
     #<WORDNET-NOUN-ENTRY frump dog  {100291F3E3}>
     #<WORDNET-NOUN-ENTRY dog  {1002921F33}>
     #<WORDNET-NOUN-ENTRY cad bounder blackguard dog hound heel  {10029265D3}>
     #<WORDNET-NOUN-ENTRY frank frankfurter hotdog hot_dog dog wiener wienerwurst weenie        {100292B9F3}>
     #<WORDNET-NOUN-ENTRY pawl detent click dog  {100292F6D3}>
     #<WORDNET-NOUN-ENTRY andiron firedog dog dog-iron  {1002932DC3}>)

``` 
