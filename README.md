# Common Lisp Interface to WordNet

## About WordNet

[Professor George Miller](http://www.cogsci.princeton.edu/~geo/) of the [Cognitive Science Laboratory](http://www.cogsci.princeton.edu/) of [Princeton University](http://www.princeton.edu/) directed the development a lexicographic database called [WordNet](http://clarity.princeton.edu:80/~wn/).

Princeton maintains a server by which the WordNet database can be [browsed](http://www.cogsci.princeton.edu/~wn/w3wn.html) via the World Wide Web.

The WordNet database is implemented as a set of [text files](data-file-format.text). [Mark Nahabedian](http://www.ai.mit.edu/people/naha/naha.html) (naha@mit.edu) has developed an interface to this database written in [Common Lisp](http://www.cs.cmu.edu:8001/Web/Groups/AI/html/cltl/cltl2.html). This software provides an interface by which Common Lisp programs can access lexicgraphic data from WordNet.

## Common Lisp Interface

The interface is written in several layers:

*   [a base layer](#the-base-layer)
*   [record extraction](#record-extraction)
*   [record parsing](#record-parsing)
*   [data representation](#data-representation)
*   [pointer reasoning](#pointer-reasoning)

There is also a simple [browser](#browser) implemented in [CLIM](ftp://ftp.digitool.com/pub/clim/papers/) for navigating the WordNet database.

This software represents parts of speech as lisp keyword symbols: `:noun`, `:verb`, `:adjective` and `:adverb`.

~~The current version of this software only knows how to find WordNet index and data files as they are named in the UNIX implementation of WordNet. Set the value of the parameter `wn::+wordnet-database-directory+` in the file `wordnet-database-files.lisp` to the pathname of the directory where these files can be found.~~ This software bundles WordNet database version 3.1.

~~The current version has only been tested with Symbolics Genera and [Macintosh Common Lisp](http://www.digitool.com/) (thanks to Andrew Blumberg, blumberg@ai.mit.edu). The software might require slight modification to run on other Lisp Implementations.~~ The current version has been modified and tested on SBCL x64 on Linux. It should work on other implementations without further modifications.

All the files from the original repository can be found at `ftp://ftp.ai.mit.edu/pub/users/naha/WordNet`. A single file in UNIX tar format is also available at `ftp://ftp.ai.mit.edu/pub/users/naha/WordNet/everything.tar`.`

### The Base Layer

The base layer defines the packages and export lists for this software. It is implemented by these files:

*   [packages.lisp](packages.lisp)
*   [parts-of-speech.lisp](parts-of-speech.lisp)

### Record Extraction

The record extraction layer is the bottom-most one. It implements functions which extract records from the database files as text strings.

(**index-entry-for-word** _file-description_ _word_)

Looks up _word_ in the specified index file and returns the string corresponding to that record of the index file. The _file-description_ argument can either be a part of speech keyword, a pathname naming an index file, or a stream which has been opened to that file.

(**read-data-file-entry** _file-description_ _offset_)

Reads a WordNet "symset" record from the specified _offset_ in the specified file. A string is returned. Offset was either read from an index record, or from a pointer description in another synset record. The _file-description_ argument should identify a WordNet data file. It should either be a part of speech keyword, a pathname, or a stream.

This layer is implemented by the file [wordnet-database-files.lisp](wordnet-database-files.lisp).

This layer depends on the files in the [base layer](#base-layer).

### Record Parsing

The functions in this layer take strings as returned by the functions of the [record extraction](#record-extraction) layer. They parse those strings into components, returning them as multiple values.

(**parse-index-file-entry** _entry_)

Parse the _entry_ as returned by **index-entry-for-word**. See the definition for a list of the values returned.

(**parse-data-file-entry** _entry_)

Parse the _entry_ as returned by **read-data-file-entry**. See the definition for a list of the values returned.

This layer is implemented by the file [parse-wordnet-data.lisp](parse-wordnet-data.lisp).

This layer depends on the files in the [base](#base-layer) layer.

### Data Representation

The data representation was chosen to parallel WordNet's own representation. It models index entries, synonym sets and pointers. Depending on ones application, there might well be more useful ways to represent the WordNet lexicon. Practice might lead us to modify this representation or develop a new one.

Class **wn:wordnet-index-entry**

Objects of this class are used to represent entries read from the index files. They are created and returned by the function **wn:cached-index-lookup**.

(**wn:cached-index-lookup** _word_ _part-of-speech_)

Looks up _word_ in the index file corresponding to _part-of-speech_ and returns an index entry object for it.

(**wn:index-entry-synsets** _index-entry_)

Returns a list of the synonym sets, as **wn:wordnet-synset-entry** objects, which _index-entry_ refers to.

Class **wn:wordnet-synset-entry**

Objects of this class represent synonym sets. There is a subclass for each part of speech:

*   **wn:wordnet-noun-entry**
*   **wn:wordnet-adjective-entry**
*   **wn:wordnet-adverb-entry**
*   **wn:wordnet-verb-entry**

(**wn:synset-words** _synset_)

Returns a list of "words" that are in the synonym set _synset_. Each word is represented by a list, the first element of which is the word as a string. The second element is the sense number assigned by the lexicographer.

(**wn:wordnet-pointers** _synset_)

Returns a list of the wordnet pointers from the specified _synset_.

Class **wn:wordnet-pointer**

These are how wordnet pointers are represented.

(**wn:wordnet-pointer-type** _pointer_)

Returns the wordnet pointer type for _pointer_, e.g. **:antonym**, **:hypernym**, **:entailment**, etc.

(**wordnet-pointer-from-synset** _pointer_)

Returns the synonym set which _pointer_ points from.

(**wordnet-pointer-to-synset** _pointer_)

Returns the synonym set which _pointer_ points to.

(**wordnet-pointer-from-word** _pointer_) (**wordnet-pointer-to-word** _pointer_)

If _pointer_ refers to a specific word in the synonym set, that word (as a list of string and sense number) are returned, otherwise the synonym set is returned.

This layer is implemented by the file [representation.lisp](representation.lisp).

This layer depends on the files in the [base](#base-layer) layer, the [record extraction](#record-extraction) layer and the [record parsing](#record-parsing) layer.

### Pointer Reasoning

This layer provides some functions for operating on the graph formed by WordNet synonym sets and the pointer relationships among them. Here follows a description of the operations currently provided. This set is expected to grow with time.

(**wn:relation-transitive-closure** _synset_ _relation-type_)

_relation-type_ must be a WordNet pointer type representing a transitive relation. This function returns a set which is the transitive closure of that relation starting with _synset_. The closure set is returned as a list. Each element of the list is a cons whose **car** is a synset object and whose **cdr** is an integer rpresenting the distance along the _relation-type_ between this synset and _synset_.

(**wn:commonality** _relation-type_ &rest _synsets_)

Finds the common "ancestors" of the synset objects in _synsets_ along the _relation-type_ graph. It returns a list, the first element of which is the closest common ancestor. The rest of the list has one element for each of _synsets_. Each element is a cons whose **car** is one of the _synsets_ and whose **cdr** is the distance from this synset to the common ancestor.

This Layer is implemented by the file [relationship-algorithms.lisp](relationship-algorithms.lisp).

This layer depends on the [data representation](#data-representation) layer.

### Browser

The browser provides a simple user interface for examining the wordnet database. It defines CLIM presentation types and commands for displaying the objects defined in the [data representation](#data-representation) layer.

It depends on the [data representation](#data-representation) layer, and on the layers on which that layer depends.

The browser also depends on a domonstration lisp interactor implemented in CLIM, which in the Symbolics Genera CLIM distribution can be found in the directory "sys>clim>rel-2>demo>listener.lisp".

The command **:Lookup** takes a string as argument. It looks up that string in the indices and prints out a list of index entries that were found.

You can click on one of these index entries to get a list of the synonym sets that it refers to.

Clicking on a synonym set will list the pointer references that it has to other synsets. The presentation of the pointer includes a presentation of the synset that it points to. You can click in it in turn to see its pointers.

### Examples

Some [examples](examples.lisp) have been written which illustrate the use of this software. Included are functions which list synonyms and antonyms for a specified word, and a function which lists the names and nicknames of the U.S. States. There is also a function which tries to identify the synset for a word having a sense most similar to a specified word by comparing distances along hypernym pointers among the synsets for the word being looked up and the sense indicating word.
