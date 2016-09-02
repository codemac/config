A dictionary is used to store strings, along with arbitrary data associated
with each string. As well as basic data insertion, manipulation and
retrieval, a dictionary can perform prefix searches on those strings,
retrieving all strings with a given prefix in either alphabetical or any
other order (see the `dictree-complete' and `dictree-complete-ordered'
functions), and is able to cache results in order to speed up those
searches. The package also provides persistent storage of the data
structures to files.

You create a dictionary using `dictree-create', add entries to it using
`dictree-insert', lookup entries using `dictree-lookup', find completions
of sequences using `dictree-complete', find completions and sort them in
any order you speficy using `dictree-complete-ordered', map over it using
`dictree-map' and `dictree-mapcar', save it to a file using `dictree-save'
or `dictree-write', and load from file it using `dictree-load'. Various
other useful functions are also provided.

This package uses the trie package trie.el. the tagged NFA package tNFA.el,
and the heap package heap.el.
