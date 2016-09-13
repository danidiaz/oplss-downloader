This is a downloader for the videos of the [2012 Oregon Programming Languages Summer School](https://www.cs.uoregon.edu/research/summerschool/summer12/curriculum.html).

The videos are quite big and you could [watch them on youtube instead](https://gist.github.com/danidiaz/36f5647c0968361eedd677ad3870715f#file-programming-language-theory-reading-list-md). 

The downloader works in two stages, to allow you to select a specific series of lectures.

First, invoke it like

    oplss-downloader prepare D:\oplss2012\

where the second argument is the target folder. 

A series of sub-folders will be created but no video will be downloaded. Delete
the sub-folders of those lectures in which you are not interested.

Then, invoke the command again like

    oplss-downloader download D:\oplss2012\

to actually download the files corresponding to the remaining lectures.

Concurrent downloads are hardcoded to a maximum of 2.
