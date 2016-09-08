This is a downloader for the videos of the [Oregon Programming Languages Summer School 2012](https://www.cs.uoregon.edu/research/summerschool/summer12/curriculum.html).

The videos are quite big and you could [watch them on youtube instead](https://gist.github.com/danidiaz/36f5647c0968361eedd677ad3870715f#file-programming-language-theory-reading-list-md). 

The downloader works in two stages, to allow you to select an specific series of letures.

First, invoke it like

    oplss-downloader prepare D:\oplss2012\

where the second argument is an empty folder which must exist previously. 

A series of sub-folders will be created but no video will be downloaded. Delete the sub-folders of those lectures in which you are not interested.

Then, invoke the command again like

    oplss-downloader download D:\oplss2012\

To actually download the files corresponding to the remaining lectures.

Concurrent downloads are hardcoded to a maximum of 2.
