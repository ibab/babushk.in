---
title: Organize your PhD with git and Github
date: 2015-10-25
---

Git is a version control system that keeps track of changes to a set of text files for you.
[Github](https://github.com) is a web service that allows you to back up your git repository and collaborate with others.

After starting my PhD recently, I realized that Github works incredibly well as a git-backed PhD notebook.
The idea is to create a new Github repository with an appropriate name, like `phd`.
This can be a public or private repo (you can request the [Student developer pack](https://education.github.com/pack) to get these for free).

The repo can then be used to take notes in Markdown.
This is a simple markup language that allows you to define headings, emphasize text, create links and todo lists, and much more. 
Whenever you place a Markdown file (with the ending `.md`) in your repository and browse to it, Github will show a rendered HTML version.
If you have a `README.md` file in a directory, the HTML page will show up when browsing to that directory.

The repository can also contain all the source code, papers, data files, LaTeX code or anything else you need to keep track of your research!
This allows you to keep all your files neatly accessible and documented, while you can set up your results to be reproducible.
Github also makes it easy to give your supervisor or other collaborators access.

It makes sense to come up with an organizational structure inside the repo that suits your style of working.
For example, I like to create a new numbered directory for every task or small project I'm working on, like `01-first-steps`.
Inside that directory, I place a `README.md` that reminds me of what the goal of the task is and what has already been achieved (e.g. using a todo list).
I also place a `Makefile` in that directory that builds and runs my code, which creates plots and numbers, which are in turn included into paper or a presentation.

Whenever I come across a piece of knowledge that I will have to remember later, I place it into the `README.md` of the corresponding project.
What has also worked very well is to have a top-level `TODO.md` that collects my current tasks and links to different places in the repository with further tasks and explanations. 
It is convenient to edit Markdown files directly from Gitub: Click on the `.md` file in the file browser and choose the *Edit this file* button in the upper right corner.
You can then edit the file online and save it by clicking the *Commit changes* button.

When it's time to write up your thesis, you can create a new directory called `thesis`.
Having all your work inside the repo, fully documented and reproducible, should make it easier to write everything up.
Plots and numbers can either be copied into the `thesis` directory (if you want to freeze them permanently) or you can include them directly from the location where they are produced.

See the repository at [https://github.com/ibab/phd-example](https://github.com/ibab/phd-example) for an example of how such a PhD notebook looks in action.

