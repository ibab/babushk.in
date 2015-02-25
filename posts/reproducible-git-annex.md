---
title: Reproducible Data Science with git-annex
date: 2015-02-25
---

If you're interested in analysing data in a reproducible way, you're probably already using a version control system like git to manage your source code.
If not, you should!
A VCS like git allows you to track and document changes you make to your analysis, easily cooperate with others, roll back to any previous state of your project at any time, and do many other things that can be indispensible to data scientists.

Git is great for managing changes to plain text files (scripts, config files, makefiles, ...), but it's not designed to work with large binary files.
If there are large binary files checked into the repository, git slows down significantly, to the point of becoming unusable.

This means you can't check your 20GB dataset into your git repository.
For me, this usually means that while my analysis code is neatly managed by git, my datasets float around on several machines, with little to no organization.

This can pose lots of problems for reproducible science:
 
 - Which dataset did I use to create this figure 2 months ago?
 - Is the dataset I have really the same one that Bob used to derive those results?
 - How can I be sure that I haven't accidentally changed or replaced my dataset?

Wouldn't it be ideal if there was some way to tell git that an analysis depends on a certain version of a dataset, for examply by saving a cryptographic hash of the file?
Thankfully, there's an extension to git called *git-annex* that does exactly that.

[git-annex](http://git-annex.branchable.com/) is a tool written by Joey Hess that integrates with git to manage large binary files conveniently.
Setting it up is as simple as running `git annex init` in an existing git repository:
```
$ git init
Initialized empty Git repository in /home/igor/annex-test/.git/
$ git annex init "work"
init work ok
(recording state in git...)
```
Run `git annex add` on a dataset to add it to the annex datastore:
```
$ cp /data/from/somewhere/bigfile.h5 .
$ git annex add bigfile.h5
add bigfile.h5 ok
(recording state in git...)
$ ls
bigfile.h5@
```
`bigfile.h5` has now become a symlink pointing to a subdirectory of `.git/annex`:
```
$ ls -l
lrwxrwxrwx 1 igor users 184 Feb 25 11:17 bigfile.h5 -> .git/annex/
objects/Qw/Mg/SHA256E-s0--e3b0c44298fc1c149afbf4c8996fb92427ae41e4
649b934ca495991b7852b855.h5/SHA256E-s0--e3b0c44298fc1c149afbf4c899
6fb92427ae41e4649b934ca495991b7852b855.h5
```
The symlink has been automatically staged:
```
$ git status
On branch master

Initial commit

Changes to be committed:
	new file:   bigfile.h5
```
After the next `git commit`, the symlink (which contains a cryptographic hash) will become part of your git history.
But only the symlink will be saved, the actual file is managed by `git-annex`.
```
$ git commit -m "Add bigfile.h5 to annex"
```
One thing I particularly like about `git-annex` is that the repository stays a regular git repository, which means all your existing git tools and workflows can stay the same.
For example, you can enjoy the benefits of having your dataset managed by `git-annex`, while your collaborators use a regular version of the repository and manually work with the dataset.
(Even then, having the hash available can be useful)

If you want to take `git-annex` one step further, you can make use of its synchronization features to reliably and easily manage datasets between several machines.
You can either register another `git-annex` repository as a git remote, or you can make use of a variety of [remote backends](http://git-annex.branchable.com/special_remotes/) to store your data.
`git-annex` doesn't require you to store all your files in all your repositories, it's only important that your file is available *somewhere*.
It can then be automatically retrieved with `git annex get`.

In general, I've found `git-annex` to be very flexible.
Its paradigm of "save symlink in git, manage file separately" is easy to understand and leaves enough room to come up with a convenient workflow.

