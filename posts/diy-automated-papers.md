---
title: DIY Automated Papers
date: 2016-05-20
math: true
---

If you've ever written a scientific paper, a report or a thesis, the following
might evoke painful memories: Imagine you have some code (for example, a few
Python scripts) that produce the outputs that you want to write about.  Let's
refer to these scripts as your *analysis*. When you run your analysis, it might
print its outputs directly to the terminal, or store them in a text file
somewhere. Then, when writing your paper, you painstakingly insert every single
number, table and figure into your document.  After making a lot of progress,
you notice a bug in the analysis. Bugs happen all the time when writing
software, and unfortunately your scripts are no exception.  In this case, the
bug affects various outputs of the analysis, which will inevitably change once
you fix the mistake. Now you have no choice but to go through your outputs and
carefully update the ones that changed. Afterwards, you are left wondering
whether you missed one of the changed values, which is pretty distressing.
Needless to say that tight deadlines make this much worse, as you're more
likely to make mistakes, and at the same time you can't afford to spend a lot
of time updating numbers in your paper.

Another problem that can occur when entering many figures by hand is the fact
that you will inevitably make mistakes. A single typo in a large table probably
won't get noticed. If you've ever managed to discovered one of these, you'll
agree that they are irritating: Even if a single wrong value might not be the
end of the world, you've worked hard to produce your results and the fact that
a single typo can render them incorrect is uncomfortable.

Clearly, it would be nice if there was a system that could take all the outputs
of your analysis and insert them into the document in some automated way, "as
if by magic". There are some features such a system should have that I'd say
are pretty indispensible:

 - It should work with your existing typesetting application. For most
   scientists, this means that it should support LaTeX.
 - It should work with the programming language and tools that you are already
   using. I usually use Python for data analysis, so for me it's important
   that it integrates well with Python.
 - It should be easy to use and simple to debug, as otherwise you will
   eventually lose motivation to use it.
 - It should provide a lot of flexibility when inserting values into the
   document, so that you don't have to constantly resort to hacks. For example,
   it's important that you are able to insert the same value in different ways,
   like rounding the same floating point number to different significant
   figures.

In this post, I will show you how you can build your own templating tool that
ticks all of these boxes. This might sound very difficult at first, but it's
not that bad if we make use of the following key insights:

 - We should use a plaintext markup language for typesetting, as this greatly
   simplifies handling text. This means that LaTeX and Markdown are in, but
   Word is unfortunately out.
 - The problem we want to solve is actually not too different to
   programmatically inserting text into an HTML web page, which is a pretty
   common task in web programming. Therefore, various templating frameworks
   exist for all popular programming languages.
   We just have to use one that's flexible enough for our use case.

The [jinja2](http://jinja.pocoo.org/docs/dev/) templating framework is the most
popular templating system for Python. It's well-designed, thoroughly tested,
and flexible enough for what we want to do.
You might have used jinja2 before for generating HTML documents, but in case
you've never used it before, here is a quick explanation of the templating
syntax:

By default, you can insert a value anywhere in your text by enclosing
it in double curly braces.
For example, if my text contains
```
The value is {{ myvariable }}.
```
And the current value of `myvariable` is the integer `42`, then the output
becomes
```
The value is 42.
```
We have access to Python functionality inside the template as well:
```
The value is {{ myvariable**2 }}.
```
becomes
```
The value is 1764.
```

You can also insert sequences of text into your document by using `for` loops:
```
{% for x in numbers %}
The value is {{ x }}.
{% endfor %}
```
will become
```
The value is 1.
The value is 2.
The value is 3.
```
in the final text, if `numbers` is the list `[1, 2, 3]`.
It will turn out that this feature will be extremely useful for automatically
creating tables.

Let's try this out with a real LaTeX document!
There's just one problem: The above template syntax clashes with the use of `{`
and `}` in LaTeX.
Luckily, `jinja2` allows us to change the default template syntax.
I prefer to use `<<` and `>>` when inserting values and `#<` and `>#` for `for`
loops and other blocks.

Let's start by writing a minimal LaTeX document that we want to insert
something into.
Create the following file and name it `main.tex`:
```
\documentclass{article}
\begin{document}
Given $x = << value >>$, we know that $x^2 = << value**2 >>$.
\end{document}
```

Now, we should make sure that the `jinja2` package is installed.
Run
```
pip install --user jinja2
```
to install it into your home directory. Then, open up a script called
`process.py` and add the following code:
```python
import sys
from jinja2 import Environment, FileSystemLoader, StrictUndefined
env = Environment(
    loader=FileSystemLoader('.'),
    undefined=StrictUndefined,
)
```
This sets `jinja2` up to look for files in the current directory and to raise
an error if we use an undefined value in our template. We can then configure
our custom template syntax:
```python
env.block_start_string = '#<'
env.block_end_string = '>#'
env.variable_start_string = '<<'
env.variable_end_string = '>>'
env.comment_start_string = '#='
env.comment_end_string = '=#'
```
Here, I've also defined `#=` and `=#` in case you want to use `jinja2`'s
comment functionality.

Now, let's define the values that we want to be available inside the template:
```python
inserts = {
    'value': 5,
}
```
Load the paper as a template into `jinja2`:
```
template = env.get_template('main.tex')
```
We are now ready to insert the values and save the resulting text:
```python
text = template.render(**inserts)
with open('main.processed.tex', 'w') as f:
    f.write(text.encode('utf-8'))
```
You can run the script by executing
```
python process.py
```
If you open up `main.processed.tex`, it should contain
```
\documentclass{article}
\begin{document}
Given $x = 5$, we know that $x^2 = 25$.
\end{document}
```
If you compile it, the document should contain the text

Given $x = 5$, we know that $x^2 = 25$.

The problem with the above setup is that we have passed in the inserted values
manually. Ideally, we should be able to define the values that are available in
the template conveniently from inside our analysis scripts.

Let's create such a script. Open up the file `run.py` and enter the following
helper class:
```python
import os
import sys
import shelve
from contextlib import closing

class store:
    def __init__(self, build='.'):
        name = os.path.basename(sys.argv[0])
        name += '.shelve'
        self.name = os.path.join(build, name)

    def __getitem__(self, key):
        with closing(shelve.open(self.name)) as db:
            value = db[key]
        return value

    def __setitem__(self, key, value):
        with closing(shelve.open(self.name)) as db:
            db[key] = value

```
The `store` class will allow us to easily store values from within our script.
First, we instantiate a `store` object:
```python
db = store()
```
Then we can simply assign a value to a key of the `store` whenever we want to
save something:
```python
db['value'] = 5
```
The `store` object will automatically create the file `run.shelve` that
contains all stored variables.
In the `process.py`, we should add
```python
all_db = dict()
for dbname in sys.argv[1:]:
    db = shelve.open(dbname)
    all_db.update(dict(**db))
    db.close()
```
to get the contents of all `.shelve` files that are passed on the command line
to the script.
The code at the end of the file can be modified to
```python
text = template.render(**all_db)
with open('build/main.tex', 'w') as f:
    f.write(text.encode('utf-8'))
```
We can now run
```
python process.py run.shelve main.tex
```
To insert the values stored from our `run.py` script into the `main.tex` file.
It might make sense to automate the process with a `Makefile` that contains
```make
all: main.processed.pdf
SHELVES=$(wildcard *.shelve)
FILTERED=$(filter-out process.py, $(SHELVES))

%.pdf: %.tex
    lualatex $^

%.processed.tex: $(FILTERED) %.tex
    python process.py $^

%.shelve: %.py
    python $^
```
You can then simply run
```
make
```
to generate the finished PDF.
The flexibility of `jinja2` comes in handy when we want to create tables from
values inside our `run.py` script.
Let's add the following code to `run.py`:
```python
data = [
    (1, 2.3),
    (2, 1.2),
    (3, 1.1),
    (4, 1.5),
]

db['data'] = data
```
Inside our `main.tex`, we can add a table by specifying
```
\begin{tabular}{r r}
  \hrule
    First item & Second item \\
  \hrule
    #< for line in data >#
      << 2 * line[0] >> & << "{.2f}".format(line[1]) >> \
    #< endfor >#
  \hrule
\end{tabular}
```
Note that we can heavily post-process the template variables to achieve the
exact output we need.
Also, we could easily be dealing with other kinds of data types like `numpy`
arrays or `pandas` DataFrames here instead of a simple list.

If you now run `python run.py` and then `make`, your document should be updated
and include the table.

In principle, this system is already powerful enough to write simple documents.
If you want to discover more ways of inserting values into your document and
simplify your template, it would make sense to read the [`jinja2`
documentation](http://jinja.pocoo.org/docs/dev/).

This basic system can be improved in many different ways:

 - Incorporate code written in other languages by either parsing the stdout of
   those programs from within Python, or by introducing a standard data format
   (e.g. JSON) for storing the outputs.
 - Make the storage of outputs more robust by using a different kind of
   database than `.shelve` files.
 - Write useful helper functions called
   [*filters*](http://jinja.pocoo.org/docs/dev/templates/#filters) that can be
   used inside the templates. For example, you could use these to round values
   or escape LaTeX-specific characters.
 - Also incorporate plots into the template workflow by storing `matplotlib`
   plots conveniently. For example, you could pickle them and then dynamically
   adjust the font and size of the figures when inserting them into the
   document.

Sometimes, you might want to collaborate with others who don't need to run your
template setup. In that case, you can just give them the processed file, which
should be mostly indistinguishable from a manually created one.

I find that using a templating system like this speeds up the creation of
papers, and comes in especially handy when changes need to be incorporated in a
short amount of time.
The idea for this kind of system is not my own, but taken from several places
on the internet, for example [this StackOverflow
question](http://stackoverflow.com/questions/14878729/insert-calcuated-results-into-latex-document).

If you want to check out a complete code example, you can have a look at [this
Github repository](https://github.com/ibab/fully-reproducible) that I've
prepared.
This repository also runs Travis on each commit and stores the resulting PDF on
the `gh-pages` branch, so that it's easy to look at the finished output that
each commit produces.

