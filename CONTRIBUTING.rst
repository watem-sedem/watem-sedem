.. highlight:: shell

============
Contributing
============

Contributions are welcome, and they are greatly appreciated! Every
little bit helps, and credit will always be given.

You can contribute in many ways:

Types of Contributions
----------------------

Report Bugs
~~~~~~~~~~~

Report bugs at https://github.com/cn-ws/cn-ws/issues

If you are reporting a bug, please include:

* Your operating system name and version.
* Any details about your local setup that might be helpful in troubleshooting.
* Detailed steps to reproduce the bug.

Fix Bugs
~~~~~~~~

Look through the GitHub issues for bugs. Anything tagged with "bug"
and "help wanted" is open to whoever wants to implement it.

Implement Features
~~~~~~~~~~~~~~~~~~

Look through the GitHub issues for features. Anything tagged with "enhancement"
and "help wanted" is open to whoever wants to implement it.

Write Documentation
~~~~~~~~~~~~~~~~~~~

CN-WS could always use more documentation, whether as part of the
official CN-WS docs, in docstrings, or even on the web in blog posts,
articles, and such. Note that on every documentation page there is a small
"edit on GitHub" link in the top right - if you catch small errors, please
suggest improvements.

Please note that all formulas and mathematical expressions must be written
in the correct LaTeX-syntax in order to render correctly.

Submit Feedback
~~~~~~~~~~~~~~~

The best way to send feedback is to file an issue at
https://github.com/cn-ws/cn-ws/issues .

If you are proposing a feature:

* Explain in detail how it would work.
* Keep the scope as narrow as possible, to make it easier to implement.

Get Started!
------------

Ready to contribute? Here's how to set up `CN-WS` for local development.

1. Fork the `CN-WS` repo on GitHub.
2. Clone your fork locally::

    $ git clone git@github.com:your_name_here/cn_ws.git

3. We use git LFS for our testfiles. If it is not installed, install
`git-lfs <https://git-lfs.github.com/>`_ and run::

   $ git-lfs pull

4. Create a branch for local development::

    $ git checkout -b name-of-your-bugfix-or-feature

   Now you can make your changes locally.

5. Commit your changes and push your branch to GitHub::

    $ git add .
    $ git commit -m "Your detailed description of your changes."
    $ git push origin name-of-your-bugfix-or-feature

6. Submit a pull request through the GitHub website.

Pull Request Guidelines
-----------------------

Before you submit a pull request, check that it meets these guidelines:

1. If the pull request adds functionality, the docs should be updated. Put
   your new functionality into a function with a docstring, and add the
   feature to the list in README.rst.

.. note::
    Note that this are guidelines. If you are stuck while adding functionality
    - consider doing a pull request anyway, others may be able to help.

Building the documentation
--------------------------
The documentation for the project can be found under the ``docs/`` folder, and
is written using `reStructuredText`_.

To build the documentation locally, you need to install the doc requirements,
which are based on sphinx_ (preferably in a new conda environment).

.. code-block:: bash

  $ pip install -r docs/requirements.txt

After which you should be able to generate HTML output by typing
``make html`` from the `docs` directory.

Publishing on the documentation website (https://docs.fluves.net/cnws-pascal)
will happen when changes to master build correctly. Note that this may mean that
the documentation is actually a bit more recent than the last released version.

.. _reStructuredText: http://docutils.sourceforge.net/rst.html
.. _sphinx: http://www.sphinx-doc.org/en/master/
.. _semver: https://semver.org/

Code structure
--------------

CN-WS was originally written to work in three different modi operandi:

1. as a command line tool (without GUI)
2. via a GUI
3. a long-term version with a GUI

The long-term version was made to calculate multiple rain events and runs the
command line version several times behind each other. Both GUI versions of CN-WS
are not maintained since 2016 and are not available as a download.

CN-WS is written in Free Pascal. In the main directory of the repository four
folders with pascal code can be found:

- cn_ws: all code that is unique for the command line tool without GUI
- cn_ws_gui: all code that is unique for GUI model (not maintained)
- cn_ws_gui_LT: all code that is unique for Long term GUI model (not maintained)
- common: the code base that is used by all three models.

We refer to the documentation of `Pascal <https://www.freepascal.org/docs.html>`_
and the `Lazarus IDE <https://www.lazarus-ide.org/>`_ to get started with
developping.
