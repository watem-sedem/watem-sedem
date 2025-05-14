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

Report bugs at https://github.com/watem-sedem/watem-sedem/issues

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

WaTEM/SEDEM could always use more documentation, whether as part of the
official WaTEM/SEDEM docs, in docstrings, or even on the web in blog posts,
articles, and such. Note that on every documentation page there is a small
"edit on GitHub" link in the top right - if you catch small errors, please
suggest improvements.

Please note that all formulas and mathematical expressions must be written
in the correct LaTeX-syntax in order to render correctly.

Submit Feedback
~~~~~~~~~~~~~~~

The best way to send feedback is to file an issue at
https://github.com/watem-sedem/watem-sedem/issues .

If you are proposing a feature:

* Explain in detail how it would work.
* Keep the scope as narrow as possible, to make it easier to implement.

Get Started!
------------

Ready to contribute? Here's how to set up `WaTEM/SEDEM` for local development.

1. Fork the `watem-sedem` repo on GitHub.
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

After which you should be able to generate HTML output by:

.. code-block:: bash

  $ sphinx-build ./docs ./docs/_build/


Publishing on the documentation website (https://watem-sedem.github.io/watem-sedem/)
will happen when changes to master build correctly. Note that this may mean that
the documentation is actually a bit more recent than the last released version.

.. _reStructuredText: http://docutils.sourceforge.net/rst.html
.. _sphinx: http://www.sphinx-doc.org/en/master/
.. _semver: https://semver.org/

Code structure
--------------

WaTEM/SEDEM was originally written to work in three different modi operandi:

1. as a command line tool (without GUI)
2. via a GUI
3. a long-term version with a GUI

The long-term version was made to calculate multiple rain events and runs the
command line version several times behind each other. Both GUI versions of WaTEM/SEDEM
are not maintained since 2016 and are not available as a download.

WaTEM/SEDEM is written in Free Pascal. In the main directory of the repository four
folders with pascal code can be found:

- cn_ws: all code that is unique for the command line tool without GUI
- cn_ws_gui: all code that is unique for GUI model (not maintained)
- cn_ws_gui_LT: all code that is unique for Long term GUI model (not maintained)
- common: the code base that is used by all three models.

We refer to the documentation of `Pascal <https://www.freepascal.org/docs.html>`_
and the `Lazarus IDE <https://www.lazarus-ide.org/>`_ to get started with
developping.

Debugging
---------

In case errors occur during execution, it is best to try to run the code directly in debug mode from
lazarus (F9). This allows will catch most exceptions and allows inspection of values at that point.
If this is not an option and unexpected errors occur, it is possible that the executable generates an
exception address:

   Invalid floating point operation
   Exception Address:   $00000000004AFA28
   
When used with the exact same binary, this exception can be decoded using addr2line:

   addr2line -e watem_sedem 00000000004AFA28
   watem-sedem/watem_sedem//../common/raster_calculations.pas:355


Making a new release
--------------------

When a new release will be published on GitHub make sure following steps are taken:

- Update the CHANGELOG.rst file with all changes compared to the previous version
- Make sure that the release number is updated in 'watem-sedem/watem-sedem/version.inc'
- Make sure that the release number and the abstract is updated in 'CITATION.CFF'

When the release is done check the following:

- Are the binaries added to the release page of your latest release on GitHub?
- Is the release visible on Zenodo? Check if all the metadata on Zenodo is correct, adjust them if necassary.

.. warning::
    When making a release for a test, make sure that the automatic syncing with zenodo is disabled,
    otherwise, your test will published on zenodo and will get an DOI.

Release number
~~~~~~~~~~~~~~

We use the Semantic Versioning Specification (SemVer)  
for the release numbers. For more information about this numbering we refer to
`the documentation page <https://semver.org/#semantic-versioning-200>`_ of SemVer.

It is important to note that this repository of WaTEM-SEDEM was build from
the original code of WaTEM-SEDEM of KULeuven. Before GitHub was used, 
WaTEM-SEDEM was versioned with the year of the build. E.g. the most recent
binary publicaly available before GitHub was used was WaTEM-SEDEM 2006. This
versioning sheme will not be used anymore. 

Zenodo
~~~~~~

`Zenodo <https://zenodo.org/>`_ is used to create a DOI and a citable reference
for the model code. For every version of the model a new publication on Zenodo is made based
on the CITATION.cff file in the root directory of the repository. You can find the publication of
watem-sedem on Zenodo `here <10.5281/zenodo.10997287>`_.

Every owner of the
`watem-sedem organisation on GitHub <https://github.com/watem-sedem>`_ can enable or
disable the automatic publication of a new version of the code on Zenodo. The automatic publication
is prefered, otherwise all metadata of CITATION.cff must be entered manually in Zenodo.

How to check if the automatic publication is enabled in Zenodo?

- Log in on Zenodo with your GitHub account
- Go to the menu 'GitHub repositories' under 'Settings' and check if watem-sedem/watem-sedem is listed
  under the 'enabled repositories'.
- If the repository is enabled, all is fine and after a new release on GitHub, the metadata will be published on
  Zenodo automatically
- If the repository is listed under repositories, but not enabled, the metadata will not be published automatically on a
  new release. Set the switch in the menu to 'on'.
- If the repository is not listed under repositories, make sure you have synced your GitHub repositories on Zenodo.
