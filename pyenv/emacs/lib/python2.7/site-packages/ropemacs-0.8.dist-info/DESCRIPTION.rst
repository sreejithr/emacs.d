
**Note:** *Please note that this project has been moved to* `GitHub python-rope / ropemacs`_

.. _GitHub python-rope / ropemacs: https://github.com/python-rope/ropemacs


=========================
 ropemacs, rope in emacs
=========================

Ropemacs is an emacs mode that uses rope_ library to provide features
like python refactorings and code-assists.  You should install rope_
library and pymacs_ before using ropemacs.

.. _rope: http://rope.sf.net/
.. _pymacs: http://pymacs.progiciels-bpi.ca/pymacs.html


New Features
============

``rope-find-occurrences`` sets ``next-error-function``.  That means
compilation mode keys like ``C-x \``` work for occurrences buffer,
too.

Also there is a bug in pymacs 23 and 24-beta1 that makes python reach
maximum recursion after interrupting a pymacs command; a patch is
included in the docs folder.




