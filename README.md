# trunk-io-tree

A treemacs interface for trunk.io's trunk check

## Installation

This isn't in melpa yet. I install it using `use-package` and `straight` with:
```
(use-package trunk-io-tree
    :straight (trunk-io-tree :type git :host github :repo "jsadusk/trunk-io-tree")
    )
```

You can also clone https://github.com/jsadusk/trunk-io-tree into your `load-path` with:
```
git clone https://github.com/jsadusk/trunk-io-tree
```
and in your `init.el`
```
(add-to-list 'load-path /where/you/cloned/trunk-io-tree)
(require 'trunk-io-tree)
```

## Usage

From a `project.el` managed buffer, run `M-x trunk-check`.

When trunk has completed, a window will pop up with the issue breakdown for your project. Hitting `RET` on any error will jump to the location of the error.
