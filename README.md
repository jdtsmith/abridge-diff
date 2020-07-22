# abridge-diff
A simple Emacs package for _abridging_ refined diff hunks (for example in [magit](https://github.com/magit/magit)) .  Adds post-processing to `diff-refine-hunk` (which itself uses `smerge-refine-regions`). Useful for line-based diffs of files with very long lines, as in LaTeX files or text files with full paragraphs per line (often using `visual-line-mode`).  

Simplest to illustrate with a pair of pictures:
### Before:

### After:


## Usage:

Once installed, it will start abridging all refined diffs. You can enable and disable showing the abridged version using `abridge-diff-toggle-hiding`.  Automatically configures itself to work with [magit](https://github.com/magit/magit) to add a new `D a` diff command.  
