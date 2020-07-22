# abridge-diff
A simple Emacs package for _abridging_ refined diff hunks (for example in [magit](https://github.com/magit/magit)) .  Adds post-processing to `diff-refine-hunk` (which itself uses `smerge-refine-regions`). Useful for line-based diffs of files with very long lines, as in LaTeX files or text files with full paragraphs per line (often using `visual-line-mode`).  

Simplest to illustrate with a pair of pictures:
### Before
Long "sentences" with few changes are full of useless context:
![](examples/before.png)

### After
Only changes and a bit of surrounding context are shown.  Much cleaner:
![](examples/after.png)

## Usage:

Once installed, it will start abridging all refined diffs. You can enable and disable showing the abridged version using `abridge-diff-toggle-hiding`.  Automatically configures itself to work with [magit](https://github.com/magit/magit) to add a new `D a` diff command.  
