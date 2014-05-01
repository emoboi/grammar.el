Ginger grammar checker intafce with some utils
=================
Ginger grammar checker intafce

https://gist.github.com/syohex/5457732

custmized for Latex 

and add intafce ( ido-completing-read) 

some codes in 
     http://code.google.com/p/bcui-emacs/source/browse/#svn/trunk/grammar
used 

# Install

+ install ht.el https://github.com/niitsuma/ht.el
(dont use package.el for install this package)

+ install https://github.com/tkf/emacs-request
(dont use package.el for install this package)

+ install ispell.el json.el and s.el 
 (can use package.el)

+ cp grammar.el [your elisp path]

# Usage

(require 'grammar)

M-x grammar-buffer

# Ref

- ido-completing-read minimun example http://d.hatena.ne.jp/niitsuma/20080123/1398913970 
