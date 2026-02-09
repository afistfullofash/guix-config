(define-module (affa-guix-config home package-collections tree-sitter)
  #:use-module (gnu packages tree-sitter)
  
  #:export (tree-sitter-grammar-packages))

(define tree-sitter-grammar-packages
  (list tree-sitter-typescript
	tree-sitter-scheme
	tree-sitter-rust
	tree-sitter-lua
	tree-sitter-ruby
	tree-sitter-python
	tree-sitter-php
	tree-sitter-org
	tree-sitter-nix
	tree-sitter-meson
	tree-sitter-markdown
	tree-sitter-json
	tree-sitter-javascript
	tree-sitter-html
	tree-sitter-hcl
	tree-sitter-gomod
	tree-sitter-yaml
	tree-sitter-go
	tree-sitter-dockerfile
	tree-sitter-css
	tree-sitter-cmake
	tree-sitter-c-sharp
	tree-sitter-bash))
