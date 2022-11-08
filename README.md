# Overview

This package is a language server protocol implementation helping coding. It provides completion and will provide many other functionalities. These functionalities are established on the basis of static code analysis and distinguished from Geiser and many other coding plugins. For example, Geiser can only complete top-level binding identifiers and these do not include local bindings.

This package is published with Akku(https://akkuscm.org/), which is a language package manager for Scheme. 

This package has been tested with Chez Scheme versions 9.4 and 9.5.

Your donation will make this world better. And you can also raise your advice and I would implement is if it's available.
[![paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/paypalme/ufo5260987423/10)

# Attention
Many lisp macro using 

# Feature

1. Top-level identifiers binding completion.
2. Local identifiers binding completion.
3. Compatible with package manager: Akku.

## TODO: 

4. Fully compatible with r6rs standard.
5. File modification notification and corresponding index changing.
6. Cross-platform Parallel indexing.
7. Macro expanding.
8. Code eval.
9. Code diagnostic.
10. Fully r6rs compability.
11. Add cross-language semantic supporting.
12. File-change notification to improve workspace refreshing.

# Test
> bash test.sh
