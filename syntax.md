Syntax definition of this language
====================

Notational Convention Used
--------------------

In this document, I use extended version of BNF
which is based on Augmented BNF, found in
[RFC2616 (HTTP1.1)](http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2.1).

First, I use `[...]` as character class. Inverted character class is
denoted as `[^...]` as usual.

Second, I introduce new notion of separated list. In the ABNF above,
_#rule_ is defined as follows:


    #rule
       A construct "#" is defined, similar to "*", for defining lists of
       elements. The full form is "<n>#<m>element" indicating at least
       <n> and at most <m> elements, each separated by one or more commas
       (",") and OPTIONAL linear white space (LWS). This makes the usual
       form of lists very easy; a rule such as
          ( *LWS element *( *LWS "," *LWS element ))
       can be shown as
          1#element
       Wherever this construct is used, null elements are allowed, but do
       not contribute to the count of elements present. That is,
       "(element), , (element) " is permitted, but counts as only two
       elements. Therefore, where at least one element is required, at
       least one non-null element MUST be present. Default values are 0
       and infinity so that "#element" allows any number, including zero;
       "1#element" requires at least one; and "1#2element" allows one or
       two.


Instead of using `#<m>` slot as upper limit, I use it as separator rule slot.


    #<SEP>elem
       This form defines separated list. Separator (SEP) must appear
       between each element. Repeated (SEP) can appear list head and/or tail too.
       So, following is all valid in this form:

       elem SEP elem
       elem SEP SEP elem
       SEP elem SEP elem SEP
       SEP SEP elem SEP SEP elem



With this, the above example is rewritten as follows:

      1#<",">element

or simply

      #<",">element


The sytax
--------------------

     script       = #<NLSEP>(statement | directive)

     directive    = "#" *[^\n] "\n"

     statement    = #<HWS>(labeled_term)

     labeled_term = #<label>(term)

     label        = "[" #<WS>(term_or_label) "]"

     term_or_label= (term | label)

     term         = (atom | group | formula
                   | quoted_block | quoted_string)

     group        = "(" #<WS>(term_or_label) ")"

     formula      = "$" #<WS>(term_or_label) ";"

     quoted_block = "{"  *([^{}"\\]  | "\\" ANY | quoted_block) "}"

     quoted_strng = "\"" *([^{}"\\]  | "\\" ANY | quoted_block) "\""

     atom         = 1*[_ A-Z a-z 0-9 = ! @ % & < > ? + - * / : . ~ , | ^]

     NLSEP        = ("\n"| ";")

     HWS          = (" " | "\t")

     WS           = (" " | "\t" | "\n")

     ANY          = [\x00-\xff]

