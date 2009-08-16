@c @documentencoding utf-8

@macro Function {args}
@defun \args\
@end defun
@end macro

@macro Macro {args}
@defmac \args\
@end defmac
@end macro

@macro Accessor {args}
@deffn {Accessor} \args\
@end deffn
@end macro

@macro GenericFunction {args}
@deffn {Generic Function} \args\
@end deffn
@end macro

@macro ForeignType {args}
@deftp {Foreign Type} \args\
@end deftp
@end macro

@macro Variable {args}
@defvr {Special Variable} \args\
@end defvr
@end macro

@macro Condition {args}
@deftp {Condition Type} \args\
@end deftp
@end macro

@macro cffi
@acronym{CFFI}
@end macro

@macro impnote {text}
@quotation
@strong{Implementor's note:} @emph{\text\}
@end quotation
@end macro

@c Info "requires" that x-refs end in a period or comma, or ) in the
@c case of @pxref.  So the following implements that requirement for
@c the "See also" subheadings that permeate this manual, but only in
@c Info mode.
@ifinfo
@macro seealso {name}
@ref{\name\}.
@end macro
@end ifinfo

@ifnotinfo
@alias seealso = ref
@end ifnotinfo

@c Typeset comments in roman font for the TeX output.
@iftex
@alias lispcmt = r
@end iftex
@ifnottex
@alias lispcmt = asis
@end ifnottex

@c My copy of makeinfo is not generating any HTML for @result{} for
@c some odd reason. (It certainly used to...)
@ifhtml
@macro result
=>
@end macro
@end ifhtml

@c Similar macro to @result. Its purpose is to work around the fact
@c that &rArr; does not work properly inside @lisp.
@ifhtml
@macro res
@html
&rArr;
@end html
@end macro
@end ifhtml

@ifnothtml
@alias res = result
@end ifnothtml

@c ============================= Macros =============================


@c Show types, functions, and concepts in the same index.
@syncodeindex tp cp
@syncodeindex fn cp
