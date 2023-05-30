## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release, resubmitted to CRAN to address comments.

> If there are references describing the methods in your package, please
> add these in the description field of your DESCRIPTION file

This package doesn't implement methods from a paper or other reference.

> Missing Rd-tags:
>   epoxy_style.Rd: \value

I have added the missing `\value` tag for `epoxy_style()`.

> Please ensure that your functions do not write by default or in your
> examples/vignettes/tests in the user's home filespace.

This package doesn't include any functions that write output.
I verified that tests write to the temporary directory.
There is an internal function that is used to build the
package documentation during development; it's possible that
this was the source of the note.
