-----
title: Chrome for Android webfont bug workaround
date: 2012-10-22
-----

While testing this site on an Android tablet (Nexus 7), I came across
a bug in Chrome for Android. The browser has a “text enlarger” that is
supposed to detect when a line of text is too long, and then raise the
font size to increase legibility on a mobile device.

However, when it does this, it changes the text to the default Android
font, instead of a web-font specified in the CSS. [This bug report][1]
details the problem; the response is “that'll hopefully resolve itself
at some point in the future”. In the meantime, it seems that the
options are:

1.  Give up on specifying custom fonts;
2.  Use a workaround to disable the auto-sizing.

If you pick option 2, one method is described [here][2]: set
`max-height: 1000000px;` in the CSS for the elements for which you
want to disable the auto-sizing. This seems like a brittle hack, but
it works, I guess. Perhaps in the future can use
`text-size-adjust` (see [here][3]).

[1]: http://code.google.com/p/chromium/issues/detail?id=138257
[2]: https://bugs.webkit.org/show_bug.cgi?id=84186#c17
[3]: http://dev.w3.org/csswg/css-size-adjust/#adjustment-control

