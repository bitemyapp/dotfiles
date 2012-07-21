<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml" lang="en">
<head>
<title>Introducing json.el &#8212; Edward O&#8217;Connor</title>
<link rel="shortcut icon" href="/favicon.ico" />
<meta name="viewport" content="width=device-width; initial-scale=1.0; maximum-scale=1.0;" />
<link rel="alternate" type="application/atom+xml" title="Atom feed" href="/feed" />
<script type="text/javascript" src="http://ajax.googleapis.com/ajax/libs/jquery/1.3.2/jquery.min.js"></script>
<link rel="stylesheet" type="text/css" href="/css/layout-1c.css" />
<link rel="stylesheet" type="text/css" href="/css/typography.css" />
<link rel="stylesheet" type="text/css" media="screen" href="/css/color-dark-on-light.css" />
<link rel="pingback" href="http://edward.oconnor.cx/pingback" />
<script src="/js/showdown.js" type="text/javascript"></script>
<script src="/js/hober.js" type="text/javascript"></script>
<script type="text/javascript">
if (document.location.host != "localhost") {
var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
}
</script>
<script type="text/javascript">
if (document.location.host != "localhost") {
var pageTracker = _gat._getTracker("UA-648822-1");
pageTracker._initData();
pageTracker._trackPageview();
}
</script>
</head>
<body>
<div id="content">
<div id="primary">
<div class="hentry">
<div class="header">
<h1><a rel="bookmark" class="entry-title" href="/2006/03/json.el">Introducing json.el</a></h1>
<address class="author vcard">
  by <a href="http://edward.oconnor.cx/" class="fn url">Edward O’Connor</a>
on <abbr class="published updated" title="2006-03-26T15:24:24-05:00">26 March 2006</abbr>
</address>
</div> <!-- .header -->

<div class="entry-content">
<p>
        JSON is a lightweight data interchange format based on a subset
        of JavaScript. You can read all about JSON at <a href="http://json.org/">json.org</a>.

        <a href="/elisp/json.el">json.el</a> is a JSON parser and
        generator for Emacs Lisp, which can produce an Emacs Lisp data
        structure from a JSON object and vice-versa.

        It’s been Included with Emacs since February 2008.
      </p>

      <p>Using it is pretty straightforward; here are some examples.</p>

      <pre class="ielm"><span class="prompt">*</span> <code class="elisp"><kbd>(<span class="keyword">require</span> '<span class="constant">json</span>)</kbd>
<samp>json</samp></code></pre>

      <p>
        JSON's primitive values are strings, numbers, and the keywords
        <code class="json">true</code>, <code class="json">false</code>,
        and <code class="json">null</code>.
      </p>

      <pre class="ielm"><span class="prompt">*</span> <code class="elisp"><kbd>(json-read-from-string <span class="string">"true"</span>)</kbd>
<samp>t</samp></code>
<span class="prompt">*</span> <code class="elisp"><kbd>(json-encode t)</kbd>
<samp class="string">"true"</samp></code>
<span class="prompt">*</span> <code class="elisp"><kbd>(json-read-from-string <span class="string">"4.5"</span>)</kbd>
<samp>4.5</samp></code>
<span class="prompt">*</span> <code class="elisp"><kbd>(json-read-from-string <span class="string">"\\"foo\\""</span>)</kbd>
<samp class="string">"foo"</samp></code></pre>

      <p>JSON's compound values are arrays and dictionaries.</p>

      <pre class="ielm"><span class="prompt">*</span> <code class="elisp"><kbd>(json-read-from-string <span class="string">"[true, 4.5]"</span>)</kbd>
<samp>[t 4.5]</samp></code>
<span class="prompt">*</span> <code class="elisp"><kbd>(json-read-from-string <span class="string">"{\\"foo\\": true}"</span>)</kbd>
<samp>((foo . t))</samp></code></pre>

      <p>
        Notice that we read the JSON array as a lisp vector and the JSON
        dictionary as an alist. We could just have read the array as a
        list, and the dictionary as a plist or hashtable. json.el allows
        for all of these representations. (Also, note that the alist
        keys are symbols; we could read these as keywords or strings.)
      </p>

      <ul>
        <li>dictionary-as-plist:
        <pre class="ielm"><span class="prompt">*</span> <code class="elisp"><kbd>(let ((json-object-type 'plist))
    (json-read-from-string <span class="string">"{\\"foo\\": true}"</span>))</kbd>
<samp>(<span class="builtin">:foo</span> t)</samp></code></pre></li>
        <li>key-as-string:
        <pre class="ielm"><span class="prompt">*</span> <code class="elisp"><kbd>(let ((json-key-type 'string))
    (json-read-from-string <span class="string">"{\\"foo\\": true}"</span>))</kbd>
<samp>((<span class="string">"foo"</span> . t))</samp></code></pre></li>
        <li>dictionary-as-hashtable:
        <pre class="ielm"><span class="prompt">*</span> <code class="elisp"><kbd>(let ((json-object-type 'hash-table))
    (json-read-from-string <span class="string">"{\\"foo\\": true}"</span>))</kbd>
<samp>#&lt;hash-table 'equal nil 1/65 0x314f800&gt;</samp>
<span class="prompt">*</span> <kbd>(gethash <span class="string">"foo"</span> *)</kbd>
<samp>t</samp></code></pre></li>
      </ul>

      <p>
        json.el generally does the right thing when encoding idiomatic
        lisp data structures:
      </p>

      <pre class="ielm"><span class="prompt">*</span> <code class="elisp"><kbd>(json-encode '(1 2 3))</kbd>
<samp class="string">"[1, 2, 3]"</samp></code>
<span class="prompt">*</span> <code class="elisp"><kbd>(json-encode '(<span class="builtin">:foo</span> 1 <span class="builtin">:bar</span> 2 <span class="builtin">:baz</span> 3))</kbd>
<samp class="string">"{\\"foo\\":1, \\"bar\\":2, \\"baz\\":3}"</samp></code></pre>
</div>

<dl class="entry-meta">

<dt>Related links</dt>
<dd>
<ul>
<li><a href="/2005/09/editing-javascript-in-emacs" rel="related">Editing JavaScript in Emacs</a></li>
</ul>
</dd>
<dt>Tags</dt>
<dd>
<a href="/tags/elisp" rel="tag">elisp</a>
<a href="/tags/emacs" rel="tag">emacs</a>
<a href="/tags/emacs-lisp" rel="tag">emacs lisp</a>
<a href="/tags/hack" rel="tag">hack</a>
<a href="/tags/javascript" rel="tag">javascript</a>
<a href="/tags/json" rel="tag">json</a>
</dd>

<dt>Pingbacks</dt>
<dd>
<ul>
<li><a href="/2007/01/twitter-from-emacs" rel="nofollow">Emacsen.org » Blog Archive » Twitter from Emacs</a></li>
</ul>
</dd>

<dt>Rights</dt>
<dd class="entry-rights">
<p>
        Copyright © 2005 – 2007 <a href="http://edward.oconnor.cx/" rel="me">Edward O’Connor</a>.
      </p>
      <p>
        This work is licensed to you under version 2 of the <a href="http://www.gnu.org/">GNU</a> <a href="http://www.gnu.org/licenses/old-licenses/gpl-2.0.txt">General
        Public License</a>.

        Alternatively, you may choose to receive this work under any
        other license that grants the right to use, copy, modify, and/or
        distribute the work, as long as that license imposes the
        restriction that derivative works have to grant the same rights
        and impose the same restriction.

        For example, you may choose to receive this work under the <a href="http://www.gnu.org/">GNU</a> <a href="http://www.gnu.org/licenses/fdl.txt">Free Documentation
        License</a>, the <a href="http://creativecommons.org/">CreativeCommons</a> <a href="http://creativecommons.org/licenses/sa/1.0/">ShareAlike</a>
        License, the XEmacs manual license, or <a href="http://www.emacswiki.org/OLD">similar licenses</a>.
      </p>
</dd>
</dl>
</div> <!-- .hentry -->

<div id="comments">
<h3>Comments</h3>

<ol>
<li id="comment-f52cd60dc5b4366fbfc65ad684e8a7d8" class="">
<div class="gravatar">
<img src="http://www.gravatar.com/avatar.php?gravatar_id=b7b4ca9dbc3ab2a5d9adda0e4ac3ebbc&amp;rating=X&amp;size=80"
width="80" height="80" alt="Brian Palmer" />
</div>
<blockquote><p>
        Just curious, did you develop this for any reason in particular?
        I've been looking at json lately for a project I'm going to try
        to ajaxify...
      </p></blockquote>
<p class="source">
&#8212;
<cite>Brian Palmer</cite>,
<abbr title="2006-04-01T15:30:11-05:00"><a href="#comment-f52cd60dc5b4366fbfc65ad684e8a7d8">1 April 2006</a></abbr>
</p>
</li>

</ol>

<form action="/comment/2006/03/json.el" method="post">
<fieldset>
<legend>Add a comment</legend>

<p id="comment-status">
  <img src="/images/throbber.gif" alt="Posting..." />
  <span class="output"></span>
</p>

<div>
<input type="text" id="name" name="name" value=""
       placeholder="John Doe" />
<label for="name">Your name (<em>required</em>)</label>
</div>

<div>
<input type="text" id="email" name="email" value=""
       placeholder="foobar@gmail.com" />
<label for="email">
Email address (<em>required</em>, won&#8217;t be shared)
</label>
</div>

<div>
<input type="text" id="url" name="url" value=""
       placeholder="http://example.com/" />
<label for="url">Your website's URL</label>
</div>

<div>
<input type="text" id="irc-nick" name="irc-nick" value="" />
<label for="irc-nick">
What's my IRC nick?
(<em>required</em>, it’s <samp>hober</samp>)
</label>
</div>

<div class="editor">
<textarea id="comment-content" name="content"
          placeholder="Something positive, insightful, and awesome"
          rows="8" cols="72"></textarea>
<div id="preview"></div>
<label for="comment-content">Use <a
href="http://daringfireball.net/projects/markdown/syntax">Markdown</a>
for formatting. Live preview powered
by <a href="http://www.attacklab.net/showdown-gui.html">showdown</a>.
</label>
</div>

<div>
<button type="submit" id="comment-submit">Post</button>
<label for="comment-submit">
By posting, you place your comment
under <a href="http://creativecommons.org/licenses/by-sa/3.0/"
rel="license">CC BY-SA 3.0</a>.
</label>
</div>
</fieldset>
</form>
</div> <!-- #comments -->
</div> <!-- #primary -->
</div> <!-- #content -->
</body>
</html>
