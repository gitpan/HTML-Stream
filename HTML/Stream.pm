package HTML::Stream;

=head1 NAME

HTML::Stream - HTML output stream class, and some markup utilities


=head1 DESCRIPTION

This module provides you with an object-oriented (and subclassable)
way of outputting HTML.  Basically, you open up an "HTML stream"
on an existing filehandle, and then do all of your output to the
HTML stream (you can intermix HTML-stream-output and ordinary-print-output,
if you like).

Here's small sample of the different ways you can use this module:

      use HTML::Stream;
      $HTML = new HTML::Stream \*STDOUT;
      
      # The vanilla interface...
      tag  $HTML 'A', HREF=>"$href";
      tag  $HTML 'IMG', SRC=>"logo.gif", ALT=>"LOGO";
      text $HTML "My caption!";
      tag  $HTML '_A';
      text $HTML $a_lot_of_text;

      # The chocolate interface (with whipped cream)...
      $HTML -> A(HREF=>"$href")
            -> IMG(SRC=>"logo.gif", ALT=>"LOGO")
            -> t("My caption!")
            -> _A
            -> t($a_lot_of_text);

      # The strawberry interface...
      output $HTML [A, HREF=>"$href"], 
                   [IMG, SRC=>"logo.gif", ALT=>"LOGO"],
                   "My caption!",
                   [_A];
      output $HTML $a_lot_of_text;


=head2 Function interface

Let's start out with the simple stuff.
This module provides a collection of non-OO utility functions
for escaping HTML text and producing HTML tags, like this:

    use HTML::Stream qw(:funcs);        # imports functions from @EXPORT_OK
    
    print html_tag(A, HREF=>$url);
    print '&copy; 1996 by', html_escape($myname), '!';
    print html_tag('/A');

By the way: that last line could be rewritten as:

    print html_tag(_A);

And if you need to get a parameter in your tag that doesn't have an
associated value, supply the I<undefined> value (I<not> the empty string!):

    print html_tag(TD, NOWRAP=>undef, ALIGN=>'LEFT');
    
         <TD NOWRAP ALIGN=LEFT>
    
    print html_tag(IMG, SRC=>'logo.gif', ALT=>'');
    
         <IMG SRC="logo.gif" ALT="">

There are also some routines for reversing the process, like:

    $text = "This <i>isn't</i> &quot;fun&quot;...";    
    print html_unmarkup($text);
       
         This isn't &quot;fun&quot;...
      
    print html_unescape($text);
       
         This isn't "fun"...

I<Yeah, yeah, yeah>, I hear you cry.  I<We've seen this stuff before.>
But wait!  There's more...


=head2 OO interface, vanilla

Using the function interface can be tedious... so we also
provide an B<"HTML output stream"> class.  Messages to an instance of
that class generally tell that stream to output some HTML.  Here's the
above example, rewritten using HTML streams:

    use HTML::Stream;
    $HTML = new HTML::Stream \*STDOUT;
    
    tag  $HTML 'A', HREF=>$url;
    ent  $HTML 'copy';
    text $HTML " 1996 by $myname!";
    tag  $HTML '_A';

Or, if indirect-object syntax ain't your thang:

    $HTML->tag(A, HREF=>$url);
    $HTML->ent('copy');
    $HTML->text(" 1996 by $myname!");
    $HTML->tag(_A);

As you've probably guessed:

    ent()      Outputs an HTML entity, like C<&copy;>.
    tag()      Outputs an ordinary tag, like <A>, possibly with parameters.
               The parameters will all be HTML-escaped automatically.
    text()     Outputs some text, which will be HTML-escaped.

It you're I<not> using indirect-object syntax, you might prefer
to use C<t()> and C<e()> instead of C<text()> and C<ent()>: they
are absolutely identical... just shorter to type:

    $HTML -> tag(A, HREF=>$url);
    $HTML -> e('copy');
    $HTML -> t(" 1996 by $myname!");
    $HTML -> tag(_A);

Now, it wouldn't be nice to give you those C<text()> and C<ent()> shortcuts
without giving you one for C<tag()>, would it?  Of course not...


=head2 OO interface, chocolate

The known HTML tags are even given their own B<tag-methods,> compiled on 
demand... so the above could be written like this:

    $HTML -> A(HREF=>$url);
    $HTML -> e('copy');
    $HTML -> t(" 1996 by $myname!");
    $HTML -> _A;

As you've probably guessed:

    A(HREF=>$url)   ==   tag(A, HREF=>$url)   ==   <A HREF="/the/url">
    _A              ==   tag(_A)              ==   </A>

All such "tag-methods" use the tagname in I<all-uppercase>.
A C<"_"> prefix on any tag-method means that an end-tag is desired.
The C<"_"> was chosen for several reasons: 
(1) it's short and easy to type,
(2) it doesn't produce much visual clutter to look at,
(3) C<_TAG> looks a little like C</TAG> because of the straight line.

=over 

=item *

I<I know, I know... it looks like a private method.  You get used to it.  
Really.>

=back

I should stress that this module will only auto-create tag methods
for B<known> HTML tags.  So you're protected from typos like this
(which will cause a fatal exception at run-time):

    $HTML -> IMGG(SRC=>$src);

(You're not yet protected from illegal tag parameters, but it's a start, 
ain't it?)

If you need to make a tag known (sorry, but this is currently a 
I<global> operation, and not stream-specific), do this:

    HTML::Stream->accept_tag('MARQUEE');     # for you MSIE fans...

B<There is no corresponding "reject_tag">.  I thought and thought
about it, and could not convince myself that such a method would 
do anything more useful that cause other people's modules to suddenly
stop working because some bozo function decided to reject the C<FONT> tag.


=head2 OO interface, with whipped cream

In the grand tradition of C++, output method chaining is supported
in both the Vanilla Interface and the Chocolate Interface.  
So you can (and probably should) say:

    $HTML -> A(HREF=>$url) 
          -> e('copy') -> t("1996 by $myname!") 
          -> _A;

But wait... there's one more flavor...


=head2 OO interface, strawberry

I was jealous of the compact syntax of HTML::AsSubs, but I didn't
want to worry about clogging the namespace with a lot of functions
like p(), a(), etc. (especially when markup-functions like tr() conflict
with existing Perl functions).  So I came up with this:

    output $HTML [A, HREF=>$url], "Here's my $caption", [_A];

Conceptually, arrayrefs are sent to C<html_tag()>, and strings to 
C<html_escape()>.


=head2 Newlines

As special cases, some tag-methods (like C<P>, C<_P>, and C<BR>) all cause 
newlines to be output before and/or after the tag, so your HTML is a little 
more readable when you do stuff like "view source" on a browser.  So:

    $HTML -> HTML 
          -> HEAD  
          -> TITLE -> t("Hello!") -> _TITLE 
          -> _HEAD
          -> BODY(BGCOLOR=>'#808080');

Actually produces:

    <HTML><HTML>
    <HEAD>
    <TITLE>Hello!</TITLE>
    </HEAD>
    <BODY BGCOLOR="#808080">

(This will improve slightly as time goes on).
You can also output newline explicitly via the special C<nl> method
in the Chocolate Interface:

    $HTML->nl;     # one newline
    $HTML->nl(6);  # six newlines


=head2 Entities

As shown above, You can use the C<ent()> (or C<e()>) method to output 
an entity:

    $HTML->t('Copyright ')->e('copy')->t(' 1996 by Me!');

But this can be a pain, particularly for Europeans:

    $HTML -> t('Copyright ') 
          -> e('copy') 
          -> t(' 1996 by Fran') -> e('ccedil') -> t('ois, Inc.!');

Sooooooooo...


=head2 Changing the way text is escaped

The default "autoescape" behavior of an HTML stream can be a drag if
you've got a lot character entities that you want to output.  So here's
how you can use the C<autoescape()> method to change the way an
HTML::Stream works at any time:

    $HTML->autoescape('ALL');        # escapes [<>"&] - the default
    $HTML->autoescape('NON_ENT');    # escapes [<>"] only, and not [&]

If you can also install your own autoescape function (note that you might
very well want to install it for just a little bit only, and then 
de-install it):

    sub my_autoescape {
        my $text = shift;
	$text = HTML::Stream::html_escape_all($text);   # start with default
        $text =~ s/\(c\)/&copy;/ig;        # (C) becomes copyright
        $text =~ s/\\,(c)/\&$1cedil;/ig;   # \,c becomes a cedilla
 	$text;
    }

    # Start using my autoescape:
    my $oldesc = $HTML->autoescape(\&my_autoescape);      # use sub refs ONLY!
    $HTML-> ADDRESS;
    $HTML-> IMG(SRC=>'logo.gif', ALT=>'Fran\,cois, Inc');
    output $HTML 'Copyright (C) 1996 by Fran\,cois, Inc.!';
    $HTML->_ADDRESS;
    
    # Stop using my autoescape:
    $HTML->autoescape($oldesc);

By the way, the following are equivalent:

    $HTML->autoescape('ALL')
    $HTML->autoescape(\&HTML::Stream::escape_all);

No arguments to C<autoescape()> returns the current autoescape function.


=head2 Outputting HTML to things besides filehandles

As of Revision 1.21, you no longer need to supply C<new()> with a 
filehandle: I<any object that responds to a print() method will do>.
Of course, this includes B<blessed> FileHandles.

If you supply a GLOB reference (like C<\*STDOUT>) or a string (like
C<"Module::FH">), HTML::Stream will automatically create an invisible
object for talking to that filehandle (I don't dare bless it into a
FileHandle, since it'd get closed when the HTML::Stream is destroyed,
and you might not like that).

You say you want to print to a string?  For kicks and giggles, try this:

    package StringHandle;
    sub new {
	my $self = '';
	bless \$self, shift;
    }
    sub print {
        my $self = shift;
        $$self .= join('', @_);
    }
    
  
    package main;
    use HTML::Stream;
    
    my $SH = new StringHandle;
    my $HTML = new HTML::Stream $SH;
    $HTML -> H1 -> "<Hello & welcome!>" -> _H1;
    print "PRINTED STRING: ", $$SH, "\n";


=head2 Subclassing

This is where you can make your application-specific HTML-generating code
I<much> easier to look at.  Consider this:

    package MY::HTML;
    @ISA = qw(HTML::Stream);
     
    sub Aside {
	$_[0] -> FONT(SIZE=>-1) -> I;
    }
    sub _Aside {
	$_[0] -> _I -> _FONT;
    }

Now, you can do this:

    my $HTML = new MY::HTML \*STDOUT;
    
    $HTML -> Aside
          -> t("Don't drink the milk, it's spoiled... pass it on...")
          -> _Aside;

If you're defining these markup-like, chocolate-interface-style functions,
I recommend using mixed case with a leading capital.  You probably 
shouldn't use all-uppercase, since that's what this module uses for
real HTML tags.


=head1 PERFORMANCE

Slower than I'd like.  Both the output() method and the various "tag" 
methods seem to run about 5 times slower than the old 
just-hardcode-the-darn stuff approach.  That is, in general, this:

    ### Approach #1...
    tag  $HTML 'A', HREF=>"$href";
    tag  $HTML 'IMG', SRC=>"logo.gif", ALT=>"LOGO";
    text $HTML "My caption!";
    tag  $HTML '_A';
    text $HTML $a_lot_of_text;

And this:

    ### Approach #2...
    output $HTML [A, HREF=>"$href"], 
	         [IMG, SRC=>"logo.gif", ALT=>"LOGO"],
		 "My caption!",
		 [_A];
    output $HTML $a_lot_of_text;

And this:

    ### Approach #3...
    $HTML -> A(HREF=>"$href")
	  -> IMG(SRC=>"logo.gif", ALT=>"LOGO")
	  -> t("My caption!")
	  -> _A
          -> t($a_lot_of_text);

Each run about 5x slower than this:

    ### Approach #4...
    print '<A HREF="', html_escape($href), '>',
          '<IMG SRC="logo.gif" ALT="LOGO">',
  	  "My caption!",
          '</A>';
    print html_escape($a_lot_of_text);

Of course, I'd much rather use any of first three I<(especially #3)> 
if I had to get something done right in a hurry.  Or did you not notice
the typo in approach #4?  C<;-)>

(BTW, thanks to Benchmark:: for allowing me to... er... benchmark stuff.)


=head1 WHY IN THE WORLD DID I WRITE THIS?

I was just mucking about with different ways of generating large
HTML documents, seeing which ways I liked the most/least.


=head1 VERSION

$Revision: 1.24 $


=head1 AUTHOR

Eryq, eryq@rhine.gsfc.nasa.gov .  

Enjoy.

=cut

use Carp;
use Exporter;
use strict;
use vars qw(@ISA %EXPORT_TAGS $AUTOLOAD $DASH_TO_SLASH $VERSION %Tags);

# Exporting...
@ISA = qw(Exporter);
%EXPORT_TAGS = (
      'funcs' => [qw(html_escape html_unescape html_unmarkup html_tag)]
);
Exporter::export_ok_tags('funcs');

# Version...
( $VERSION ) = '$Revision: 1.24 $ ' =~ /\$Revision:\s+([^\s]+)/;
         


#------------------------------------------------------------
#
# GLOBALS
#
#------------------------------------------------------------

# HTML escape sequences.  This bit was stolen from html_escape() in CGI::Base.
my %Escape = (
    '&' => '&amp;', 
    '>' => '&gt;', 
    '<' => '&lt;', 
    '"' => '&quot;'
);
my $AllEscapes    = '<>"&';
my $NonEntEscapes = '<>"';

# Allow dashes to become slashes?
$DASH_TO_SLASH = 1;


#------------------------------------------------------------
#
# PRIVATE UTILITIES
#
#------------------------------------------------------------

#------------------------------------------------------------
# escape_all TEXT
#------------------------------------------------------------
# Given a TEXT string, turn the text into valid HTML by interpolating the 
# appropriate escape sequences for angles, double-quotes, and ampersands:

sub escape_all {
    my $text = shift;
    $text =~ s/([$AllEscapes])/$Escape{$1}/mgoe; 
    $text;
}

#------------------------------------------------------------
# escape_non_ent TEXT
#------------------------------------------------------------
# Given a TEXT string, turn the text into valid HTML by interpolating the 
# appropriate escape sequences for angles and double-quotes only:

sub escape_non_ent {
    my $text = shift;
    $text =~ s/([$NonEntEscapes])/$Escape{$1}/mgoe; 
    $text;
}

#------------------------------------------------------------
# escape_none TEXT
#------------------------------------------------------------
# No-op, provided for very simple compatibility.  Just returns TEXT.

sub escape_none {
    $_[0];
}

#------------------------------------------------------------
# build_tag ESCAPEFUNC, ARRAYREF
#------------------------------------------------------------
# I<Internal use only!>  Build an HTML tag using the given ESCAPEFUNC.
# As an efficiency hack, only the values are HTML-escaped currently:
# it is assumed that the tag and parameters will already be safe.

sub build_tag {
    my $esc = shift;       # escape function
    my $taginfo = shift;   # tag info

    # Start off, converting "_x" to "/x":
    my $tag = shift @$taginfo;
    $tag =~ s|^_|/|;
    my $s = '<' . $tag;

    # Add parameters, if any:
    while (@$taginfo) {
	my $k = shift @$taginfo;
	my $v = shift @$taginfo;
	$s .= " $k";
	defined($v) and ((($s .= '="') .= &$esc($v)) .= '"');
    }
    $s .= '>';
}


#------------------------------------------------------------
#
# PUBLIC UTILITIES
#
#------------------------------------------------------------

#------------------------------------------------------------
# html_escape TEXT
#------------------------------------------------------------
# Given a TEXT string, turn the text into valid HTML by 
# interpolating the appropriate escape sequences. 

sub html_escape {
    my $text = shift;
    $text =~ s/([$AllEscapes])/$Escape{$1}/mgoe; 
    $text;
}
 
#------------------------------------------------------------
# html_tag TAG [, PARAM=>VALUE, ...]
#------------------------------------------------------------
# Return the text for a given TAG, possibly with parameters.
# As an efficiency hack, only the values are HTML-escaped currently:
# it is assumed that the tag and parameters will already be safe.
#
# For convenience and readability, you can say _A instead of '/A'
# for the first tag, if you're into barewords.

sub html_tag {
    build_tag(\&html_escape, \@_);    # warning! using ref to @_!
}

#------------------------------------------------------------
# html_unescape TEXT
#------------------------------------------------------------
# Remove <tag> markup and &-escapes.

sub html_unescape {
    my ($text) = @_;

    # Remove <tag> sequences.  KLUDGE!  I'll code a better way later.
    $text =~ s/\<[^>]+\>//g;
    $text =~ s/\&lt;/</gi;
    $text =~ s/\&gt;/>/gi;
    $text =~ s/\&quot;/"/gi;     # Just the standard " : no `` or ''
    $text =~ s/\&amp;/&/gi;
    return $text;
}

#------------------------------------------------------------
# html_unmarkup TEXT
#------------------------------------------------------------
# Remove HTML markup from TEXT.  Cheesy.

sub html_unmarkup {
    my ($text) = @_;

    # Remove <tag> sequences.  KLUDGE!  I'll code a better way later.
    $text =~ s/\<[^>]+\>//g;
    return $text;
}


#------------------------------------------------------------
#
# OO INTERFACE, VANILLA
#
#------------------------------------------------------------

# Special mapping from names to utility functions:
my %AutoEscapeSubs = 
    ('ALL'     => \&HTML::Stream::escape_all,
     'NON_ENT' => \&HTML::Stream::escape_non_ent,
     );


#------------------------------------------------------------
# new [PRINTABLE] 
#------------------------------------------------------------
# Create a new HTML output stream.
# If no PRINTABLE is given, does a select() and uses that.

sub new {
    my $class = shift;
    my $out = shift || select;      # defaults to current output stream

    # If it looks like an unblessed filehandle, bless it:
    if (!ref($out) || ref($out) eq 'GLOB') {
	$out = new HTML::Stream::FileHandle $out;
    }

    # Create the object:
    my $self = { 
	OUT  => $out,
	Esc => \&escape_all,
    };
    bless $self, $class;
}

#------------------------------------------------------------
# DESTROY
#------------------------------------------------------------
# Destructor.  Does I<not> close the filehandle!

sub DESTROY { 1 }

#------------------------------------------------------------
# autoescape [NAME]
# autoescape [SUBREF]
#------------------------------------------------------------
# Set the autoescape function for this HTML stream.
#
# If a textual name is given, then one of the appropriate built-in 
# functions is used.  If the argument is a subroutine reference, 
# then that subroutine will be used.
#
# Returns the previously-installed function, in the manner of C<select()>.
# No arguments just returns the currently-installed function.

sub autoescape {
    my $self = shift;

    # Grab existing value:
    my $oldesc = $self->{Esc}; 

    # If arguments were given, they specify the new value:
    if (@_) { 
	my $newesc = shift;
	if (ref($newesc) ne 'CODE') {  # must be a string: map it to a subref
	    $newesc = $AutoEscapeSubs{uc($newesc)} or
		croak "never heard of autoescape option '$newesc'";
	}
	$self->{Esc} = $newesc;
    }

    # Return old value:
    $oldesc;
}

#------------------------------------------------------------
# comment COMMENT
#------------------------------------------------------------
# Output an HTML comment.

sub comment {
    my $self = shift;
    $self->{OUT}->print('<!-- ', &{$self->{Esc}}(join('',@_)), ' -->');
    $self;
}

#------------------------------------------------------------
# ent ENTITY
#------------------------------------------------------------
# Output an HTML entity.  For example, here's how you'd output C<&nbsp;>:
#     
#      $html->ent('nbsp');
#
# B<Warning:> this function assumes that the entity argument is legal.

sub ent {
    my ($self, $entity) = @_;
    $self->{OUT}->print("\&$entity;");
    $self;
}

# Make a synonym:
*e = \&ent;


#------------------------------------------------------------
# nl COUNT
#------------------------------------------------------------
# Output COUNT newlines.  If undefined, COUNT defaults to 1.

sub nl {
    my ($self, $count) = @_;
    $self->{OUT}->print("\n" x (defined($count) ? $count : 1));
    $self;
}

#------------------------------------------------------------
# tag TAGNAME [, PARAM=>VALUE, ...]
#------------------------------------------------------------
# Output a tag.  Returns the self object, to allow method chaining.
# You can specify _A for '/A' if you're into barewords.

sub tag {
    my $self = shift;
    $self->{OUT}->print(build_tag($self->{Esc}, \@_));
    $self;
}

#------------------------------------------------------------
# text TEXT, ..., TEXT
#------------------------------------------------------------
# Output some text. Returns the self object, to allow method chaining.

sub text {
    my $self = shift;
    $self->{OUT}->print(&{$self->{Esc}}(join('',@_)));
    $self;
}

# Make a synonym:
*t = \&text;


#------------------------------------------------------------
#
# OO INTERFACE, STRAWBERRY
#
#------------------------------------------------------------

#------------------------------------------------------------
# output ITEM,...,ITEM
#------------------------------------------------------------
# Go through the items.  If an item is an arrayref, treat it like
# the array argument to html_tag() and output the result.  If an item
# is a text string, escape the text and output the result.  Like this:
#
#     output $HTML [A, HREF=>$url], "Here's my $caption!", [_A];

sub output {
    my $self = shift;
    my $out = $self->{OUT};
    my $esc = $self->{Esc};
    foreach (@_) {
	if (ref($_) eq 'ARRAY') {    # E.g., $_ is [A, HREF=>$url]
	    $out->print(&build_tag($esc, $_));
	}
	elsif (!ref($_)) {           # E.g., $_ is "Some text"
	    $out->print(&$esc($_));
	}
	else {
	    confess "bad argument to output: $_";
	}
    }
    $self;        # heh... why not...
}


#------------------------------------------------------------
#
# OO INTERFACE, CHOCOLATE
#
#------------------------------------------------------------

# The known HTML tags. 
# The value is a set of flags:
#     0x01    newline before <TAG>
#     0x02    newline after <TAG>
#     0x04    newline before </TAG>
#     0x08    newline after </TAG>
# This can be summarized as:

my $TP     = 1 | 0 | 0 | 0;
my $TBR    = 0 | 2 | 0 | 0;
my $TFONT  = 0 | 0 | 0 | 0;  # fontlike
my $TOUTER = 1 | 0 | 0 | 8;
my $TBOTH  = 0 | 2 | 0 | 8;
my $TLIST  = 0 | 2 | 0 | 8;
my $TELEM  = 0 | 0 | 0 | 0; 
my $TTITLE = 0 | 0 | 0 | 8;
my $TSOLO  = 0 | 2 | 0 | 0;

%Tags = 
    (
     A       => 0,
     ADDRESS => $TBOTH,
     APPLET  => $TBOTH,
     B       => 0,
     BASE    => 0,
  BLOCKQUOTE => $TBOTH,
     BODY    => $TBOTH,
     BR      => $TBR,
     CAPTION => $TTITLE,
     CENTER  => $TBOTH,
     CODE    => 0,
     DD      => $TLIST,
     DIV     => $TOUTER,
     DL      => $TELEM,
     DT      => $TELEM,
     EM      => 0,
     FONT    => 0,
     FORM    => $TBOTH,
     H1      => $TTITLE,
     H2      => $TTITLE,
     H3      => $TTITLE,
     H4      => $TTITLE,
     H5      => $TTITLE,
     H6      => $TTITLE,
     HEAD    => $TBOTH,
     HR      => $TBOTH,
     HTML    => $TBOTH,
     I       => 0,
     IMG     => 0,
     INPUT   => 0,
     ISINDEX => 0,
     LI      => $TELEM,
     LINK    => 0,
     META    => $TSOLO,
     OBJECT  => 0,
     OL      => $TLIST, 
     P       => $TP,
     PRE     => $TOUTER,
     SELECT  => 0,
     SMALL   => 0,
     STRONG  => 0,
     SUB     => 0,
     SUP     => 0,
     TABLE   => $TBOTH,
     TD      => 0,
    TEXTAREA => 0,
     TH      => 0,
     TITLE   => $TTITLE,
     TR      => $TOUTER,
     TT      => 0,
     U       => 0,
     UL      => $TLIST, 
     VAR     => 0,
     );

#------------------------------------------------------------
# accept_tag TAG
#------------------------------------------------------------
# I<Class method>.
# Accept the given TAG in the Chocolate Interface.

sub accept_tag {
    my ($class, $tag) = @_;
    my $tag = uc($tag);        # it's GOT to be uppercase!!!
    unless ($Tags{$tag}) {
	$Tags{$tag} = 0;       # no newlines
    }
    1;
}

#------------------------------------------------------------
# AUTOLOAD
#------------------------------------------------------------
# The custom autoloader, for the chocolate interface.
#
# B<WARNING:> I have no idea if the mechanism I use to put the
# functions in this module (HTML::Stream) is perlitically correct.

sub AUTOLOAD {
    my $funcname = $AUTOLOAD;
    $funcname =~ s/.*:://;            # get rid of package name 
    my $tag;
    ($tag = $funcname) =~ s/^_//;     # get rid of leading "_"

    # If it's a tag method...
    if (defined($Tags{$tag})) {

	# A begin-tag, like "IMG"...
	if ($funcname !~ /^_/) {     
	    my $BEFORE = ($Tags{$tag} & 1 ? '"\n",' : '');
	    my $AFTER  = ($Tags{$tag} & 2 ? ',"\n"' : '');
	    eval <<EOF;
            sub HTML::Stream::$funcname { 
		my \$self = shift; 
                \$self->{OUT}->print($BEFORE html_tag('$tag',\@_) $AFTER);
                \$self;
            }
EOF
	}
        # An end-tag, like "_IMG"...
	else { 
            my $BEFORE = ($Tags{$tag} & 4 ? '"\n",' : '');
            my $AFTER  = ($Tags{$tag} & 8 ? ',"\n"' : ''); 
	    eval <<EOF;
            sub HTML::Stream::$funcname { 
                \$_[0]->{OUT}->print($BEFORE "</$tag>" $AFTER);
                \$_[0];
            }
EOF
	}
	if ($@) { $@ =~ s/ at .*\n//; croak $@ }   # die!
        my $fn = "HTML::Stream::$funcname";        # KLUDGE: is this right???
        goto &$fn;
    }

    # If it's NOT a tag method...
    else { 
	# probably should call the *real* autoloader in the future...
	croak "Sorry: $AUTOLOAD is neither defined or loadable";
    }
    goto &$AUTOLOAD;
}


# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
# = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

# A small, private package for turning FileHandles into safe printables:

package HTML::Stream::FileHandle;
no strict 'refs';
sub new {
    my ($class, $raw) = @_;
    bless \$raw, $class;
}
sub print {
    my $self = shift;
    print { $$self } @_;
}

#------------------------------------------------------------
1;

