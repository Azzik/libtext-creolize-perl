use strict;
use warnings;
use Text::Creolize;
use Encode;

my $wiki_source = do {
    use utf8;
<<'EOS';
== first heading
== second heading
=== second a
=== second b
=== second c
EOS
};
my $creolize = Text::Creolize->new({toc => 1});
my $xhtml = $creolize->convert($wiki_source)->result;
print Encode::encode('UTF-8', $xhtml);
__END__
<div class="toc">
<ul>
<li><a href="#h0c8qjyk">first heading</a></li>
<li><a href="#h00ggpvq">second heading</a>
<ul>
<li><a href="#h0rmhl0b">second a</a></li>
<li><a href="#h12t762g">second b</a></li>
<li><a href="#h00tp7h4">second c</a></li>
</ul>
</li>
</ul>
</div>
<h2 id="h0c8qjyk">first heading</h2>
<h2 id="h00ggpvq">second heading</h2>
<h3 id="h0rmhl0b">second a</h3>
<h3 id="h12t762g">second b</h3>
<h3 id="h00tp7h4">second c</h3>

