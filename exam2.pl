use strict;
use warnings;
use Text::Creolize;
use Encode;

{
    package WordCorrector;
    
    sub new {
        my($class) = @_;
        return bless {
            items => [],
            resources => {},
        }, $class;
    }

    sub visit_link {
        my($self, $link, $title, $builder) = @_;
        if ($link !~ m/\A(?:https?|ftp):/mosx
            && ! exists $self->{resources}{$link}
        ) {
            $self->{resources}{$link} = $link;
            if ($builder->{list}) {
                push @{$self->{items}}, $link;
            }
        }
        return $builder->visit_link($link, $title, $builder);
    }
    
    sub visit_image {
        my($self, $link, $title, $builder) = @_;
        return $builder->visit_image($link, $title, $builder);
    }

    sub items { return @{$_[0]->{items}} }
}

my $creolize = Text::Creolize->new({
    link_visitor => WordCorrector->new,
});

my $wiki_source = do {
    use utf8;
<<'EOS';
== [[種別:目次]]

[[Creolize]] のあれやこれや。

# [[Creolizeのあらまし]]
## [[Creolizeの一歩]]
## [[Creolize書式紹介]]
## [[Creolizeを組み込む]]
# [[Creolize書式詳細]]
## [[Creolizeの段落]]
## [[Creolizeのヘッディング]]
## [[Creolizeのリスト]]
*** [[Creolizeの順序付きリスト]]
*** [[Creolizeの順序なしリスト]]
*** [[Creolizeの記述リスト]]
## [[Creolizeのインデント段落]]
## [[Creolizeの整形済み]]
## [[CreolizeのWikiリンク]]
## [[Creolizeの文字修飾]]
*** [[Creolizeの改行]]
*** [[Creolizeの太文字強調]]
*** [[Creolizeの斜体強調]]
*** [[Creolizeの上付き・下付き]]
*** [[Creolizeのnowiki]]
*** [[Creolizeの等幅]]
# [[Creolizeのカスタマイズ]]
## [[Creolizeのリンク加工]]
## [[Creolizeのプラグイン処理]]
## [[Creolizeのマークアップ変更]]

[[Creolizeの作者]]
EOS
};

my $xhtml = $creolize->convert($wiki_source)->result;
print Encode::encode('UTF-8', $xhtml);
for ($creolize->link_visitor->items) {
    print Encode::encode('UTF-8', $_), "\n";
}

