
###
# XML::SAX::Expat - SAX2 Driver for Expat (XML::Parser)
# Robin Berjon <robin@knowscape.com>
# 06/12/2001 - v.0.30
# 15/10/2001 - v.0.01
###

package XML::SAX::Expat;
use strict;
use base qw(XML::SAX::Base);
use XML::NamespaceSupport   qw();
use XML::Parser             qw();

use vars qw($VERSION);
$VERSION = '0.30';


#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#
#`,`, Variations on parse `,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,#
#```````````````````````````````````````````````````````````````````#

#-------------------------------------------------------------------#
# CharacterStream
#-------------------------------------------------------------------#
sub _parse_characterstream {
    my $p       = shift;
    my $xml     = shift;
    my $opt     = shift;

    my $expat = $p->_create_parser($opt);
    my $result = $expat->parse($xml);
    $p->_cleanup;
    return $result;
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# ByteStream
#-------------------------------------------------------------------#
sub _parse_bytestream {
    my $p       = shift;
    my $xml     = shift;
    my $opt     = shift;

    my $expat = $p->_create_parser($opt);
    my $result = $expat->parse($xml);
    $p->_cleanup;
    return $result;
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# String
#-------------------------------------------------------------------#
sub _parse_string {
    my $p       = shift;
    my $xml     = shift;
    my $opt     = shift;

    my $expat = $p->_create_parser($opt);
    my $result = $expat->parse($xml);
    $p->_cleanup;
    return $result;
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# SystemId
#-------------------------------------------------------------------#
sub _parse_systemid {
    my $p       = shift;
    my $xml     = shift;
    my $opt     = shift;

    my $expat = $p->_create_parser($opt);
    my $result = $expat->parsefile($xml);
    $p->_cleanup;
    return $result;
}
#-------------------------------------------------------------------#


#-------------------------------------------------------------------#
# $p->_create_parser(\%options)
#-------------------------------------------------------------------#
sub _create_parser {
    my $self = shift;
    my $opt  = shift;

    die "ParserReference: parser instance ($self) already parsing\n"
         if defined $self->{_InParse};

    my $expat = XML::Parser->new(
                            Handlers => {
                                Init        => sub { $self->_handle_init(@_)            },
                                Final       => sub { $self->_handle_final(@_)           },
                                Start       => sub { $self->_handle_start(@_)           },
                                End         => sub { $self->_handle_end(@_)             },
                                Char        => sub { $self->_handle_char(@_)            },
                                Comment     => sub { $self->_handle_comment(@_)         },
                                Proc        => sub { $self->_handle_proc(@_)            },
                                CdataStart  => sub { $self->_handle_start_cdata(@_)     },
                                CdataEnd    => sub { $self->_handle_end_cdata(@_)       },
                                Unparsed    => sub { $self->_handle_unparsed_entity(@_) },
                                Notation    => sub { $self->_handle_notation_decl(@_)   },
                                #ExternEnt
                                #ExternEntFin
                                Entity      => sub { $self->_handle_entity_decl(@_)     },
                                Element     => sub { $self->_handle_element_decl(@_)    },
                                Attlist     => sub { $self->_handle_attr_decl(@_)       },
                                Doctype     => sub { $self->_handle_start_doctype(@_)   },
                                DoctypeFin  => sub { $self->_handle_end_doctype(@_)     },
                                XMLDecl     => sub { $self->_handle_xml_decl(@_)        },
                                        }
                                  );

    $self->{_InParse} = 1;
    $self->{_NodeStack} = [];
    $self->{_NSStack} = [];
    $self->{_NSHelper} = XML::NamespaceSupport->new({xmlns => 1});

    return $expat;
}
#-------------------------------------------------------------------#


#-------------------------------------------------------------------#
# $p->_cleanup
#-------------------------------------------------------------------#
sub _cleanup {
    my $self = shift;

    $self->{_InParse} = 0;
    delete $self->{_NodeStack};
}
#-------------------------------------------------------------------#



#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#
#`,`, Expat Handlers ,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,#
#```````````````````````````````````````````````````````````````````#

#-------------------------------------------------------------------#
# _handle_init
#-------------------------------------------------------------------#
sub _handle_init {
    my $self    = shift;
    my $expat   = shift;

    my $document = $self->_create_node();
    push @{$self->{_NodeStack}}, $document;
    $self->SUPER::start_document($document);
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_final
#-------------------------------------------------------------------#
sub _handle_final {
    my $self    = shift;
    my $expat   = shift;

    my $document = pop @{$self->{_NodeStack}};
    return $self->SUPER::end_document($document);
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_start
#-------------------------------------------------------------------#
sub _handle_start {
    my $self    = shift;
    my $expat   = shift;
    my $e_name  = shift;
    my %attr    = @_;

    # take care of namespaces
    $self->{_NSHelper}->push_context;
    my @new_ns;
    while (my ($k, $v) = each %attr) {
        if ($k =~ m/^xmlns(:(.*))?$/) {
            my $prefix = $2 || '';
            $self->{_NSHelper}->declare_prefix($prefix, $v);
            my $ns = $self->_create_node(
                                        Prefix       => $prefix,
                                        NamespaceURI => $v
                                         );
            push @new_ns, $ns;
            $self->SUPER::start_prefix_mapping($ns);
        }
    }
    push @{$self->{_NSStack}}, \@new_ns;


    # create the attributes
    my %saxattr;
    while (my ($k, $v) = each %attr) {
        my ($ns,$prefix,$lname) = $self->{_NSHelper}->process_attribute_name($k);
        $ns ||= '';
        $prefix ||= '';
        $lname ||= '';
        my $at = $self->_create_node(
                                    Name         => $k,
                                    LocalName    => $lname,
                                    Prefix       => $prefix,
                                    Value        => $v,
                                    NamespaceURI => $ns,
                                    );
        $saxattr{"{$ns}$lname"} = $at;
    }

    # now the element
    my ($ns,$prefix,$lname) = $self->{_NSHelper}->process_element_name($e_name);
    $ns ||= '';
    $prefix ||= '';
    $lname ||= '';
    my $element = $self->_create_node(
                                    Name         => $e_name,
                                    LocalName    => $lname,
                                    Prefix       => $prefix,
                                    NamespaceURI => $ns,
                                    Attributes   => \%saxattr,
                                   );

    push @{$self->{_NodeStack}}, $element;
    $self->SUPER::start_element($element);
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_end
#-------------------------------------------------------------------#
sub _handle_end {
    my $self = shift;

    my %element = %{pop @{$self->{_NodeStack}}};
    delete $element{Attributes};
    $self->SUPER::end_element(\%element);

    my $prev_ns = pop @{$self->{_NSStack}};
    for my $ns (@$prev_ns) {
        $self->SUPER::end_prefix_mapping($ns);
    }
    $self->{_NSHelper}->pop_context;
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_char
#-------------------------------------------------------------------#
sub _handle_char {
    my $self    = shift;
    my $expat   = shift;
    my $string  = shift;

    my $characters = $self->_create_node(Data => $string);
    $self->SUPER::characters($characters);
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_comment
#-------------------------------------------------------------------#
sub _handle_comment {
    my $self    = shift;
    my $expat   = shift;
    my $string  = shift;

    my $comment = $self->_create_node(Data => $string);
    $self->SUPER::comment($comment);
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_proc
#-------------------------------------------------------------------#
sub _handle_proc {
    my $self    = shift;
    my $expat   = shift;
    my $target  = shift;
    my $data    = shift;

    my $pi = $self->_create_node(
                                Target => $target,
                                Data   => $data,
                                );
    $self->SUPER::processing_instruction($pi);
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_start_cdata
#-------------------------------------------------------------------#
sub _handle_start_cdata {
    $_[0]->SUPER::start_cdata();
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_end_cdata
#-------------------------------------------------------------------#
sub _handle_end_cdata {
    $_[0]->SUPER::end_cdata();
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_xml_decl
#-------------------------------------------------------------------#
sub _handle_xml_decl {
    my $self        = shift;
    my $expat       = shift;
    my $version     = shift;
    my $encoding    = shift;
    my $standalone  = shift;

    if (not defined $standalone) { $standalone = '';    }
    elsif ($standalone)          { $standalone = 'yes'; }
    else                         { $standalone = 'no';  }
    my $xd = $self->_create_node(
                                    Version     => $version,
                                    Encoding    => $encoding,
                                    Standalone  => $standalone,
                                );
    $self->SUPER::xml_decl($xd);
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_notation_decl
#-------------------------------------------------------------------#
sub _handle_notation_decl {
    my $self        = shift;
    my $expat       = shift;
    my $notation    = shift;
    shift;
    my $system      = shift;
    my $public      = shift;

    my $not = $self->_create_node(
                                    Name        => $notation,
                                    PublicId    => $public,
                                    SystemId    => $system,
                                 );
    $self->SUPER::notation_decl($not);
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_unparsed_entity
#-------------------------------------------------------------------#
sub _handle_unparsed_entity {
    my $self        = shift;
    my $expat       = shift;
    my $name        = shift;
    my $system      = shift;
    my $public      = shift;
    my $notation    = shift;

    my $ue = $self->_create_node(
                                    Name        => $name,
                                    PublicId    => $public,
                                    SystemId    => $system,
                                    Notation    => $notation,
                                 );
    $self->SUPER::unparsed_entity_decl($ue);
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_element_decl
#-------------------------------------------------------------------#
sub _handle_element_decl {
    my $self    = shift;
    my $expat   = shift;
    my $name    = shift;
    my $model   = shift;

    my $ed = $self->_create_node(
                                    Name    => $name,
                                    Model   => $model,
                                 );
    $self->SUPER::element_decl($ed);
}
#-------------------------------------------------------------------#


#-------------------------------------------------------------------#
# _handle_attr_decl
#-------------------------------------------------------------------#
sub _handle_attr_decl {
    my $self    = shift;
    my $expat   = shift;
    my $ename   = shift;
    my $aname   = shift;
    my $type    = shift;
    my $default = shift;
    my $fixed   = shift;

    my ($vd, $value);
    if ($fixed) {
        $vd = '#FIXED';
        $default =~ s/^(?:"|')//; #"
        $default =~ s/(?:"|')$//; #"
        $value = $default;
    }
    else {
        if ($default =~ m/^#/) {
            $vd = $default;
            $value = '';
        }
        else {
            $vd = ''; # maybe there's a default ?
            $default =~ s/^(?:"|')//; #"
            $default =~ s/(?:"|')$//; #"
            $value = $default;
        }
    }

    my $at = $self->_create_node(
                                    eName           => $ename,
                                    aName           => $aname,
                                    Type            => $type,
                                    ValueDefault    => $vd,
                                    Value           => $value,
                                );
    $self->SUPER::attribute_decl($at);
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_entity_decl
#-------------------------------------------------------------------#
sub _handle_entity_decl {
    my $self    = shift;
    my $expat   = shift;
    my $name    = shift;
    my $val     = shift;
    my $sys     = shift;
    my $pub     = shift;
    my $ndata   = shift;
    my $isprm   = shift;

    # deal with param ents
    if ($isprm) {
        $name = '%' . $name;
    }

    # int vs ext
    if ($val) {
        my $ent = $self->_create_node(
                                        Name    => $name,
                                        Value   => $val,
                                     );
        $self->SUPER::internal_entity_decl($ent);
    }
    else {
        my $ent = $self->_create_node(
                                        Name        => $name,
                                        PublicId    => $pub,
                                        SystemId    => $sys,
                                     );
        $self->SUPER::external_entity_decl($ent);
    }
}
#-------------------------------------------------------------------#


#-------------------------------------------------------------------#
# _handle_start_doctype
#-------------------------------------------------------------------#
sub _handle_start_doctype {
    my $self    = shift;
    my $expat   = shift;
    my $name    = shift;
    my $sys     = shift;
    my $pub     = shift;

    my $dtd = $self->_create_node(
                                    Name        => $name,
                                    SystemId    => $sys,
                                    PublicId    => $pub,
                                 );
    $self->SUPER::start_dtd($dtd);
}
#-------------------------------------------------------------------#

#-------------------------------------------------------------------#
# _handle_end_doctype
#-------------------------------------------------------------------#
sub _handle_end_doctype {
    $_[0]->SUPER::end_dtd();
}
#-------------------------------------------------------------------#







#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#
#`,`, Private Helpers `,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,#
#```````````````````````````````````````````````````````````````````#

#-------------------------------------------------------------------#
# _create_node
#-------------------------------------------------------------------#
sub _create_node {
    shift;
    # this may check for a factory later
    return {@_};
}
#-------------------------------------------------------------------#


1;
#,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,#
#`,`, Documentation `,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,`,#
#```````````````````````````````````````````````````````````````````#

=pod

=head1 NAME

XML::SAX::Expat - SAX2 Driver for Expat (XML::Parser)

=head1 SYNOPSIS

  use XML::SAX::Expat;
  use XML::SAX::MyFooHandler;
  my $h = XML::SAX::MyFooHandler->new;
  my $p = XML::SAX::Expat->new(Handler => $h);
  $p->parse_file('/path/to/foo.xml');

=head1 DESCRIPTION

This is an implementation of a SAX2 driver sitting on top of Expat
(XML::Parser) which Ken MacLeod posted to perl-xml and which I have
updated.

It is still incomplete, though most of the basic SAX2 events should be
available. The SAX2 spec is currently available from
http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/~checkout~/perl-xml/libxml-perl/doc/sax-2.0.html?rev=HEAD&content-type=text/html

A more friendly URL as well as a PODification of the spec are in the
works.

=head1 METHODS

The methods defined in this class correspond to those listed in the
PerlSAX2 specification, available above.


=head1 MISSING PARTS

XML::Parser has no listed callbacks for the following events, which
are therefore not presently generated (ways may be found in the
future):

  * ignorable_whitespace
  * skipped_entity
  * start_entity / end_entity
  * resolve_entity

Ways of signalling them are welcome. In addition to those,
set_document_locator is not yet called.

=head1 TODO

  - reuse Ken's tests and add more

=head1 AUTHOR

Robin Berjon, robin@knowscape.com; stolen from Ken Macleod,
ken@bitsko.slc.ut.us, and with suggestions and feedback from
perl-xml.

=head1 COPYRIGHT

Copyright (c) 2001 Robin Berjon. All rights reserved. This program is
free software; you can redistribute it and/or modify it under the same
terms as Perl itself.

=head1 SEE ALSO

XML::Parser::PerlSAX

=cut
