
###
# XML::SAX::Expat - SAX2 Driver for Expat (XML::Parser)
# Robin Berjon <robin@knowscape.com>
# 18/11/2001 - v.0.05
# 15/10/2001 - v.0.01
###

package XML::SAX::Expat;
use strict;
use base qw(XML::SAX::Base);
use XML::NamespaceSupport   qw();
use XML::Parser             qw();

use vars qw($VERSION);
$VERSION = '0.05';

#-------------------------------------------------------------------#
# constructor
#-------------------------------------------------------------------#
sub new {
    my $class   = ref($_[0]) ? ref(shift) : shift;
    my $self    = bless {}, $class;
    %$self = %{$self->get_options(@_)};
    return $self;
}
#-------------------------------------------------------------------#



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
                                Init        => sub { $self->_handle_init(@_) },
                                Final       => sub { $self->_handle_final(@_) },
                                Start       => sub { $self->_handle_start(@_) },
                                End         => sub { $self->_handle_end(@_) },
                                Char        => sub { $self->_handle_char(@_) },
                                Comment     => sub { $self->_handle_comment(@_) },
                                Proc        => sub { $self->_handle_proc(@_) },
                                CdataStart  => sub { $self->_handle_start_cdata(@_) },
                                CdataEnd    => sub { $self->_handle_end_cdata(@_) },
                                #Unparsed
                                #Notation
                                #ExternEnt
                                #ExternEntFin
                                #Entity
                                #Element
                                #Attlist
                                #Doctype
                                #DoctypeFin
                                #XMLDecl
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

    my $element = pop @{$self->{_NodeStack}};
    delete $element->{Attributes};
    $self->SUPER::end_element($element);

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
PerlSAX2 specification, available above. Apart from the bits that are
presently missing, everything is implemented down to the letter.

=head1 TODO

  - reuse Ken's tests and add more
  - implement the entire spec

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
