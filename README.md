# A tutorial for GraphQL-Erlang

This repository contains a tutorial for the graphql-erlang system. It
implements (a subset of) the SWAPI as an example project in order to
clarify how the system is supposed to be used in a full
implementation. The idea is that it can be used as a starting point for
your own implementation of your own GraphQL schema.

# Documentation

The primary URL for the document is:

https://shopgun.github.io/graphql-erlang-tutorial/

To read the latest version of the documentation, go there and start
reading!

## Building

To build this software you need:

* rebar3
* Erlang/OTP - Version 19.3.3 was used in preparing this document

The needed dependencies will be pulled in as part of building the
software.

To build the documentation you need:

* asciidoctor - We use asciidoctor's tagging feature to refer to
  source code in the repository inside the documentation. I installed
  it with `gem install asciidoctor`

In addition you need:

* A decent amount of Erlang knowledge. This document doesn't explain
  Erlang in any way, and assumes prior knowledge.
* A decent amount of web knowledge.
* Some GraphQL knowledge. If you don't know what GraphQL is, this
  document may not be the best initial exposition. Some terminology
  is taken for granted in advance. On the other hand, this document
  explains how GraphQL fits into the Erlang world.
