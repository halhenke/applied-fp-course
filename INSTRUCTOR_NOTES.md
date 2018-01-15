Compilation of instructions and important things to remember for the different
levels.

# Level 1

We're going to be building an extremely simple web server using the 'wai'
framework. Wai is a low level HTTP implementation similar to WSGI in Python or
Rack in Ruby.

- Hackage
  - Primary repository for Haskell packages and their documentation.
  - Package **Index** is extremely useful for discovering and interrogating
    packages.

# Level 2

We're going to start building our own REST application by converting our "spec"
into a data structure that will guide the construction of the rest of the
application.

We'll also demonstrate the usefulness of ``newtype`` and how it can trivially
make code more type safe, as well as providing more descriptive type signatures.

# Level 3

Our application is starting to grow we should add another layer of assurance by
writing some tests.

We will cover:
- How to write a ``test-suite`` in the cabal file.
- Usage of the ``hspec`` library as our test runner and how to feed it our
  application.
- We will also write some tests using the ``hspec-wai`` package to give us some
  basic confidence in our error handling.

# Level 4

# Level 5

# Level 6

# Level 7

- structure repo
- readme in each repo

- monoid instance - rehash single/multiple number of possible instances. Don't let people hang too long on this point.

- how to find the documentation for the Header / ContentType

- more instruction that lead people to hackage documentation for Text/ByteString etc

- mention that import lists may need to be updated for the new types

- some editors will need to jump in and out of the different levels (close, cd, re-open)

- level05: Hardcode db name, once they have it running then add it to the config

- Mention that it's fine to use case statements for Either handling, we make it okay at the end.

- Call out the encoding instances & the automatic deriving of the ToJSON instances

!IMPORTANT!: Stephen Diehl - What I Wish I Knew Learning Haskell
