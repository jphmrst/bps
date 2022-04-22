
Note that the *repository* version numbers do not directly correspond
to the release numbers of either the Haskell or Scala releases.
Haskell releases adhere to Haskell's [Package Versioning
Policy](https://pvp.haskell.org/); Scala releases use the [Semantic
Versioning spec](https://semver.org/).

# Version 0.5.0 (Haskell 0.1.0.0, Scala 0.1.0)

 - First version with Haskell: working implementation of a monad
   transformer for the JTMS and ATMS.  The `interpretations` and
   `explain-node` functions of the ATMS are not yet translated, and
   are omitted from the module API.

 - Promoting Scala release to 0.1.0.  No significant additions, but it
   will be nice to distinguish new feature additions at 0.x.0 from
   patches and documentation additions at 0.x.y.

# Version 0.4.0 (Scala only, 0.0.4)

 - Documentation for both JTMS and ATMS.

 - Generator for large randomized ATMS examples.

 - Some tweaks to data structure selection based on rough profiling.
 
# Version 0.3.0 (Scala only, 0.0.3)

 - Contains a mostly-working version of a standalone ATMS.

# Version 0.2.0 (Scala only, 0.0.2)

 - Separation of the standalone JTMS from (untranslated) JTMS+JTRE
   wrapper.

    - Further testing and debugging of the standalone JTMS.

    - Scaladoc documentation of the standalone JTMS.

    - There may be additional type parameters in later versions of
      the standalone JTMS.

 - First version containing this file.

# Version 0.1.0 (Scala only, 0.0.1)

Initial release

 - Contains a mostly-working version of the standalone JTMS.
