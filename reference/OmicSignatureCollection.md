# OmicSignatureCollection R6 object

a R6 object to store a collection of OmicSignature objects. In cluding
metadata, OmicSigList which is a list of OmicSignature object.

## Active bindings

- `metadata`:

  a list to describe the metadata

- `OmicSigList`:

  a list of OmicSignature object(s)

## Methods

### Public methods

- [`OmicSignatureCollection$new()`](#method-OmicSignatureCollection-new)

- [`OmicSignatureCollection$print()`](#method-OmicSignatureCollection-print)

- [`OmicSignatureCollection$extractSignature()`](#method-OmicSignatureCollection-extractSignature)

- [`OmicSignatureCollection$metadataSummary()`](#method-OmicSignatureCollection-metadataSummary)

- [`OmicSignatureCollection$clone()`](#method-OmicSignatureCollection-clone)

------------------------------------------------------------------------

### Method `new()`

Create an OmicSignatureCollection object

#### Usage

    OmicSignatureCollection$new(metadata, OmicSigList, print_message = FALSE)

#### Arguments

- `metadata`:

  required, must be a list

- `OmicSigList`:

  required, a list of OmicSignature R6 objects

- `print_message`:

  use TRUE if want to see all messages printed

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print an OmicSignatureCollection object

#### Usage

    OmicSignatureCollection$print()

------------------------------------------------------------------------

### Method `extractSignature()`

#### Usage

    OmicSignatureCollection$extractSignature(conditions, bind = TRUE)

#### Arguments

- `conditions`:

  conditions for new signatures

- `bind`:

  use TRUE to return all results in a single dataframe. Otherwise, will
  return a list contains the result of each OmicSignature individually

#### Returns

a dataframe or a list of new signatures

------------------------------------------------------------------------

### Method `metadataSummary()`

#### Usage

    OmicSignatureCollection$metadataSummary(only_shared = TRUE)

#### Arguments

- `only_shared`:

  use TRUE to only print the shared metadata fields in the
  OmicSignatures

#### Returns

a dataframe of the summary of the metadata

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OmicSignatureCollection$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
