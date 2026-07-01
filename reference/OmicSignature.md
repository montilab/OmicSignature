# OmicSignature R6 object

An R6 object to store signatures generated from experiments, including
metadata, signature, and an optional differential expression analysis
result dataframe.

\`OmicSignature\` uses R6 reference semantics: assigning an object to a
new variable does not make an independent copy. Both variables point to
the same mutable object, so changes through one variable are visible
through the other. Use \`\$clone()\` when an independent copy is needed,
and \`\$clone(deep = TRUE)\` when nested R6 objects also need to be
copied.

## Active bindings

- `metadata`:

  a list to describe the metadata

- `signature`:

  a dataframe contains probe_id, feature_name, score (optional) and
  group_label (optional)

- `difexp`:

  a dataframe for differential expression result

- `removeDifexp`:

  a function to remove difexp from the object

## Methods

### Public methods

- [`OmicSignature$new()`](#method-OmicSignature-new)

- [`OmicSignature$print()`](#method-OmicSignature-print)

- [`OmicSignature$extractSignature()`](#method-OmicSignature-extractSignature)

- [`OmicSignature$clone()`](#method-OmicSignature-clone)

------------------------------------------------------------------------

### Method `new()`

Create a new OmicSignature object

#### Usage

    OmicSignature$new(metadata, signature, difexp = NULL, print_message = FALSE)

#### Arguments

- `metadata`:

  required. a list. See \`createMetadata\` for more information

- `signature`:

  required. a vector, or a dataframe including columns: "probe_id",
  "feature_name" and "group_label", and an optional column "score"

- `difexp`:

  optional

- `print_message`:

  use TRUE if want to see all messages printed

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print an OmicSignature object

#### Usage

    OmicSignature$print()

------------------------------------------------------------------------

### Method `extractSignature()`

#### Usage

    OmicSignature$extractSignature(conditions)

#### Arguments

- `conditions`:

  conditions for new signatures

#### Returns

a dataframe of new signatures

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OmicSignature$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
