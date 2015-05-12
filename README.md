Relational
==========

Relational allows the Haskell programmer to work with columns, schemas,
projections, and conditions to compose selections, insertions, updates, and
deletions on tables. Lots of information is carried in the types, so the
programmer can rest easy, safe in the knowledge that

  - Every selection projects onto columns which are in the table's schema
  - Every insertion provides a value of the right type for each column in the
    table's schema
  - Every update provides a value of the right type for each column to be
    updated, and the columns to be updated are a subset of the table's schema
  - Every column which is used in a condition on a selection, deletion, or
    update is in the table's schema
