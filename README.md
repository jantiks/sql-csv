# sql-csv

## Overview

`sql-csv` is a tool to run SQL like queries on CSV files. Currently, it supports `SELECT` and `DELETE` operations.

## Prerequisites

- **Stack**: This project uses stack as the build tool.
[Haskell Stack](https://docs.haskellstack.org/en/stable/README/).

## Installation

1. **Clone the Repository**:
2. **Build**
    `stack build`

## Usage
### Select
`stack exec sql-csv-exe 'SELECT * FROM /path/to/your/data.csv WHERE age = 30'`

### Delete
`stack exec sql-csv-exe 'DELETE FROM /path/to/your/data.csv WHERE age = 30'`

### Insert
`stack exec sql-csv-exe 'INSERT INTO /path/to/your/data.csv (name, age, city) VALUES (Frank, 28, Seattle)'`

### Update
`stack exec sql-csv-exe 'UPDATE /path/to/your/data.csv SET name=VZGO WHERE name=Charlie'`