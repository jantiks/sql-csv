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
`stack exec sql-csv-exe '/path/to/your/data.csv SELECT * FROM /path/to/your/data.csv WHERE age = 30'`

### Delete
`stack exec sql-csv-exe '/path/to/your/data.csv DELETE FROM /path/to/your/data.csv WHERE age = 30'`