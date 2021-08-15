# CSV Analyzer

A simple but powerful command-line tool for analyzing CSV files, written in Haskell.

## Features

- **CSV Analysis**: Get comprehensive statistics about your CSV files
  - Row and column counts
  - Column name listing
  - Automatic numeric column detection
  - Most common values for each column (top 3)

- **CSV Filtering**: Filter CSV data by column values
  - Case-insensitive filtering
  - Easy command-line interface

## Installation

### Prerequisites

- GHC (Glasgow Haskell Compiler) 8.10 or later
- Cabal 3.0 or later

### Building from source

```bash
git clone https://github.com/yourusername/csv-analyzer.git
cd csv-analyzer
cabal build
cabal install
```

## Usage

### Analyze a CSV file

```bash
csv-analyzer --file data.csv analyze
```

This will output:
- Number of rows and columns
- List of column names
- Detected numeric columns
- Most common values for each column

### Filter CSV data

```bash
csv-analyzer --file data.csv filter --column "Status" --value "Active"
```

This will show all rows where the "Status" column equals "Active" (case-insensitive).

### Help

```bash
csv-analyzer --help
csv-analyzer analyze --help
csv-analyzer filter --help
```

## Example

Given a CSV file `employees.csv`:

```csv
Name,Age,Department,Salary,Status
John Doe,30,Engineering,75000,Active
Jane Smith,25,Marketing,65000,Active
Bob Johnson,35,Engineering,80000,Inactive
Alice Brown,28,HR,60000,Active
```

### Analysis Output

```bash
$ csv-analyzer --file employees.csv analyze

=== CSV Analysis Results ===
Number of rows: 4
Number of columns: 5

Column names:
  - Name
  - Age  
  - Department
  - Salary
  - Status

Numeric columns detected:
  - Age
  - Salary

Most common values per column (top 3):
  Name:
    - Alice Brown (1)
    - Bob Johnson (1)
    - Jane Smith (1)

  Age:
    - 25 (1)
    - 28 (1)
    - 30 (1)

  Department:
    - Engineering (2)
    - HR (1)
    - Marketing (1)

  Salary:
    - 60000 (1)
    - 65000 (1)
    - 75000 (1)

  Status:
    - Active (3)
    - Inactive (1)
```

### Filter Output

```bash
$ csv-analyzer --file employees.csv filter --column "Department" --value "Engineering"

Found 2 matching rows:

John Doe, 30, Engineering, 75000, Active
Bob Johnson, 35, Engineering, 80000, Inactive
```

## Library Usage

You can also use this as a Haskell library in your own projects:

```haskell
import CSVAnalyzer
import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  content <- BL.readFile "data.csv"
  case parseCSVFile content of
    Left err -> putStrLn $ "Error: " ++ err
    Right (headers, records) -> do
      let stats = analyzeCSV headers records
      print stats
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request. For major changes, please open an issue first to discuss what you would like to change.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Why This Project?

This project demonstrates:
- Real-world Haskell application development
- Proper use of Haskell's type system for data processing
- Command-line interface design with `optparse-applicative`
- CSV parsing with the `cassava` library
- Efficient data processing with `vector` and `text`
- Clean, functional programming patterns

It's simple enough to understand quickly but useful enough for daily data analysis tasks. 