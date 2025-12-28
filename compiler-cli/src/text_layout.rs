use ecow::EcoString;

/// Generates a string delimeted table with 2 spaces between each column, columns padded with
/// enough spaces to be aligned, and hyphens under the headers (excluding the final column of each
/// row). Rows should have the right number of columns.
///
/// ## Example
///
/// ```txt
/// Package  Current  Latest
/// -------  -------  ------
/// wibble   1.4.0    1.4.1
/// wobble   1.0.1    2.3.0
/// ```
///
pub fn space_table<Grid, Row, Cell>(headers: &[impl AsRef<str>], data: Grid) -> EcoString
where
    Grid: AsRef<[Row]>,
    Row: AsRef<[Cell]>,
    Cell: AsRef<str>,
{
    let mut output = EcoString::new();

    let mut column_widths: Vec<usize> =
        headers.iter().map(|header| header.as_ref().len()).collect();

    for row in data.as_ref() {
        for (index, cell) in row.as_ref().iter().enumerate() {
            if let Some(width) = column_widths.get_mut(index) {
                let cell = cell.as_ref();
                *width = (*width).max(cell.len());
            }
        }
    }

    for (index, (header, width)) in headers.iter().zip(column_widths.iter()).enumerate() {
        if index > 0 {
            output.push_str("  ");
        }
        let header = header.as_ref();
        output.push_str(header);
        if index < headers.len() - 1 {
            let padding = width - header.len();
            if padding > 0 {
                output.push_str(&" ".repeat(padding));
            }
        }
    }
    output.push('\n');

    for (index, (header, width)) in headers.iter().zip(column_widths.iter()).enumerate() {
        if index > 0 {
            output.push_str("  ");
        }
        let header = header.as_ref();
        output.push_str(&"-".repeat(header.len()));
        if index < headers.len() - 1 {
            let padding = width - header.len();
            if padding > 0 {
                output.push_str(&" ".repeat(padding));
            }
        }
    }
    output.push('\n');

    for row in data.as_ref() {
        for (index, (cell, width)) in row.as_ref().iter().zip(column_widths.iter()).enumerate() {
            if index > 0 {
                output.push_str("  ");
            }
            let cell = cell.as_ref();
            output.push_str(cell);
            if index < headers.len() - 1 {
                let padding = width - cell.len();
                if padding > 0 {
                    output.push_str(&" ".repeat(padding));
                }
            }
        }
        output.push('\n');
    }

    output
}
