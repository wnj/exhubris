use comfy_table::CellAlignment;
use size::Size;

use crate::alloc::Allocations;

pub fn simple_table<'a>(content: impl IntoIterator<Item = (&'a str, &'a str)>) {
    let mut table = comfy_table::Table::new();
    table.load_preset(comfy_table::presets::UTF8_FULL);
    table.apply_modifier(comfy_table::modifiers::UTF8_ROUND_CORNERS);
    table.apply_modifier(comfy_table::modifiers::UTF8_SOLID_INNER_BORDERS);
    table.set_content_arrangement(comfy_table::ContentArrangement::Dynamic);

    for row in content {
        table.add_row([row.0, row.1]);
    }

    println!();
    println!();
    println!("{table}");
    println!();
}

pub fn banner(content: impl core::fmt::Display) {
    let mut table = comfy_table::Table::new();
    table.load_preset(comfy_table::presets::UTF8_FULL);
    table.apply_modifier(comfy_table::modifiers::UTF8_ROUND_CORNERS);
    table.set_content_arrangement(comfy_table::ContentArrangement::Dynamic);

    table.add_row([content]);

    println!();
    println!();
    println!("{table}");
    println!();
}

pub fn print_allocations(allocs: &Allocations) {
    let mut table = comfy_table::Table::new();
    table.load_preset(comfy_table::presets::NOTHING);
    table.set_header(["MEMORY", "TASK", "START", "END", "SIZE", "LOST"]);
    table.column_mut(2).unwrap().set_cell_alignment(CellAlignment::Right);
    table.column_mut(3).unwrap().set_cell_alignment(CellAlignment::Right);
    table.column_mut(4).unwrap().set_cell_alignment(CellAlignment::Right);
    table.column_mut(5).unwrap().set_cell_alignment(CellAlignment::Right);
    for (region_name, regallocs) in allocs.by_region() {
        let mut regallocs = regallocs.iter().collect::<Vec<_>>();
        regallocs.sort_by_key(|(_name, ta)| ta.base);
        let mut total = 0;
        let mut loss = 0;

        let mut last = None;
        for (task_name, talloc) in regallocs {
            let base = talloc.base;
            let req_size = talloc.requested;

            if let Some(last) = last {
                if base != last {
                    let pad_size = base - last;
                    total += pad_size;
                    loss += pad_size;
                    table.add_row([
                        region_name.to_string(),
                        "-pad-".to_string(),
                        format!("{:#x}", last),
                        format!("{:#x}", base - 1),
                        Size::from_bytes(pad_size).to_string(),
                        Size::from_bytes(pad_size).to_string(),
                    ]);
                }
            }
            let size = talloc.sizes.iter().sum::<u64>();
            let internal_pad = size - req_size;
            table.add_row([
                region_name.to_string(),
                task_name.to_string(),
                format!("{:#x}", base),
                format!("{:#x}", base + size - 1),
                Size::from_bytes(size).to_string(),
                Size::from_bytes(internal_pad).to_string(),
            ]);
            total += size;
            loss += internal_pad;

            last = Some(base + size);
        }
        table.add_row([
            region_name.to_string(),
            "(total)".to_string(),
            String::new(),
            String::new(),
            Size::from_bytes(total).to_string(),
            Size::from_bytes(loss).to_string(),
        ]);
    }
    println!("{table}");
}
