import os
import re

# Derive ordering for Rust files
derive_pattern = re.compile(r"^#\[derive\((\s*[a-zA-Z0-9_]+\s*,)*(\s*[a-zA-Z0-9_]+\s*)\)]$")
special_derives = ["Debug", "Clone", "Copy", "Default", "PartialEq", "Eq", "PartialOrd", "Ord", "Serialize", "Deserialize", "Hash"]
special_derives = {x: i for i, x in enumerate(special_derives)}

# TOML sections to sort
toml_sections_to_sort = [
    "[dependencies]",
    "[dev-dependencies]",
    "[build-dependencies]",
    "[workspace.dependencies]",
    "[workspace.lints]",
    "[workspace.lints.clippy]",
    "[workspace.lints.rust]",
]


def process_rust_file(path):
    """Sort derive macros in Rust files."""
    lines = []
    with open(path, "r") as f:
        for line in f:
            if derive_pattern.match(line.rstrip()):
                derives = line.rstrip()[9:-2]  # Remove #[derive( and )]
                derives = [(special_derives.get(x.strip(), 100), x.strip()) for x in derives.split(",")]
                derives.sort()
                line = "#[derive(" + ", ".join(x[1] for x in derives) + ")]\n"
            lines.append(line)
    
    with open(path, "w") as f:
        f.writelines(lines)


def sort_toml_block(block):
    """Sort a block of TOML key-value pairs alphabetically by key."""
    if not block:
        return block
    # Sort by the key (everything before '=' or the whole line if no '=')
    return sorted(block, key=lambda line: line.split("=")[0].strip().lower())


def process_toml_file(path):
    """Sort specified sections in TOML files, respecting comment-separated blocks."""
    with open(path, "r") as f:
        lines = f.readlines()
    
    result = []
    i = 0
    
    while i < len(lines):
        line = lines[i]
        stripped = line.strip()
        
        # Check if this is a section we want to sort
        is_target_section = any(stripped == section for section in toml_sections_to_sort)
        
        if is_target_section:
            # Add the section header
            result.append(line)
            i += 1
            
            # Collect and sort blocks within this section
            while i < len(lines):
                current_line = lines[i]
                current_stripped = current_line.strip()
                
                # Stop if we hit a new section
                if current_stripped.startswith("[") and not current_stripped.startswith("[["):
                    break
                
                # Skip empty lines, add them directly
                if not current_stripped:
                    result.append(current_line)
                    i += 1
                    continue
                
                # If it's a comment, add it and start a new block after
                if current_stripped.startswith("#"):
                    result.append(current_line)
                    i += 1
                    continue
                
                # Collect a block of key-value pairs until comment, empty line, or new section
                block = []
                while i < len(lines):
                    bl = lines[i]
                    bs = bl.strip()
                    
                    # Stop block on new section
                    if bs.startswith("[") and not bs.startswith("[["):
                        break
                    # Stop block on empty line or comment
                    if not bs or bs.startswith("#"):
                        break
                    
                    block.append(bl)
                    i += 1
                
                # Sort and add the block
                sorted_block = sort_toml_block(block)
                result.extend(sorted_block)
        else:
            result.append(line)
            i += 1
    
    with open(path, "w") as f:
        f.writelines(result)


for root, dirs, files in os.walk("."):
    if "target" in root:
        continue
    for name in files:
        path = os.path.join(root, name)

        if name.endswith(".rs"):
            print(f"[RS] {path}")
            process_rust_file(path)
        elif name.endswith(".toml"):
            print(f"[TOML] {path}")
            process_toml_file(path)