#!/usr/bin/env python3
import sys
import re
import os

files = [os.path.expanduser('~/org/main/Tasks.org'), os.path.expanduser('~/org/main/Inbox.org')]
uuid_pattern = re.compile(r'^[ \t]*:ID:[ \t]*([a-f0-9]{8}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{4}-[a-f0-9]{12})[ \t]*$', re.IGNORECASE)

seen_ids = set()
duplicates_removed = 0

for file_path in files:
    with open(file_path, 'r') as f:
        lines = f.readlines()
    
    new_lines = []
    for line in lines:
        match = uuid_pattern.match(line)
        if match:
            uid = match.group(1).lower()
            if uid in seen_ids:
                duplicates_removed += 1
                continue # Skip this line (remove duplicate ID)
            else:
                seen_ids.add(uid)
                new_lines.append(line)
        else:
            new_lines.append(line)
            
    with open(file_path, 'w') as f:
        f.writelines(new_lines)

print(f"Successfully removed {duplicates_removed} duplicate ID entries.")
