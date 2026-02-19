#!/usr/bin/env python3
import sys
import re
import os

files = [os.path.expanduser('~/org/main/Tasks.org'), os.path.expanduser('~/org/main/Inbox.org')]
# Pattern to match headings starting with multiple DL: prefixes
# It looks for lines starting with one or more stars, followed by space, then multiple DL: 
dl_pattern = re.compile(r'^(\*+[ \t]+)DL:[ \t]+DL:[ \t]+')

replacements_made = 0

for file_path in files:
    if not os.path.exists(file_path):
        continue
        
    with open(file_path, 'r') as f:
        lines = f.readlines()
    
    new_lines = []
    for line in lines:
        if dl_pattern.match(line):
            # Replace 'DL: DL: ' with just 'DL: '
            new_line = dl_pattern.sub(r'\1DL: ', line)
            new_lines.append(new_line)
            replacements_made += 1
        else:
            new_lines.append(line)
            
    with open(file_path, 'w') as f:
        f.writelines(new_lines)

print(f"Successfully cleaned up {replacements_made} headings with double 'DL:' prefixes.")
