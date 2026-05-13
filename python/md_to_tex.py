import re
import sys

def convert_md_to_tex(md_file, tex_file):
    with open(md_file, 'r') as f:
        lines = f.readlines()
        
    tex_lines = []
    in_itemize = False
    in_sub_itemize = False
    
    for line in lines:
        line = line.rstrip()
        
        # Replace inline bold and code
        line = re.sub(r'\*\*(.*?)\*\*', r'\\textbf{\1}', line)
        line = re.sub(r'`(.*?)`', r'\\texttt{\1}', line)
        line = re.sub(r'&', r'\\&', line)
        line = re.sub(r'%', r'\\%', line)
        line = re.sub(r'\$', r'\\$', line)
        line = re.sub(r'_', r'\\_', line)
        
        # Headings
        if line.startswith('# '):
            if in_sub_itemize: tex_lines.append("\\end{itemize}"); in_sub_itemize = False
            if in_itemize: tex_lines.append("\\end{itemize}"); in_itemize = False
            tex_lines.append(f"\\chapter{{{line[2:].strip()}}}")
            continue
        elif line.startswith('## '):
            if in_sub_itemize: tex_lines.append("\\end{itemize}"); in_sub_itemize = False
            if in_itemize: tex_lines.append("\\end{itemize}"); in_itemize = False
            tex_lines.append(f"\\section{{{line[3:].strip()}}}")
            continue
        elif line.startswith('### '):
            if in_sub_itemize: tex_lines.append("\\end{itemize}"); in_sub_itemize = False
            if in_itemize: tex_lines.append("\\end{itemize}"); in_itemize = False
            tex_lines.append(f"\\subsection{{{line[4:].strip()}}}")
            continue
            
        # Lists
        if line.startswith('* ') or line.startswith('- ') or re.match(r'^\d+\.\s', line):
            if in_sub_itemize:
                tex_lines.append("\\end{itemize}")
                in_sub_itemize = False
            if not in_itemize:
                tex_lines.append("\\begin{itemize}")
                in_itemize = True
            
            content = line.lstrip('*-0123456789. ')
            tex_lines.append(f"\\item {content}")
            continue
            
        elif line.startswith('  * ') or line.startswith('    * '):
            if not in_itemize:
                tex_lines.append("\\begin{itemize}")
                in_itemize = True
            if not in_sub_itemize:
                tex_lines.append("\\begin{itemize}")
                in_sub_itemize = True
                
            content = line.lstrip(' *')
            tex_lines.append(f"\\item {content}")
            continue
            
        else:
            if in_sub_itemize:
                tex_lines.append("\\end{itemize}")
                in_sub_itemize = False
            if in_itemize and not line.strip():
                tex_lines.append("\\end{itemize}")
                in_itemize = False
            
            tex_lines.append(line)

    if in_sub_itemize: tex_lines.append("\\end{itemize}")
    if in_itemize: tex_lines.append("\\end{itemize}")

    with open(tex_file, 'w') as f:
        f.write('\n'.join(tex_lines))

convert_md_to_tex('new_changes.md', 'latex/changelog.tex')
