"""Org-mode text chunking utilities."""

import re
from typing import List, Tuple


def chunk_by_org_headings(text: str, include_heading: bool = True) -> List[Tuple[str, int]]:
    """
    Split org-mode text by headings using orgparse.

    Each chunk contains a top-level heading and all its sub-content.
    Nested headings are included in their parent's chunk.

    Args:
        text: Input org-mode text
        include_heading: Whether to include the heading line in the chunk

    Returns:
        List of (chunk_text, line_number) tuples
        line_number is 1-based starting line for chunk
    """
    try:
        import orgparse
    except ImportError:
        raise ImportError(
            "orgparse is required for org-mode chunking. "
            "Install it with: pip install orgparse"
        )

    org = orgparse.loads(text)
    chunks = []

    for heading in org[1:]:
        if heading.level == 1:
            heading_line = heading.linenumber
            
            content_parts = [heading.get_body(format='raw')]
            for child in heading:
                if child != heading:  # Skip self
                    child_heading = f"{'*' * child.level} {child.heading}"
                    if child.tags:
                        tags_str = ':'.join([''] + sorted(list(child.tags)) + [''])
                        child_heading += f" {tags_str}"
                    child_body = child.get_body(format='raw')
                    content_parts.append(f"{child_heading}\n{child_body}" if child_body else child_heading)
            
            content = '\n'.join(content_parts)

            if include_heading:
                heading_text = f"{'*' * heading.level} {heading.heading}"
                if heading.tags:
                    tags_str = ':'.join([''] + sorted(list(heading.tags)) + [''])
                    heading_text += f" {tags_str}"

                if content:
                    chunk_content = f"{heading_text}\n{content}"
                else:
                    chunk_content = heading_text
            else:
                chunk_content = content if content else ""

            if chunk_content:
                chunks.append((chunk_content, heading_line))

    return chunks


def chunk_by_paragraphs(text: str) -> List[Tuple[str, int]]:
    """
    Split text by paragraphs (empty lines).

    Args:
        text: Input text

    Returns:
        List of (paragraph_text, line_number) tuples
    """
    paragraphs = []
    lines = text.split('\n')
    current_paragraph = []
    start_line = 1

    for line_num, line in enumerate(lines, 1):
        stripped = line.strip()
        
        if not stripped:
            if current_paragraph:
                paragraph_text = '\n'.join(current_paragraph)
                paragraphs.append((paragraph_text, start_line))
                current_paragraph = []
            start_line = line_num + 1
        else:
            if not current_paragraph:
                start_line = line_num
            current_paragraph.append(line)

    if current_paragraph:
        paragraph_text = '\n'.join(current_paragraph)
        paragraphs.append((paragraph_text, start_line))

    return paragraphs


def chunk_by_sentences(text: str) -> List[Tuple[str, int]]:
    """
    Split text by sentences.

    Args:
        text: Input text

    Returns:
        List of (sentence_text, line_number) tuples
    """
    sentence_pattern = re.compile(r'(?<=[.!?])\s+(?=[A-Z])')
    sentences = sentence_pattern.split(text)
    
    chunks = []
    current_line = 1
    for sentence in sentences:
        sentence = sentence.strip()
        if sentence:
            chunks.append((sentence, current_line))
            current_line += sentence.count('\n') + 1

    return chunks


def chunk_text_fixed(text: str, *, chunk_size: int, overlap: int = 0) -> List[Tuple[str, int]]:
    """
    Split text into overlapping chunks with line number tracking.

    Args:
        text: Input text to chunk
        chunk_size: Maximum characters per chunk
        overlap: Characters to overlap between chunks

    Returns:
        List of (chunk_text, line_number) tuples
        line_number is 1-based starting line for chunk

    Algorithm:
        1. Start at position 0
        2. Extract chunk [start:start+chunk_size]
        3. Calculate line number by counting '\n' before start
        4. Move start forward by (chunk_size - overlap)
        5. Repeat until end of text
    """
    chunks = []
    start = 0

    while start < len(text):
        end = min(start + chunk_size, len(text))
        chunk_content = text[start:end]

        line_number = text[:start].count('\n') + 1

        chunks.append((chunk_content, line_number))

        if end == len(text):
            break

        start = max(start + 1, end - overlap)

    return chunks


def chunk_org_recursively(
    text: str,
    *,
    chunk_size: int,
    overlap: int = 0,
    min_chunk_size: int = 10,
    include_heading: bool = True,
    oversize_strategy: str = "recursive"
) -> List[Tuple[str, int]]:
    """
    Recursively chunk org-mode text with size limits.

    Strategy:
    1. Try chunking by org headings
    2. If chunks exceed size, recursively split by sub-headings
    3. If still too large, split by paragraphs
    4. If still too large, split by sentences
    5. If still too large, use fixed-size chunking

    Args:
        text: Input org-mode text
        chunk_size: Maximum characters per chunk
        overlap: Characters to overlap between chunks (for fixed-size fallback)
        min_chunk_size: Minimum chunk size to keep
        include_heading: Whether to include heading lines
        oversize_strategy: Strategy for handling oversized chunks
            - "recursive": Recursively split (default)
            - "truncate": Truncate to chunk_size
            - "skip": Skip oversized chunks
            - "warn": Warn and skip oversized chunks

    Returns:
        List of (chunk_text, line_number) tuples
    """
    try:
        import orgparse
    except ImportError:
        raise ImportError(
            "orgparse is required for org-mode chunking. "
            "Install it with: pip install orgparse"
        )

    chunks = []

    def process_chunk(chunk_text: str, base_line: int, depth: int = 0):
        nonlocal chunks

        if oversize_strategy == "truncate":
            if len(chunk_text) > chunk_size:
                chunks.append((chunk_text[:chunk_size], base_line))
            elif len(chunk_text) >= min_chunk_size:
                chunks.append((chunk_text, base_line))
            return
        elif oversize_strategy in ["skip", "warn"]:
            if len(chunk_text) > chunk_size:
                if oversize_strategy == "warn":
                    print(f"Warning: Skipping oversized chunk ({len(chunk_text)} chars)")
                return
            elif len(chunk_text) >= min_chunk_size:
                chunks.append((chunk_text, base_line))
            return

        if depth > 5:
            fixed_chunks = chunk_text_fixed(
                chunk_text,
                chunk_size=chunk_size,
                overlap=overlap
            )
            for text, line in fixed_chunks:
                if len(text) >= min_chunk_size:
                    chunks.append((text, base_line + line - 1))
            return

        try:
            org = orgparse.loads(chunk_text)
            headings = list(org[1:])

            if not headings:
                para_chunks = chunk_by_paragraphs(chunk_text)
                for para_text, para_line in para_chunks:
                    adjusted_line = base_line + para_line - 1
                    if len(para_text) > chunk_size:
                        process_chunk(para_text, adjusted_line, depth + 1)
                    elif len(para_text) >= min_chunk_size:
                        chunks.append((para_text, adjusted_line))
                return

            first_heading_line = headings[0].linenumber
            if first_heading_line > 1:
                lines = chunk_text.split('\n')
                prefix_lines = lines[:first_heading_line - 1]
                prefix_text = '\n'.join(prefix_lines)
                if prefix_text.strip():
                    para_chunks = chunk_by_paragraphs(prefix_text)
                    for para_text, para_line in para_chunks:
                        adjusted_line = base_line + para_line - 1
                        if len(para_text) > chunk_size:
                            process_chunk(para_text, adjusted_line, depth + 1)
                        elif len(para_text) >= min_chunk_size:
                            chunks.append((para_text, adjusted_line))

            if len(headings) > 1:
                for heading in headings:
                    heading_line = base_line + heading.linenumber - 1
                    content = heading.get_body(format='raw')

                    if include_heading:
                        heading_text = f"{'*' * heading.level} {heading.heading}"
                        if heading.tags:
                            tags_str = ':'.join([''] + sorted(list(heading.tags)) + [''])
                            heading_text += f" {tags_str}"

                        if content:
                            sub_chunk = f"{heading_text}\n{content}"
                        else:
                            sub_chunk = heading_text
                    else:
                        sub_chunk = content if content else ""

                    if sub_chunk:
                        if len(sub_chunk) <= chunk_size:
                            if len(sub_chunk) >= min_chunk_size:
                                chunks.append((sub_chunk, heading_line))
                            else:
                                para_chunks = chunk_by_paragraphs(sub_chunk)
                                for para_text, para_line in para_chunks:
                                    adjusted_line = heading_line + para_line - 1
                                    if len(para_text) >= min_chunk_size:
                                        chunks.append((para_text, adjusted_line))
                        else:
                            process_chunk(sub_chunk, heading_line, depth + 1)
            else:
                if len(headings) == 1:
                    heading = headings[0]
                    heading_line = base_line + heading.linenumber - 1
                    content = heading.get_body(format='raw')

                    if include_heading:
                        heading_text = f"{'*' * heading.level} {heading.heading}"
                        if heading.tags:
                            tags_str = ':'.join([''] + sorted(list(heading.tags)) + [''])
                            heading_text += f" {tags_str}"

                        if content:
                            sub_chunk = f"{heading_text}\n{content}"
                        else:
                            sub_chunk = heading_text
                    else:
                        sub_chunk = content if content else ""

                    if sub_chunk:
                        if len(sub_chunk) <= chunk_size:
                            if len(sub_chunk) >= min_chunk_size:
                                chunks.append((sub_chunk, heading_line))
                            else:
                                para_chunks = chunk_by_paragraphs(sub_chunk)
                                for para_text, para_line in para_chunks:
                                    adjusted_line = heading_line + para_line - 1
                                    if len(para_text) >= min_chunk_size:
                                        chunks.append((para_text, adjusted_line))
                        else:
                            process_chunk(sub_chunk, heading_line, depth + 1)
                else:
                    para_chunks = chunk_by_paragraphs(chunk_text)
                    for para_text, para_line in para_chunks:
                        adjusted_line = base_line + para_line - 1
                        if len(para_text) > chunk_size:
                            process_chunk(para_text, adjusted_line, depth + 1)
                        elif len(para_text) >= min_chunk_size:
                            chunks.append((para_text, adjusted_line))
        except Exception:
            fixed_chunks = chunk_text_fixed(
                chunk_text,
                chunk_size=chunk_size,
                overlap=overlap
            )
            for text, line in fixed_chunks:
                if len(text) >= min_chunk_size:
                    chunks.append((text, base_line + line - 1))

    process_chunk(text, 1)
    return chunks
