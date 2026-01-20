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
            # Simple regex-based chunking for org files as fallback
            # This ensures we can handle all org files regardless of structure
            import re
            
            # Pattern to match org headings at any level
            heading_pattern = re.compile(r'^(\*+)\s+(.*?)(?:\s+(:[a-zA-Z0-9_@:]+:))?\s*$', re.MULTILINE)
            
            # Find all headings and their positions
            headings = list(heading_pattern.finditer(chunk_text))
            
            if not headings:
                # No headings found, process as paragraphs
                para_chunks = chunk_by_paragraphs(chunk_text)
                for para_text, para_line in para_chunks:
                    adjusted_line = base_line + para_line - 1
                    if len(para_text) > chunk_size:
                        process_chunk(para_text, adjusted_line, depth + 1)
                    elif len(para_text) >= min_chunk_size:
                        chunks.append((para_text, adjusted_line))
                return
            
            # Process content before first heading
            first_heading_start = headings[0].start()
            if first_heading_start > 0:
                prefix_text = chunk_text[:first_heading_start].strip()
                if prefix_text:
                    para_chunks = chunk_by_paragraphs(prefix_text)
                    for para_text, para_line in para_chunks:
                        adjusted_line = base_line + para_line - 1
                        if len(para_text) > chunk_size:
                            process_chunk(para_text, adjusted_line, depth + 1)
                        elif len(para_text) >= min_chunk_size:
                            chunks.append((para_text, adjusted_line))
            
            # Process each heading and its content
            for i, heading_match in enumerate(headings):
                heading_start = heading_match.start()
                heading_end = heading_match.end()
                
                # Calculate line number for this heading
                heading_line = base_line + chunk_text[:heading_start].count('\n') + 1
                
                # Get next heading start for content boundary
                next_heading_start = headings[i+1].start() if i+1 < len(headings) else len(chunk_text)
                
                # Extract heading and content
                heading_line_text = chunk_text[heading_start:heading_end]
                content_text = chunk_text[heading_end:next_heading_start].strip()
                
                # Build chunk content
                if include_heading:
                    if content_text:
                        full_chunk = f"{heading_line_text}\n{content_text}"
                    else:
                        full_chunk = heading_line_text
                else:
                    full_chunk = content_text if content_text else ""
                
                if full_chunk:
                    if len(full_chunk) <= chunk_size:
                        if len(full_chunk) >= min_chunk_size:
                            chunks.append((full_chunk, heading_line))
                        else:
                            # Chunk too small, process as paragraphs
                            para_chunks = chunk_by_paragraphs(full_chunk)
                            for para_text, para_line in para_chunks:
                                adjusted_line = heading_line + para_line - 1
                                if len(para_text) >= min_chunk_size:
                                    chunks.append((para_text, adjusted_line))
                    else:
                        # Chunk too large, process recursively
                        process_chunk(full_chunk, heading_line, depth + 1)
        except Exception as e:
            # Fallback to simple paragraph chunking if anything goes wrong
            print(f"Warning: Org parsing failed, falling back to paragraph chunking: {e}")
            para_chunks = chunk_by_paragraphs(chunk_text)
            for para_text, para_line in para_chunks:
                adjusted_line = base_line + para_line - 1
                if len(para_text) > chunk_size:
                    process_chunk(para_text, adjusted_line, depth + 1)
                elif len(para_text) >= min_chunk_size:
                    chunks.append((para_text, adjusted_line))

    process_chunk(text, 1)
    return chunks
