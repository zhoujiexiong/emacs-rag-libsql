"""Text chunking utilities."""

from typing import Iterable, List, Tuple, Optional

from .config import get_settings


def chunk_text(
    text: str,
    *,
    chunk_size: int,
    overlap: int = 0,
    file_path: Optional[str] = None
) -> List[Tuple[str, int]]:
    """
    Split text into chunks with line number tracking.

    Automatically selects chunking strategy based on file type and configuration:
    - For .org files with org strategy: uses org heading chunking
    - Otherwise: uses fixed-size character chunking

    Args:
        text: Input text to chunk
        chunk_size: Maximum characters per chunk
        overlap: Characters to overlap between chunks (for fixed-size)
        file_path: Optional file path to determine strategy

    Returns:
        List of (chunk_text, line_number) tuples
        line_number is 1-based starting line for chunk
    """
    settings = get_settings()

    if file_path and file_path.endswith('.org') and settings.chunk_strategy == 'org':
        try:
            from .org_chunking import chunk_org_recursively

            return chunk_org_recursively(
                text,
                chunk_size=chunk_size,
                overlap=overlap,
                min_chunk_size=settings.min_chunk_size,
                include_heading=settings.org_chunk_include_heading,
                oversize_strategy=settings.org_chunk_oversize_strategy
            )
        except ImportError:
            pass

    return chunk_text_fixed(text, chunk_size=chunk_size, overlap=overlap)


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


def batched(iterable: Iterable[str], batch_size: int) -> Iterable[List[str]]:
    """
    Yield batches from iterable.

    Args:
        iterable: Input iterable
        batch_size: Number of items per batch

    Yields:
        Lists of items with size up to batch_size
    """
    batch = []
    for item in iterable:
        batch.append(item)
        if len(batch) >= batch_size:
            yield batch
            batch = []
    if batch:
        yield batch
