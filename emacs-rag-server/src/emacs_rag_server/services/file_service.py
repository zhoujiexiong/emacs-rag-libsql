"""File indexing service."""

import hashlib
from pathlib import Path
from typing import Dict, Optional, Tuple

from ..models.database import add_documents, add_org_headings, delete_documents_for_path, get_file_metadata, update_file_metadata, get_client
from ..models.embeddings import get_embedding_model
from ..utils.chunking import batched, chunk_text
from ..utils.config import get_settings


def calculate_content_hash(content: str) -> str:
    """Calculate SHA-256 hash of content."""
    return hashlib.sha256(content.encode('utf-8')).hexdigest()


def index_file(
    path: str,
    *,
    content: Optional[str] = None,
    metadata: Optional[Dict] = None
) -> Tuple[str, int]:
    """
    Index a file and return (resolved_path, chunk_count).

    Steps:
    1. Normalize path (expand user, resolve)
    2. Get content (from parameter or read file)
    3. Calculate content hash
    4. Check if file content has changed
    5. If changed: Chunk text, generate embeddings, store in database
    6. If not changed: Return existing chunk count without reindexing
    7. Return path and chunk count

    Args:
        path: Absolute or relative file path
        content: Optional content override (if None, read from file)
        metadata: Optional custom metadata to attach

    Returns:
        Tuple of (resolved_path, chunks_indexed)

    Raises:
        FileNotFoundError: If file doesn't exist and content not provided
        PermissionError: If file cannot be read
    """
    settings = get_settings()
    embedding_model = get_embedding_model()

    # Normalize path
    file_path = Path(path).expanduser().resolve()
    resolved_path = str(file_path)

    # Get content
    if content is None:
        if not file_path.exists():
            raise FileNotFoundError(f"File not found: {resolved_path}")
        if not file_path.is_file():
            raise ValueError(f"Not a file: {resolved_path}")
        content = file_path.read_text(encoding='utf-8')

    # Calculate content hash
    content_hash = calculate_content_hash(content)

    # Check if file already exists with same content
    existing_metadata = get_file_metadata(resolved_path)
    
    # Check if content has changed
    if existing_metadata and existing_metadata['content_hash'] == content_hash:
        # Content hasn't changed, get existing chunk count from database
        client = get_client()
        result = client.execute(
            "SELECT COUNT(*) FROM documents WHERE source_path = ?",
            [resolved_path]
        )
        existing_chunk_count = result.rows[0][0]
        return (resolved_path, existing_chunk_count)

    # Chunk text
    chunks = chunk_text(
        content,
        chunk_size=settings.chunk_size,
        overlap=settings.chunk_overlap,
        file_path=resolved_path
    )

    if not chunks:
        # Empty file, delete any existing chunks
        delete_documents_for_path(resolved_path)
        # Update metadata for empty file
        update_file_metadata(resolved_path, content_hash)
        return (resolved_path, 0)

    # Delete existing chunks for this file
    delete_documents_for_path(resolved_path)

    # Prepare data for indexing
    total_chunks = len(chunks)
    all_ids = []
    all_documents = []
    all_metadatas = []
    all_embeddings = []

    # Extract chunk texts for batch embedding
    chunk_texts = [text for text, _ in chunks]

    # Generate embeddings in batches
    batch_size = 8
    for batch_idx, text_batch in enumerate(batched(chunk_texts, batch_size)):
        batch_embeddings = embedding_model.embed_documents(text_batch)

        # Calculate which chunks are in this batch
        start_idx = batch_idx * batch_size
        end_idx = min(start_idx + len(text_batch), total_chunks)

        # Prepare batch data
        for i, (text, embedding) in enumerate(zip(text_batch, batch_embeddings)):
            chunk_idx = start_idx + i
            _, line_number = chunks[chunk_idx]

            # Create chunk ID
            chunk_id = f"{resolved_path}:{chunk_idx}"

            # Create chunk metadata
            chunk_metadata = {
                'source_path': resolved_path,
                'chunk_index': chunk_idx,
                'line_number': line_number,
                'chunk_size': len(text),
                'chunk_total': total_chunks
            }

            # Add custom metadata if provided
            if metadata:
                chunk_metadata.update(metadata)

            all_ids.append(chunk_id)
            all_documents.append(text)
            all_metadatas.append(chunk_metadata)
            all_embeddings.append(embedding)

    # Store in database
    add_documents(
        ids=all_ids,
        documents=all_documents,
        metadatas=all_metadatas,
        embeddings=all_embeddings
    )

    # Extract and store org headings if this is an org file
    add_org_headings(resolved_path, content)

    # Update file metadata
    update_file_metadata(resolved_path, content_hash)

    return (resolved_path, total_chunks)


def delete_file(path: str) -> Tuple[str, bool]:
    """
    Remove all chunks for a file.

    Args:
        path: Absolute or relative file path

    Returns:
        Tuple of (resolved_path, success)
    """
    # Normalize path
    file_path = Path(path).expanduser().resolve()
    resolved_path = str(file_path)

    # Delete from database
    delete_documents_for_path(resolved_path)

    return (resolved_path, True)
