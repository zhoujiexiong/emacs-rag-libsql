"""Configuration management for Emacs RAG server."""

import os
from dataclasses import dataclass
from functools import lru_cache
from pathlib import Path


@dataclass(frozen=True)
class RAGSettings:
    """Configuration settings for the RAG server."""

    # Database
    db_path: Path = Path(
        os.getenv("EMACS_RAG_DB_PATH", Path.home() / ".emacs-rag" / "libsql")
    )

    # Chunking
    chunk_strategy: str = os.getenv("EMACS_RAG_CHUNK_STRATEGY", "org")
    chunk_size: int = int(os.getenv("EMACS_RAG_CHUNK_SIZE", "800"))
    chunk_overlap: int = int(os.getenv("EMACS_RAG_CHUNK_OVERLAP", "100"))
    min_chunk_size: int = int(os.getenv("EMACS_RAG_MIN_CHUNK_SIZE", "10"))
    org_chunk_include_heading: bool = os.getenv(
        "EMACS_RAG_ORG_CHUNK_INCLUDE_HEADING", "true"
    ).lower() in ("true", "1", "yes")
    org_chunk_oversize_strategy: str = os.getenv(
        "EMACS_RAG_ORG_CHUNK_OVERSIZE_STRATEGY", "recursive"
    )

    # Embedding Model
    embedding_model: str = os.getenv("EMACS_RAG_EMBEDDING_MODEL", "moka-ai/m3e-base")
    vector_dimensions: int = 768  # Match m3e-base

    # Reranking
    rerank_model: str = os.getenv(
        "EMACS_RAG_RERANK_MODEL", "Alibaba-NLP/gte-multilingual-reranker-base"
        #"EMACS_RAG_RERANK_MODEL", "cross-encoder/ms-marco-MiniLM-L-6-v2"
    )
    rerank_enabled: bool = os.getenv("EMACS_RAG_RERANK_ENABLED", "true").lower() in (
        "true",
        "1",
        "yes",
    )
    rerank_top_k: int = int(os.getenv("EMACS_RAG_RERANK_TOP_K", "5"))

    # Server
    host: str = os.getenv("EMACS_RAG_HOST", "127.0.0.1")
    port: int = int(os.getenv("EMACS_RAG_PORT", "8765"))

    def ensure_paths(self) -> None:
        """Create database directory if needed."""
        self.db_path.mkdir(parents=True, exist_ok=True)


@lru_cache
def get_settings() -> RAGSettings:
    """Cached singleton settings instance."""
    settings = RAGSettings()
    settings.ensure_paths()
    return settings
