CREATE TABLE IF NOT EXISTS todos (
    id UUID PRIMARY KEY,
    title VARCHAR(200) NOT NULL,
    description TEXT NULL,
    status VARCHAR(20) NOT NULL,
    created_at TIMESTAMPTZ NOT NULL,
    completed_at TIMESTAMPTZ NULL,
    CONSTRAINT todos_title_not_blank CHECK (length(btrim(title)) > 0),
    CONSTRAINT todos_status_check CHECK (status IN ('ACTIVE', 'COMPLETED')),
    CONSTRAINT todos_completion_consistency CHECK (
        (status = 'ACTIVE' AND completed_at IS NULL)
        OR (status = 'COMPLETED' AND completed_at IS NOT NULL)
    )
);

CREATE INDEX IF NOT EXISTS todos_created_at_idx ON todos (created_at ASC, id ASC);
