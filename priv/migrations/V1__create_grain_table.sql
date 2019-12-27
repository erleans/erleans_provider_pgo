CREATE TABLE IF NOT EXISTS erleans_grains (
    grain_id BYTEA NOT NULL,
    grain_type CHARACTER VARYING(2048),
    grain_ref_hash BIGINT NOT NULL,
    grain_etag BIGINT NOT NULL,
    grain_state BYTEA NOT NULL,
    change_time TIMESTAMP NOT NULL,
    PRIMARY KEY (grain_id, grain_type));

CREATE INDEX IF NOT EXISTS
    erleans_grains_term_idx
    ON erleans_grains
    USING HASH (grain_ref_hash);
