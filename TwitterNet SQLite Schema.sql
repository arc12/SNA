CREATE TABLE "relation" (
    "follower_id" INTEGER NOT NULL,
    "followed_id" INTEGER NOT NULL
);
CREATE INDEX "FOLLOWER_ID_IDX" on relation (follower_id ASC);
CREATE INDEX "FOLLOWED_ID_IDX" on relation (followed_id ASC);
CREATE TABLE user (
    "id" TEXT NOT NULL,
    "name" TEXT NOT NULL,
    "status_count" INTEGER NOT NULL DEFAULT (0),
    "location" TEXT,
    "created" TEXT,
    "cache_date" TEXT,
    "screen_name" TEXT NOT NULL,
    "description" TEXT
);
CREATE UNIQUE INDEX "ID_IDX" on user (id ASC);
