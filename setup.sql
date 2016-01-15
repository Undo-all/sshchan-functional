PRAGMA foreign_keys = ON;

CREATE TABLE boards (
    board_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    board_name VARCHAR(255),
    board_description VARCHAR(255)
);

CREATE TABLE posts (
    post_id INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    post_subject VARCHAR(255),
    post_by VARCHAR(255),
    post_content TEXT NOT NULL,
    post_board INT(8) NOT NULL,
    post_reply INT(8),
    FOREIGN KEY(post_board) REFERENCES boards(board_id) ON DELETE CASCADE,
    FOREIGN KEY(post_reply) REFERENCES posts(post_id) ON DELETE CASCADE
);

