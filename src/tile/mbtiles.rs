use rusqlite::Connection;
use std::path::Path;

pub(crate) struct MBTiles {
    pub db: Connection,
}

impl MBTiles {
    pub(crate) fn new<P>(path: P) -> Result<MBTiles, ()>
    where
        P: AsRef<Path>,
    {
        match rusqlite::Connection::open(path) {
            Ok(db) => {
                Self::init_db(&db);
                return Ok(MBTiles { db });
            }

            _ => Err(()),
        }
    }

    pub(crate) fn new_in_memory() -> Result<MBTiles, ()> {
        match rusqlite::Connection::open_in_memory() {
            Ok(db) => {
                Self::init_db(&db);
                Ok(MBTiles { db })
            }
            _ => Err(()),
        }
    }

    pub(self) fn init_db(db: &Connection) {
        let query = "CREATE TABLE tiles (
                        zoom_level INTEGER,
                        tile_column INTEGER,
                        tile_row INTEGER,
                        tile_data BLOB
                    )";
        let _ = db.execute(query, ());
    }

    pub(crate) fn set(&self, z: u8, x: u32, y: u32, bytes: &Vec<u8>) {
        let query = "INSERT INTO tiles (
                        zoom_level,
                        tile_column,
                        tile_row,
                        tile_data
                    )
                    VALUES (?1, ?2, ?3, ?4)";

        self.db.execute(query, (z, x, y, bytes)).unwrap();
    }

    pub(crate) fn get(&self, z: u8, x: u32, y: u32) -> Option<Vec<u8>> {
        let query = "SELECT tile_data
                    FROM
                        tiles
                    WHERE
                        zoom_level = ?1 AND
                        tile_column = ?2 AND
                        tile_row = ?3";

        match self.db.query_row(query, (z, x, y), |row| row.get(0)) {
            Ok(data) => Some(data),
            _ => None,
        }
    }
}
