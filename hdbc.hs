import Data.Maybe (fromMaybe)
import Database.HDBC
import Database.HDBC.Sqlite3

main :: IO ()
main = do
    conn <- connectSqlite3 "test.db"
    _ <- run conn "CREATE TABLE person (id INTEGER NOT NULL, name VARCHAR(80))" []
    _ <- run conn "INSERT INTO person (id, name) VALUES (?, ?)" [toSql (123 :: Int), toSql "John Doe"]
    commit conn
    res <- quickQuery conn "SELECT * FROM person" []
    let stringRows = map convRow res
    mapM_ putStrLn stringRows
    disconnect conn
  where
    convRow :: [SqlValue] -> String
    convRow [sqlId, sqlDesc] = show intid ++ ": " ++ desc
      where
        intid = fromSql sqlId :: Integer
        desc = fromMaybe "NULL" (fromSql sqlDesc)
    convRow x = fail $ "Unexpected result: " ++ show x
