data Person = Person
    { name :: String
    , age :: Int
    , gender :: Gender
    }
    deriving (Show)

data Gender = Male | Female deriving (Show)

jan = Person{name = "Jan", age = 10, gender = Male}

{- --"accessors" are generated automatically:
name :: Person -> String
age :: Person -> Int
gender :: Person -> Gender
-}

-- record syntax can be used for pattern matching
f p = case p of
    Person{name = "Jan", age = a} -> "Jan is " ++ show a ++ " years old"
    Person{age = 10} -> "A person that's 10 years old"
    Person{gender = gen} -> "Gender of this person is  " ++ show gen

-- creates new record based on existing with attribute modified
setAttr = jan{age = 11}
