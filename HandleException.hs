import Control.Exception (SomeException (SomeException), catch)
import Data.Typeable (typeOf)

main = error "hi" `catch` \(SomeException e) -> print (typeOf e)
