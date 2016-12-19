import Control.Exception (catch, SomeException(SomeException))
import Data.Typeable (typeOf)

main = error "hi" `catch` \(SomeException e) -> print (typeOf e)
