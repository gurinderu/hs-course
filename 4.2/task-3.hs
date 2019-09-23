data Result' = Success'
             | Fail' Int

instance Show Result' where
        show Success' = "Success"
        show (Fail' n) = "Fail: " ++ show n

doSomeWork' :: SomeData -> Result'
doSomeWork' x
  = case doSomeWork x of
        (Success, _) -> Success'
        (_, n) -> Fail' n
