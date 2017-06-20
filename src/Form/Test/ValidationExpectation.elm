module Form.Test.ValidationExpectation exposing (ValidationExpectation(..))

{-| Test expectations for use with functions from `Form.Test`.

@docs ValidationExpectation

-}

import Form.Error


{-| Use with `Form.Test.describeValidation` or `Form.Test.testValidation` to express the result you expect.
-}
type ValidationExpectation e a
    = Valid
    | ValidDecodesTo a
    | Invalid (Form.Error.ErrorValue e)
    | InvalidCustomError e
