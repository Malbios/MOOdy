module Sidecar.Tests.WarningDiagnosticsTests

open Xunit
open Sidecar.IdeActions

[<Fact>]
let ``isWarningDiagnostic recognizes ToastStunt's Warning marker`` () =
    Assert.True(isWarningDiagnostic "Line 5:  Warning: Assignment used as a condition; did you mean `=='?")

[<Fact>]
let ``isWarningDiagnostic rejects a real compile error`` () =
    Assert.False(isWarningDiagnostic "Line 3:  Unexpected end of program.")

[<Fact>]
let ``isWarningDiagnostic rejects a diagnostic with no Line prefix`` () =
    Assert.False(isWarningDiagnostic "verb not found")

[<Fact>]
let ``hasRealError is false for an empty list`` () = Assert.False(hasRealError [])

[<Fact>]
let ``hasRealError is false when every diagnostic is a warning`` () =
    Assert.False(
        hasRealError
            [ "Line 1:  Warning: Assignment used as a condition; did you mean `=='?"
              "Line 4:  Warning: Bare `ANY' in inline catch swallows all errors, including bugs; consider listing specific error codes" ]
    )

[<Fact>]
let ``hasRealError is true when a real error is mixed in with warnings`` () =
    Assert.True(hasRealError [ "Line 1:  Warning: Assignment used as a condition; did you mean `=='?"; "Line 2:  Unexpected end of program." ])
