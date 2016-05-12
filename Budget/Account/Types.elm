module Budget.Account.Types exposing (Model, AccountType(PercentBase, Fixed, Percentage), Mode(Edit, NoEdit))


type AccountType
  = PercentBase
  | Fixed
  | Percentage


type Mode
  = Edit
  | NoEdit


type alias Model =
  { name : String
  , amount : Int
  , newName : String
  , newFactor : String
  , mode : Mode
  , error : String
  , baseAmount : Int
  , currentAmount : Int
  , factor : Int
  , accountType : AccountType
  }
