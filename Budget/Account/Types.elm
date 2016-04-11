module Budget.Account.Types (Model, AccountType(PercentBase, Fixed, Percentage), Mode(Edit, NoEdit)) where


type AccountType =
      PercentBase
    | Fixed
    | Percentage

type Mode =
      Edit
    | NoEdit


type alias Model =
  { name: String
  , amount: Int
  , newName: String
  , newAmount: String
  , mode: Mode
  , error: String
  , baseAmount: Int
  , currentAmount: Int
  , factor: Int
  , accountType: AccountType
  }
