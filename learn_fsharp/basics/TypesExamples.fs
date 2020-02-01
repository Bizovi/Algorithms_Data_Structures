(*
    Some examples emphasizing the low overhead of types
    and the facilitation of DDD approaches to design

    Real systems would have:
        - constraints @creation_time != unconstrained strings
        - other methods
*)
open System

// Person name
type PersonalName = {FirstName: string; LastName: string}

// Stuff with addresses
type StreetAddress = {Line1: string; Line2: string; Line3:string}
type ZipCode = ZipCode of string
type StateAbbrev = StateAbbrev of string
type ZipAndState = {State: StateAbbrev; Zip:ZipCode} // composed

// Specializing by country
type USAddress = {Street: StreetAddress; Region: ZipAndState} // composed
type UKPostCode = PostCode of string
type UKAddress = {Street: StreetAddress; Region: UKPostCode}

// Internaational
type InternationalAddress = {
    Street:StreetAddress; Region: string; CountryName: string
}
type Address = USAddress | UKAddress | InternationalAddress // choice

// Email and Phone
type Email = Email of string
type CountryPrefix = Prefix of int
type Phone = {CountryPrefix: CountryPrefix; LocalNumber: string}


// Putting it all together in a contact
type Contact = {
    PersonalName: PersonalName;
    // option indicates it might be missing
    Address: Address option;
    Phone: Phone option;
    Email: Email option;
}

// Putting it all together in a CustomerAccount
type CustomerAccountId = AccountId of string
type CustomerType = Prospect | Active | Inactive

// override equality and deny comparison
// the only Entity here, special treatment of equality and comparison!!
// has a lifecycle, compared by "id", not attributes
[<CustomEquality; NoComparison>]
type CustomerAccount = 
    {
    CustomerAccountId: CustomerAccountId;
    CustomerType: CustomerType;
    ContactInfo: Contact;
    }

    override this.Equals(other) = 
        match other with
        | :? CustomerAccount as otherCust ->
            (this.CustomerAccountId = otherCust.CustomerAccountId)
        | _ -> false
    
    override this.GetHashCode() = hash this.CustomerAccountId