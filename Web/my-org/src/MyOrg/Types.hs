-- | Compatibility imports. Internal domain modules use their specific dependencies.
module MyOrg.Types
  ( module MyOrg.Domain.Identity, module MyOrg.Domain.Organization, module MyOrg.Domain.Goal.Types, module MyOrg.Domain.Authority, module MyOrg.Domain.Result, module MyOrg.Domain.Review.Types, module MyOrg.Domain.Error, describeError ) where

import MyOrg.Domain.Identity
import MyOrg.Domain.Organization
import MyOrg.Domain.Goal.Types
import MyOrg.Domain.Authority
import MyOrg.Domain.Result
import MyOrg.Domain.Review.Types
import MyOrg.Domain.Error
import MyOrg.Presentation.Error (describeError)
