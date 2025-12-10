module GUI.Components
    ( UIComponents(..)
    , ContactForm(..)
    ) where

import Graphics.UI.Threepenny.Core (Element)

-- | Main UI components
data UIComponents = UIComponents
    { contactList :: Element
    , searchInput :: Element
    , addButton   :: Element
    , editButton  :: Element
    , deleteButton :: Element
    , contactForm :: ContactForm
    }

-- | Contact form components
data ContactForm = ContactForm
    { nameInput    :: Element
    , phoneInput   :: Element
    , emailInput   :: Element
    , addressInput :: Element
    , saveButton   :: Element
    , cancelButton :: Element
    }