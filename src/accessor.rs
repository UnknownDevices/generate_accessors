use core::fmt::Debug;
use delegate::delegate;
use proc_macro2::{Span, Ident};
use syn::parse::{Parse, ParseStream};
use syn::{Visibility, Token};

macro_rules! impl_attr {
  ($(($enum_var_ident:ident($struct_ident:ident), $ident_str:literal)),* $(,)*) => {
    pub trait IsAccessor: Debug + Clone + Parse {
      fn vis(&self) -> Visibility;
      fn constness(&self) -> Option<Token![const]>;
      fn asyncness(&self) -> Option<Token![async]>;
      fn unsafety(&self) -> Option<Token![unsafe]>;
      fn ident(&self) -> Ident;
    }

    $(
    #[derive(Debug, Clone)]
    pub struct $struct_ident { 
      vis: Visibility,
      constness: Option<Token![const]>,
      asyncness: Option<Token![async]>,
      unsafety: Option<Token![unsafe]>,
      ident_span: Span,
    }
    
    impl Parse for $struct_ident {
      fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let vis = input.parse()?;
        let constness = input.parse().ok();
        let asyncness = input.parse().ok();
        let unsafety = input.parse().ok();
        let ident_span = {
          let ident = input.parse::<Ident>()?;
          assert!(ident == $ident_str);
          ident.span()
        };

        Ok(Self { vis, constness, asyncness, unsafety, ident_span })
      }
    }

    impl IsAccessor for $struct_ident {
      fn vis(&self) -> Visibility                  { self.vis.clone() }
      fn constness(&self) -> Option<Token![const]> { self.constness.clone() }
      fn asyncness(&self) -> Option<Token![async]> { self.asyncness.clone() }
      fn unsafety(&self) -> Option<Token![unsafe]> { self.unsafety.clone() }
      fn ident(&self) -> Ident { Ident::new($ident_str, self.ident_span.clone()) }
    }
    )*
    
    #[derive(Debug, Clone)]
    pub enum Accessor {
      $( 
      $enum_var_ident($struct_ident),
      )*
    } 

    impl Parse for Accessor {
      fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let vis = input.parse()?;
        let constness = input.parse().ok();
        let asyncness = input.parse().ok();
        let unsafety = input.parse().ok();
        let ident = input.parse::<Ident>()?;
        let ident_span = ident.span();

        return Ok( 
          match ident.to_string().as_str() {
            $(
            $ident_str => Self::$enum_var_ident(
              $struct_ident { vis, constness, asyncness, unsafety, ident_span } ),
            )*
            _ => panic!(),
          }
        );
      }
    }

    impl IsAccessor for Accessor {
      delegate! {
        to match self {
          $(
          Self::$enum_var_ident(attr) => attr,
          )*
        } {
          fn vis(&self) -> Visibility;
          fn constness(&self) -> Option<Token![const]>;
          fn asyncness(&self) -> Option<Token![async]>;
          fn unsafety(&self) -> Option<Token![unsafe]>;
          fn ident(&self) -> Ident;
        }
      }
    }
  };
}

impl_attr! {
  (Get(AccessorGet), "get"),
  (GetMut(AccessorGetMut), "get_mut"),
  (GetCopy(AccessorGetCopy), "get_copy"),
  (Take(AccessorTake), "take"),
  (Set(AccessorSet), "set"),
  (ChainSet(AccessorChainSet), "chain_set"),
  (Replace(AccessorReplace), "replace"),
}
