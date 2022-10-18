use core::fmt::Debug;
use delegate::delegate;
use strum_macros::EnumDiscriminants;
use proc_macro2::{Span, Ident, TokenStream};
use syn::token::{Bracket, Paren};
use syn::parse::{Parse, ParseStream, ParseBuffer};
use syn::{Token, bracketed, parenthesized};

macro_rules! impl_attr {
  ($(($enum_var_ident:ident($struct_ident:ident), $ident_str:literal)),* $(,)*) => {
    pub trait IsAttr: Debug + Clone + Parse {
      fn pound_token(&self) -> Token![#];
      fn bracket_token(&self) -> Bracket;
      fn ident(&self) -> Ident;
      fn paren_content(&self) -> Paren;
      fn arg(&self) -> &TokenStream;
    }

    $(
    #[derive(Debug, Clone)]
    pub struct $struct_ident { 
      pound_token: Token![#], 
      bracket_token: Bracket,
      ident_span: Span,
      paren_token: Paren,
      arg: TokenStream, 
    }
    
    impl Parse for $struct_ident {
      fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let brackets_content: ParseBuffer;
        let paren_content: ParseBuffer;
        let pound_token = input.parse()?;
        let bracket_token = bracketed!(brackets_content in input);
        let ident_span = {
          let ident = brackets_content.parse::<Ident>()?;
          assert!(ident == $ident_str);
          ident.span()
        };
        let paren_token = parenthesized!(paren_content in brackets_content);
        let arg = paren_content.parse::<TokenStream>()?;

        Ok(Self { 
          pound_token, bracket_token, ident_span, paren_token, arg,
        })
      }
    }

    impl IsAttr for $struct_ident {
      fn pound_token(&self) -> Token![#] { self.pound_token.clone() }
      fn bracket_token(&self) -> Bracket { self.bracket_token.clone() }
      fn ident(&self) -> Ident           { Ident::new($ident_str, self.ident_span.clone()) }
      fn paren_content(&self) -> Paren   { self.paren_token.clone() }
      fn arg(&self) -> &TokenStream       { &self.arg }
    }
    )*
    
    #[derive(Debug, Clone, EnumDiscriminants)]
    pub enum Attr {
      $( 
      $enum_var_ident($struct_ident),
      )*
    }

    impl Parse for Attr {
      fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let brackets_content: ParseBuffer;
        let paren_content: ParseBuffer;

        let pound_token = input.parse()?;
        let bracket_token = bracketed!(brackets_content in input);
        let ident = brackets_content.parse::<Ident>()?;
        let ident_span = ident.span();
        let paren_token = parenthesized!(paren_content in brackets_content);
        let arg = paren_content.parse::<TokenStream>()?;

        return Ok( 
          match ident.to_string().as_str() {
            $(
            $ident_str => Self::$enum_var_ident(
              $struct_ident { pound_token, bracket_token, ident_span, paren_token, arg } ),
            )*
            _ => panic!(),
          }
        );
      }
    }

    impl IsAttr for Attr {
      delegate! {
        to match self {
          $(
          Self::$enum_var_ident(attr) => attr,
          )*
        } {
          fn pound_token(&self) -> Token![#];
          fn bracket_token(&self) -> Bracket;
          fn ident(&self) -> Ident;
          fn paren_content(&self) -> Paren;
          fn arg(&self) -> &TokenStream;
        }
      }
    }
  };
}

impl_attr! {
  (Name(AttrName), "name"),
  (Receiver(AttrReceiver), "receiver"),
  (Attrs(AttrAttrs), "attrs"),
  (GetSuf(AttrGetSuf), "get_suffix"),
  (GetPost(AttrGetPost), "get_postfix"),
  (GetMutSuf(AttrGetMutSuf), "get_mut_suffix"),
  (GetMutPost(AttrGetMutPost), "get_mut_postfix"),
  (GetCopySuf(AttrGetCopySuf), "get_copy_suffix"),
  (GetCopyPost(AttrGetCopyPost), "get_copy_postfix"),
  (TakeSuf(AttrTakeSuf), "take_suffix"),
  (TakePost(AttrTakePost), "take_postfix"),
  (SetSuf(AttrSetSuf), "set_suffix"),
  (SetPost(AttrSetPost), "set_postfix"),
  (ChainSetSuf(AttrChainSetSuf), "chain_set_suffix"),
  (ChainSetPost(AttrChainSetPost), "chain_set_postfix"),
  (ReplaceSuf(AttrReplaceSuf), "replace_suffix"),
  (ReplacePost(AttrReplacePost), "replace_postfix"),
}
