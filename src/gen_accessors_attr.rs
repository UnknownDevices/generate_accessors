use core::fmt::Debug;
use delegate::delegate;
use strum_macros::EnumDiscriminants;
use proc_macro2::{Span, Ident, TokenStream as TokenStream2};
use syn::token::{Bracket, Paren};
use syn::parse::{Parse, ParseStream, ParseBuffer};
use syn::{Token, bracketed, parenthesized};
use crate::GenAccessorsItem;
use crate::accessor::Accessor;

macro_rules! impl_gen_accessors_attr {
  ($(($enum_var_ident:ident($struct_ident:ident), $ident_str:literal)),* $(,)*) => {
    pub trait IsGenAccessorsAttr: Debug + Clone + Parse {
      fn pound_token(&self) -> Token![#];
      fn bracket_token(&self) -> Bracket;
      fn ident(&self) -> Ident;
      fn paren_content(&self) -> Paren;
      fn arg(&self) -> &TokenStream2;
    }

    $(
    #[derive(Debug, Clone)]
    pub struct $struct_ident {
      pound_token: Token![#],
      bracket_token: Bracket,
      ident_span: Span,
      paren_token: Paren,
      arg: TokenStream2,
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
        let arg = paren_content.parse::<TokenStream2>()?;

        Ok(Self {
          pound_token, bracket_token, ident_span, paren_token, arg,
        })
      }
    }

    impl IsGenAccessorsAttr for $struct_ident {
      fn pound_token(&self) -> Token![#] { self.pound_token.clone() }
      fn bracket_token(&self) -> Bracket { self.bracket_token.clone() }
      fn ident(&self) -> Ident           { Ident::new($ident_str, self.ident_span.clone()) }
      fn paren_content(&self) -> Paren   { self.paren_token.clone() }
      fn arg(&self) -> &TokenStream2       { &self.arg }
    }
    )*

    #[derive(Debug, Clone, EnumDiscriminants)]
    pub enum GenAccessorsAttr {
      $(
      $enum_var_ident($struct_ident),
      )*
    }

    impl Parse for GenAccessorsAttr {
      fn parse(input: ParseStream) -> Result<Self, syn::Error> {
        let brackets_content: ParseBuffer;
        let paren_content: ParseBuffer;

        let pound_token = input.parse()?;
        let bracket_token = bracketed!(brackets_content in input);
        let ident = brackets_content.parse::<Ident>()?;
        let ident_span = ident.span();
        let paren_token = parenthesized!(paren_content in brackets_content);
        let arg = paren_content.parse::<TokenStream2>()?;

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

    impl IsGenAccessorsAttr for GenAccessorsAttr {
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
          fn arg(&self) -> &TokenStream2;
        }
      }
    }
  };
}

impl_gen_accessors_attr! {
  (Name(GenAccessorsAttrName), "name"),
  (Receiver(GenAccessorsAttrReceiver), "receiver"),
  (Attrs(GenAccessorsAttrAttrs), "attrs"),
  (GetSuf(GenAccessorsAttrGetSuf), "get_suffix"),
  (GetPost(GenAccessorsAttrGetPost), "get_postfix"),
  (GetMutSuf(GenAccessorsAttrGetMutSuf), "get_mut_suffix"),
  (GetMutPost(GenAccessorsAttrGetMutPost), "get_mut_postfix"),
  (GetCopySuf(GenAccessorsAttrGetCopySuf), "get_copy_suffix"),
  (GetCopyPost(GenAccessorsAttrGetCopyPost), "get_copy_postfix"),
  (TakeSuf(GenAccessorsAttrTakeSuf), "take_suffix"),
  (TakePost(GenAccessorsAttrTakePost), "take_postfix"),
  (SetSuf(GenAccessorsAttrSetSuf), "set_suffix"),
  (SetPost(GenAccessorsAttrSetPost), "set_postfix"),
  (ChainSetSuf(GenAccessorsAttrChainSetSuf), "chain_set_suffix"),
  (ChainSetPost(GenAccessorsAttrChainSetPost), "chain_set_postfix"),
  (ReplaceSuf(GenAccessorsAttrReplaceSuf), "replace_suffix"),
  (ReplacePost(GenAccessorsAttrReplacePost), "replace_postfix"),
}

#[derive(Debug, Clone)]
pub struct ProcGenAccessorsAttrs<'a>(
  // TODO: use tuples not arrays?
  Vec<[Option<&'a TokenStream2>; 3]>,
  Vec<[String; 2]>,
);

impl<'a> ProcGenAccessorsAttrs<'a> {
  pub fn name(&self, expr_index: usize) -> &Option<&'a TokenStream2> {
    &self.0[expr_index][0]
  }
  pub fn receiver(&self, expr_index: usize) -> &Option<&'a TokenStream2> {
    &self.0[expr_index][1]
  }
  pub fn attrs(&self, expr_index: usize) -> &Option<&'a TokenStream2> {
    &self.0[expr_index][2]
  }
  pub fn suffix(&self, acessor_index: usize, expr_index: usize) -> &String {
    &self.1[self.1.len() / self.0.len() * acessor_index + expr_index][0]
  }
  pub fn postfix(&self, expr_index: usize, accessor_index: usize) -> &String {
    &self.1[self.1.len() / self.0.len() * expr_index + accessor_index][1]
  }

  fn proc_attr(attr: &'a GenAccessorsAttr, name: &mut Option<&'a TokenStream2>,
    receiver: &mut Option<&'a TokenStream2>, attrs: &mut Option<&'a TokenStream2>,
    fixes: &mut Vec<[String; 2]>, item: &GenAccessorsItem,
    disc_of_fix_attrs_to_get: &Vec<[GenAccessorsAttrDiscriminants; 2]>)
  {
    match attr {
      GenAccessorsAttr::Name(attr_name) => *name = Some(attr_name.arg()),
      GenAccessorsAttr::Receiver(attr_receiver) => *receiver = Some(attr_receiver.arg()),
      GenAccessorsAttr::Attrs(attr_attrs) => *attrs = Some(attr_attrs.arg()),
      _ => {
        let attr_disc = GenAccessorsAttrDiscriminants::from(attr);
        for accessor_index in 0..item.accessors.len() {
          if attr_disc == disc_of_fix_attrs_to_get[accessor_index][0] {
            fixes[accessor_index][0] = attr.arg().to_string();
          } else if attr_disc == disc_of_fix_attrs_to_get[accessor_index][1] {
            fixes[accessor_index][1] = attr.arg().to_string();
          }
        }
      },
    }
  }

  pub(crate) fn new(item: &'a GenAccessorsItem) -> Self {
    let mut output = Self {0: Vec::new(), 1: Vec::new() };
    let mut name = Option::<&'a TokenStream2>::None;
    let mut receiver = Option::<&'a TokenStream2>::None;
    let mut attrs = Option::<&'a TokenStream2>::None;
    let mut fixes = Vec::<[String; 2]>::new();
    let mut disc_of_fix_attrs_to_get = Vec::<[GenAccessorsAttrDiscriminants; 2]>::new();

    output.0.reserve_exact(item.exprs.len());
    output.1.reserve_exact(item.exprs.len() * item.accessors.len());
    fixes.reserve_exact(item.accessors.len());
    disc_of_fix_attrs_to_get.reserve_exact(item.accessors.len());

    for accessor_index in 0..item.accessors.len() {
      let (fixes_elem, disc_of_fix_attrs_to_get_elem) =
      match &item.accessors[accessor_index] {
        Accessor::Get(_) =>
          ([String::new(), String::new()],
          [GenAccessorsAttrDiscriminants::GetSuf, GenAccessorsAttrDiscriminants::GetPost]),
        Accessor::GetMut(_) =>
          ([String::new(), "_mut".to_string()],
          [GenAccessorsAttrDiscriminants::GetMutSuf, GenAccessorsAttrDiscriminants::GetMutPost]),
        Accessor::GetCopy(_) =>
          ([String::new(), String::new()],
          [GenAccessorsAttrDiscriminants::GetCopySuf, GenAccessorsAttrDiscriminants::GetCopyPost]),
        Accessor::Take(_) =>
          (["take_".to_string(), String::new()],
          [GenAccessorsAttrDiscriminants::TakeSuf, GenAccessorsAttrDiscriminants::TakePost]),
        Accessor::Set(_) =>
          (["set_".to_string(), String::new()],
            [GenAccessorsAttrDiscriminants::SetSuf, GenAccessorsAttrDiscriminants::SetPost]),
        Accessor::ChainSet(_) =>
          (["set_".to_string(), String::new()],
          [GenAccessorsAttrDiscriminants::ChainSetSuf,
          GenAccessorsAttrDiscriminants::ChainSetPost]),
        Accessor::Replace(_) =>
          (["replace_".to_string(), String::new()],
          [GenAccessorsAttrDiscriminants::ReplaceSuf, GenAccessorsAttrDiscriminants::ReplacePost]),
      };

      fixes.push(fixes_elem);
      disc_of_fix_attrs_to_get.push(disc_of_fix_attrs_to_get_elem);
    }

    for attr in &item.attrs {
      Self::proc_attr(
        attr, &mut name, &mut receiver, &mut attrs, &mut fixes, item, &disc_of_fix_attrs_to_get);
    }

    for expr in &item.exprs {
      let mut name = name.clone();
      let mut receiver = receiver.clone();
      let mut attrs = attrs.clone();
      let mut fixes = fixes.clone();
      for attr in &expr.attrs {
        Self::proc_attr(
          attr, &mut name, &mut receiver, &mut attrs, &mut fixes, item, &disc_of_fix_attrs_to_get);
      }

      output.0.push([name, receiver, attrs]);
      output.1.append(&mut fixes);
    }

    output
  }
}
