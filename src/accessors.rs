#![allow(unused, unused_variables)]

use crate::gen_accessors_attr::*;
use ::std::borrow::Borrow;
use ::std::fmt::{Debug, Display};
use ::std::fmt;
use ::std::str::FromStr;
use ::proc_macro2::{Span, TokenStream};
use ::quote::{quote, format_ident, ToTokens};
use ::syn::{Attribute, NestedMeta, AttributeArgs, parse2, Receiver, parse_macro_input, bracketed, braced, Token, Expr, Member, Visibility, ExprType, Ident};
use ::syn::parse::{Parse, ParseStream, Parser};
use ::syn::punctuated::Punctuated;
use ::syn::spanned::Spanned;
use ::syn::token::{Brace, Bracket};

#[derive(Debug)]
pub struct AccessorsExpr {
  pub attrs: Vec<GenAccessorsAttr>,
  pub expr: ExprType,
}

impl Parse for AccessorsExpr {
  fn parse(input: ParseStream) -> Result<Self, syn::Error>  {
    let mut attrs = Vec::new();
    while input.peek(Token![#]) {
      attrs.push(input.parse()?);
    }
    let expr = input.parse()?;

    Ok(AccessorsExpr { attrs, expr, })
  }
}

#[derive(Debug, Clone)]
pub enum AccessorIdent {
  Get { span: Span, },
  GetMut { span: Span, },
  GetCopy { span: Span, },
  Take { span: Span, },
  Set { span: Span, },
  ChainSet { span: Span, },
  Replace { span: Span, },
}

impl Display for AccessorIdent {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    return match self {
      Self::Get { .. }      => f.write_str("get"),
      Self::GetMut { .. }   => f.write_str("get_mut"),
      Self::GetCopy { .. }  => f.write_str("get_copy"),
      Self::Take { .. }     => f.write_str("take"),
      Self::Set { .. }      => f.write_str("set"),
      Self::ChainSet { .. } => f.write_str("chain_set"),
      Self::Replace { .. }  => f.write_str("replace"),
    }
  }
}

impl Parse for AccessorIdent {
  fn parse(input: ParseStream) -> Result<Self, syn::Error> {
    let ident = Ident::parse(input)?;
    return match ident.to_string().as_str() {
      "get"       => Ok(Self::Get { span: ident.span() }),
      "get_mut"   => Ok(Self::GetMut { span: ident.span() }),
      "get_copy"  => Ok(Self::GetCopy { span: ident.span() }),
      "take"      => Ok(Self::Take { span: ident.span() }),
      "set"       => Ok(Self::Set { span: ident.span() }),
      "chain_set" => Ok(Self::ChainSet { span: ident.span() }),
      "replace"   => Ok(Self::Replace { span: ident.span() }),
      _ => Err(syn::Error::new(input.span(), "unrecognized ident")),
    };
  }
}

#[derive(Debug)]
pub struct Accessor {
  pub vis: Visibility,
  pub constness: Option<Token![const]>,
  pub asyncness: Option<Token![async]>,
  pub unsafety: Option<Token![unsafe]>,
  pub ty: AccessorIdent,
}

impl Parse for Accessor {
  fn parse(input: ParseStream) -> Result<Self, syn::Error>  {
    Ok(Accessor { 
      vis: input.parse()?,
      constness: input.parse().ok(),
      asyncness: input.parse().ok(),
      unsafety: input.parse().ok(),
      ty: input.parse()?
    })
  }
}