#![allow(unused, unused_variables)]

use ::std::borrow::Borrow;
use ::std::fmt::{Debug, Display};
use ::std::fmt;
use ::std::str::FromStr;
use ::proc_macro2::{Span, TokenStream};
use ::quote::{quote, format_ident, ToTokens};
use syn::parenthesized;
use syn::parse::ParseBuffer;
use syn::token::Paren;
use ::syn::{Attribute, NestedMeta, AttributeArgs, parse2, Receiver, parse_macro_input, bracketed, braced, Token, Expr, Member, Visibility, ExprType, Ident};
use ::syn::parse::{Parse, ParseStream, Parser};
use ::syn::punctuated::Punctuated;
use ::syn::spanned::Spanned;
use ::syn::token::{Brace, Bracket};

use crate::ForEachAccessorIdent;

#[derive(Debug, Clone)]
pub enum GenAccessorsAttrIdent {
  Name            { span: Span },
  Receiver        { span: Span },
  Attrs           { span: Span },
  GetSuffix       { span: Span },
  GetPostfix      { span: Span },
  GetMutSuffix    { span: Span },
  GetMutPostfix   { span: Span },
  GetCopySuffix   { span: Span },
  GetCopyPostfix  { span: Span },
  TakeSuffix      { span: Span },
  TakePostfix     { span: Span },
  SetSuffix       { span: Span },
  SetPostfix      { span: Span },
  ChainSetSuffix  { span: Span },
  ChainSetPostfix { span: Span },
  ReplaceSuffix   { span: Span },
  ReplacePostfix  { span: Span },
}

impl Display for GenAccessorsAttrIdent {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    return match self {
      Self::Name { .. }            => f.write_str("name"),
      Self::Receiver { .. }        => f.write_str("receiver"),
      Self::Attrs { .. }           => f.write_str("attrs"),
      Self::GetSuffix { .. }       => f.write_str("get_suffix"),
      Self::GetPostfix { .. }      => f.write_str("get_postfix"),
      Self::GetMutSuffix { .. }    => f.write_str("get_mut_suffix"),
      Self::GetMutPostfix { .. }   => f.write_str("get_mut_postfix"),
      Self::GetCopySuffix { .. }   => f.write_str("get_copy_suffix"),
      Self::GetCopyPostfix { .. }  => f.write_str("get_copy_postfix"),
      Self::TakeSuffix { .. }      => f.write_str("take_suffix"),
      Self::TakePostfix { .. }     => f.write_str("take_postfix"),
      Self::SetSuffix { .. }       => f.write_str("set_suffix"),
      Self::SetPostfix { .. }      => f.write_str("set_postfix"),
      Self::ChainSetSuffix { .. }  => f.write_str("chain_set_suffix"),
      Self::ChainSetPostfix { .. } => f.write_str("chain_set_postfix"),
      Self::ReplaceSuffix { .. }   => f.write_str("replace_suffix"),
      Self::ReplacePostfix { .. }  => f.write_str("replace_postfix"),
    }
  }
}

impl Parse for GenAccessorsAttrIdent {
  fn parse(input: ParseStream) -> Result<Self, syn::Error> {
    let ident = Ident::parse(input)?;
    return match ident.to_string().as_str() {
      "name"              => Ok(Self::Name { span: ident.span() }),
      "receiver"          => Ok(Self::Receiver { span: ident.span() }),
      "attrs"             => Ok(Self::Attrs { span: ident.span() }),
      "get_suffix"        => Ok(Self::GetSuffix { span: ident.span() }),
      "get_postfix"       => Ok(Self::GetPostfix { span: ident.span() }),
      "get_mut_suffix"    => Ok(Self::GetMutSuffix { span: ident.span() }),
      "get_mut_postfix"   => Ok(Self::GetMutPostfix { span: ident.span() }),
      "get_copy_suffix"   => Ok(Self::GetCopySuffix { span: ident.span() }),
      "get_copy_postfix"  => Ok(Self::GetCopyPostfix { span: ident.span() }),
      "take_suffix"       => Ok(Self::TakeSuffix { span: ident.span() }),
      "take_postfix"      => Ok(Self::TakePostfix { span: ident.span() }),
      "set_suffix"        => Ok(Self::SetSuffix { span: ident.span() }),
      "set_postfix"       => Ok(Self::SetPostfix { span: ident.span() }),
      "chain_set_suffix"  => Ok(Self::ChainSetSuffix { span: ident.span() }),
      "chain_set_postfix" => Ok(Self::ChainSetPostfix { span: ident.span() }),
      "replace_suffix"    => Ok(Self::ReplaceSuffix { span: ident.span() }),
      "replace_postfix"   => Ok(Self::ReplacePostfix { span: ident.span() }),
      _ => Err(syn::Error::new(input.span(), "unrecognized ident")),
    };
  }
}

#[derive(Debug, Clone)]
pub struct GenAccessorsAttr {
  pub pound_token: Token![#], 
  pub bracket_token: Bracket,
  pub ident: GenAccessorsAttrIdent, 
  pub paren_token: Paren, 
  pub arg: TokenStream,
}

impl Parse for GenAccessorsAttr {
  fn parse(input: ParseStream) -> Result<Self, syn::Error>  {
    let brackets_content: ParseBuffer;
    let paren_content: ParseBuffer;
    let pound_token = input.parse()?;
    let bracket_token = bracketed!(brackets_content in input);
    let ident = brackets_content.parse::<GenAccessorsAttrIdent>()?;
    let paren_token = parenthesized!(paren_content in brackets_content);
    let arg = paren_content.parse::<TokenStream>()?;

    Ok(GenAccessorsAttr { 
      pound_token,
      bracket_token,
      ident,
      paren_token,
      arg,
    })
  }
}

impl GenAccessorsAttr {
  pub(crate) fn unpack_into(&self, 
    name: &mut Option<TokenStream>, receiver: &mut Option<TokenStream>, attrs: &mut TokenStream, 
    suffixes: &mut ForEachAccessorIdent<TokenStream>, 
    postfixes: &mut ForEachAccessorIdent<TokenStream>)
  {
    match &self.ident {
      GenAccessorsAttrIdent::Name            { .. } => *name = Some(self.arg.clone()),
      GenAccessorsAttrIdent::Receiver        { .. } => *receiver = Some(self.arg.clone()),
      GenAccessorsAttrIdent::Attrs           { .. } => *attrs = self.arg.clone(),
      GenAccessorsAttrIdent::GetSuffix       { .. } => suffixes.get = self.arg.clone(),
      GenAccessorsAttrIdent::GetPostfix      { .. } => postfixes.get = self.arg.clone(),
      GenAccessorsAttrIdent::GetMutSuffix    { .. } => suffixes.get_mut = self.arg.clone(),
      GenAccessorsAttrIdent::GetMutPostfix   { .. } => postfixes.get_mut = self.arg.clone(),
      GenAccessorsAttrIdent::GetCopySuffix   { .. } => suffixes.get_copy = self.arg.clone(),
      GenAccessorsAttrIdent::GetCopyPostfix  { .. } => postfixes.get_copy = self.arg.clone(),
      GenAccessorsAttrIdent::TakeSuffix      { .. } => suffixes.take = self.arg.clone(),
      GenAccessorsAttrIdent::TakePostfix     { .. } => postfixes.take = self.arg.clone(),
      GenAccessorsAttrIdent::SetSuffix       { .. } => suffixes.set = self.arg.clone(),
      GenAccessorsAttrIdent::SetPostfix      { .. } => postfixes.set = self.arg.clone(),
      GenAccessorsAttrIdent::ChainSetSuffix  { .. } => suffixes.chain_set = self.arg.clone(),
      GenAccessorsAttrIdent::ChainSetPostfix { .. } => postfixes.chain_set = self.arg.clone(),
      GenAccessorsAttrIdent::ReplaceSuffix   { .. } => suffixes.replace = self.arg.clone(),
      GenAccessorsAttrIdent::ReplacePostfix  { .. } => postfixes.replace = self.arg.clone(),
    }
  }
}