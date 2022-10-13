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
      _ => Err(syn::Error::new(ident.span(), 
        format!("attribute `{ident}` not recognized in the context of this macro"))),
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

#[derive(Clone)]
pub struct ProcessedGenAccessorsAttrs<'a> {
  pub name: Option<&'a TokenStream>,
  pub receiver: Option<&'a TokenStream>,
  pub attrs: &'a TokenStream,
  pub get_suf: &'a TokenStream,
  pub get_post: &'a TokenStream,
  pub get_mut_suf: &'a TokenStream,
  pub get_mut_post: &'a TokenStream,
  pub get_copy_suf: &'a TokenStream,
  pub get_copy_post: &'a TokenStream,
  pub take_suf: &'a TokenStream,
  pub take_post: &'a TokenStream,
  pub set_suf: &'a TokenStream,
  pub set_post: &'a TokenStream,
  pub chain_set_suf: &'a TokenStream,
  pub chain_set_post: &'a TokenStream,
  pub replace_suf: &'a TokenStream,
  pub replace_post: &'a TokenStream,
}

impl<'a> ProcessedGenAccessorsAttrs<'a> {
  pub fn new(empty_token_stream: &'a TokenStream) -> Self {
    Self {
      name: None,
      receiver: None,
      attrs: empty_token_stream,
      get_suf: empty_token_stream,
      get_post: empty_token_stream,
      get_mut_suf: empty_token_stream,
      get_mut_post: empty_token_stream,
      get_copy_suf: empty_token_stream,
      get_copy_post: empty_token_stream,
      take_suf: empty_token_stream,
      take_post: empty_token_stream,
      set_suf: empty_token_stream,
      set_post: empty_token_stream,
      chain_set_suf: empty_token_stream,
      chain_set_post: empty_token_stream,
      replace_suf: empty_token_stream,
      replace_post: empty_token_stream,
    }
  }

  pub fn process(&mut self, attr: &'a GenAccessorsAttr) {
    match &attr.ident {
      GenAccessorsAttrIdent::Name            { .. } => self.name = Some(&attr.arg),
      GenAccessorsAttrIdent::Receiver        { .. } => self.receiver = Some(&attr.arg),
      GenAccessorsAttrIdent::Attrs           { .. } => self.attrs = &attr.arg,
      GenAccessorsAttrIdent::GetSuffix       { .. } => self.get_suf = &attr.arg,
      GenAccessorsAttrIdent::GetPostfix      { .. } => self.get_post = &attr.arg,
      GenAccessorsAttrIdent::GetMutSuffix    { .. } => self.get_mut_suf = &attr.arg,
      GenAccessorsAttrIdent::GetMutPostfix   { .. } => self.get_mut_post = &attr.arg,
      GenAccessorsAttrIdent::GetCopySuffix   { .. } => self.get_copy_suf = &attr.arg,
      GenAccessorsAttrIdent::GetCopyPostfix  { .. } => self.get_copy_post = &attr.arg,
      GenAccessorsAttrIdent::TakeSuffix      { .. } => self.take_suf = &attr.arg,
      GenAccessorsAttrIdent::TakePostfix     { .. } => self.take_post = &attr.arg,
      GenAccessorsAttrIdent::SetSuffix       { .. } => self.set_suf = &attr.arg,
      GenAccessorsAttrIdent::SetPostfix      { .. } => self.set_post = &attr.arg,
      GenAccessorsAttrIdent::ChainSetSuffix  { .. } => self.chain_set_suf = &attr.arg,
      GenAccessorsAttrIdent::ChainSetPostfix { .. } => self.chain_set_post = &attr.arg,
      GenAccessorsAttrIdent::ReplaceSuffix   { .. } => self.replace_suf = &attr.arg,
      GenAccessorsAttrIdent::ReplacePostfix  { .. } => self.replace_post = &attr.arg,
    }
  }
}