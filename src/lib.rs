#![allow(unused, unused_variables)]
#![feature(core_intrinsics)]
mod attr;
mod accessor;
use accessor::*;
use attr::*;
use syn::token::{Colon, Token};
use syn::{Block, Stmt, Type};
use syn::parse::ParseBuffer;
use ::std::borrow::Borrow;
use ::std::fmt::{Debug, Display};
use ::std::fmt;
use std::intrinsics::discriminant_value;
use std::iter;
use std::mem::{discriminant, MaybeUninit, size_of};
use ::std::str::FromStr;
use ::proc_macro2::{Span, TokenStream};
use ::quote::{quote, format_ident, ToTokens, TokenStreamExt};
use ::syn::{Attribute, NestedMeta, AttributeArgs, parse2, Receiver, parse_macro_input, bracketed, braced, Token, Expr, Member, Visibility, ExprType, Ident};
use ::syn::parse::{Parse, ParseStream, Parser};
use ::syn::punctuated::Punctuated;
use ::syn::spanned::Spanned;
use ::syn::token::{Brace, Bracket};

fn deduce_expr_name(expr: &Expr) -> TokenStream {
  return match expr.borrow() {
    Expr::Field(expr_field) => {
      match &expr_field.member {
        Member::Named(member_named) => member_named.to_token_stream().into(),
        Member::Unnamed(_) => panic!(),
      }
    },
    Expr::Path(expr_path) => {
      expr_path.path.segments.last().unwrap().ident.to_token_stream()
    },
    Expr::MethodCall(expr_method_call) => {
      expr_method_call.method.to_token_stream()
    },
    Expr::Try(expr_try) => {
      deduce_expr_name(expr_try.expr.borrow())
    },
    Expr::Call(expr_call) => {
      deduce_expr_name(expr_call.func.borrow())
    },
    Expr::Unary(expr_unary) => {
      deduce_expr_name(expr_unary.expr.borrow())
    },
    Expr::Cast(expr_cast) => {
      deduce_expr_name(expr_cast.expr.borrow())
    },
    Expr::Reference(expr_reference) => {
      deduce_expr_name(expr_reference.expr.borrow())
    },
    Expr::Box(expr_box) => {
      deduce_expr_name(expr_box.expr.borrow())
    },
    Expr::Binary(expr_binary) => {
      deduce_expr_name(expr_binary.left.borrow())
    },
    Expr::Paren(expr_paren) => {
      deduce_expr_name(expr_paren.expr.borrow())
    }
    _ => panic!(),
  }
}

#[derive(Debug)]
struct GenAccessorsExpr {
  pub attrs: Vec<Attr>,
  pub expr: Box<Expr>,
  pub colon_token: Colon,
  pub ty: Box<Type>,
}

impl Parse for GenAccessorsExpr {
  fn parse(input: ParseStream) -> Result<Self, syn::Error>  {
    let mut attrs = Vec::new();
    while input.peek(Token![#]) {
      attrs.push(input.parse()?);
    }
    let expr_type = input.parse::<ExprType>()?;
    let expr = expr_type.expr;
    let colon_token = expr_type.colon_token;
    let ty = expr_type.ty;

    Ok(GenAccessorsExpr { attrs, expr, colon_token, ty, })
  }
}

#[derive(Debug)]
struct GenAccessorsItem {
  pub attrs: Vec<Attr>,
  pub accessors_vis: Visibility,
  pub accessors_constness: Option<Token![const]>,
  pub accessors_asyncness: Option<Token![async]>,
  pub accessors_unsafety: Option<Token![unsafe]>,
  pub bracket_token: Bracket,
  pub accessors: Punctuated<Accessor, Token![,]>,
  pub for_token: Token![for],
  pub brace_token: Brace,
  pub exprs: Punctuated<GenAccessorsExpr, Token![,]>,
}

impl Parse for GenAccessorsItem {
  fn parse(input: ParseStream) -> Result<Self, syn::Error> {
    let brackets_content: ParseBuffer;
    let braces_content: ParseBuffer;

    let mut attrs = Vec::new();
    while input.peek(Token![#]) {
      attrs.push(input.parse()?);
    }
    let accessors_vis = input.parse()?;
    let accessors_constness = input.parse().ok();
    let accessors_asyncness = input.parse().ok();
    let accessors_unsafety = input.parse().ok();
    let bracket_token = bracketed!(brackets_content in input);
    let accessors = brackets_content.parse_terminated(Accessor::parse)?;
    let for_token = input.parse()?;
    let brace_token = braced!(braces_content in input);
    let exprs = braces_content.parse_terminated(GenAccessorsExpr::parse)?;

    Ok(GenAccessorsItem {
      attrs,
      accessors_vis,
      accessors_constness,
      accessors_asyncness,
      accessors_unsafety,
      bracket_token,
      accessors,
      for_token,
      brace_token,
      exprs,
    })
  }
}

struct ItemGenAccessors {
  pub items: Vec<GenAccessorsItem>,
}

impl Parse for ItemGenAccessors {
  fn parse(input: ParseStream) -> Result<Self, syn::Error>  {
    let mut items = Vec::new();
    while !input.is_empty() {
      items.push(input.parse()?);
    }

    Ok(ItemGenAccessors { items, })
  }
}

impl ItemGenAccessors {
  fn process_attrs(&self) -> Vec<ProcAttrs> {
    todo!()
  }
}

#[derive(Debug, Clone)]
struct ProcAttrs<'a>(
  // TODO: use tuples not arrays?
  Vec<[Option<&'a TokenStream>; 3]>, 
  // TODO: use String not TokenStream?
  Vec<[TokenStream; 2]>, 
);

impl<'a> ProcAttrs<'a> {
  fn name(&self, expr_index: usize) -> Option<&'a TokenStream> { 
    self.0[expr_index][0] 
  }
  fn receiver(&self, expr_index: usize) -> Option<&'a TokenStream> { 
    self.0[expr_index][1] 
  }
  fn attrs(&self, expr_index: usize) -> Option<&'a TokenStream> { 
    self.0[expr_index][2]
  }
  fn suffix(&self, acessor_index: usize, expr_index: usize) -> &TokenStream {
    &self.1[self.0.len() * acessor_index + expr_index][0]
  }
  fn postfix(&self, acessor_index: usize, expr_index: usize) -> &TokenStream {
    &self.1[self.0.len() * acessor_index + expr_index][1]
  }

  fn new(item: &'a GenAccessorsItem) -> Self {
    let mut output = Self {0: Vec::new(), 1: Vec::new() };
    let mut name = Option::<&'a TokenStream>::None;
    let mut receiver = Option::<&'a TokenStream>::None;
    let mut attrs = Option::<&'a TokenStream>::None;
    let mut fixes = Vec::<[TokenStream; 2]>::new();
    let mut fix_attr_discrs_to_get = Vec::<[AttrDiscriminants; 2]>::new();

    output.0.reserve_exact(item.exprs.len());
    output.1.reserve_exact(item.exprs.len() * item.accessors.len());
    fixes.reserve_exact(item.accessors.len());
    fix_attr_discrs_to_get.reserve_exact(item.accessors.len());

    for accessor_index in 0..item.accessors.len() {
      let (fixes_elem, fix_attr_discrs_to_get_elem) = 
      match &item.accessors[accessor_index] {
        Accessor::Get(_) =>
          ([TokenStream::new(), TokenStream::new()],
          [AttrDiscriminants::GetSuf, AttrDiscriminants::GetPost]),
        Accessor::GetMut(_) =>
          ([TokenStream::new(), TokenStream::from_str("_mut").unwrap()],
          [AttrDiscriminants::GetMutSuf, AttrDiscriminants::GetMutPost]),
        Accessor::GetCopy(_) => 
          ([TokenStream::new(), TokenStream::new()],
          [AttrDiscriminants::GetCopySuf, AttrDiscriminants::GetCopyPost]),
        Accessor::Take(_) => 
          ([TokenStream::from_str("take_").unwrap(), TokenStream::new()],
          [AttrDiscriminants::TakeSuf, AttrDiscriminants::TakePost]),
        Accessor::Set(_) => 
          ([TokenStream::from_str("set_").unwrap(), TokenStream::new()],
            [AttrDiscriminants::SetSuf, AttrDiscriminants::SetPost]),
        Accessor::ChainSet(_) => 
          ([TokenStream::from_str("set_").unwrap(), TokenStream::new()],
            [AttrDiscriminants::ChainSetSuf, AttrDiscriminants::ChainSetPost]),
        Accessor::Replace(_) => 
          ([TokenStream::from_str("replace_").unwrap(), TokenStream::new()],
          [AttrDiscriminants::ReplaceSuf, AttrDiscriminants::ReplacePost]),
      };

      fixes.push(fixes_elem);
      fix_attr_discrs_to_get.push(fix_attr_discrs_to_get_elem);
    }

    for attr in &item.attrs {
      match attr {
        Attr::Name(attr_name) => name = Some(attr_name.arg()),
        Attr::Receiver(attr_receiver) => receiver = Some(attr_receiver.arg()),
        Attr::Attrs(attr_attrs) => attrs = Some(attr_attrs.arg()),
        _ => {
          let attr_discr = AttrDiscriminants::from(attr);
          for accessor_index in 0..item.accessors.len() {
            if attr_discr == fix_attr_discrs_to_get[accessor_index][0] {
              fixes[accessor_index][0] = attr.arg().clone();
            } else if attr_discr == fix_attr_discrs_to_get[accessor_index][1] {
              fixes[accessor_index][1] = attr.arg().clone();
            }
          }
        },
      }
    }

    for expr in &item.exprs {
      let mut name = name.clone();
      let mut receiver = receiver.clone();
      let mut attrs = attrs.clone();
      let mut fixes = fixes.clone();
      for attr in &expr.attrs {
        match attr {
          Attr::Name(attr_name) => name = Some(attr_name.arg()),
          Attr::Receiver(attr_receiver) => receiver = Some(attr_receiver.arg()),
          Attr::Attrs(attr_attrs) => attrs = Some(attr_attrs.arg()),
          _ => {
            let attr_discr = AttrDiscriminants::from(attr);
            for accessor_index in 0..item.accessors.len() {
              if attr_discr == fix_attr_discrs_to_get[accessor_index][0] {
                fixes[accessor_index][0] = attr.arg().clone();
              } else if attr_discr == fix_attr_discrs_to_get[accessor_index][1] {
                fixes[accessor_index][1] = attr.arg().clone();
              }
            }
          },
        }
      }

      output.0.push([name, receiver, attrs]);
      output.1.append(&mut fixes);
    }

    println!("{output:#?}");
    output
  }
}


#[proc_macro]
pub fn generate_accessors(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = parse_macro_input!(tokens as ItemGenAccessors);
  let mut output = TokenStream::new();

  for item in input.items {
    ProcAttrs::new(&item);
  }

  output.into()
}
