#![allow(unused, unused_variables)]

mod gen_accessors_attr;
mod accessors;
use accessors::*;
use gen_accessors_attr::{GenAccessorsAttr, GenAccessorsAttrIdent};
use syn::parse::ParseBuffer;
use ::std::borrow::Borrow;
use ::std::fmt::{Debug, Display};
use ::std::fmt;
use ::std::str::FromStr;
use ::proc_macro2::{Span, TokenStream};
use ::quote::{quote, format_ident, ToTokens, TokenStreamExt};
use ::syn::{Attribute, NestedMeta, AttributeArgs, parse2, Receiver, parse_macro_input, bracketed, braced, Token, Expr, Member, Visibility, ExprType, Ident};
use ::syn::parse::{Parse, ParseStream, Parser};
use ::syn::punctuated::Punctuated;
use ::syn::spanned::Spanned;
use ::syn::token::{Brace, Bracket};

#[derive(Debug)]
struct GenAccessorsItem {
  attrs: Vec<GenAccessorsAttr>,
  accessors_vis: Visibility,
  accessors_constness: Option<Token![const]>,
  accessors_asyncness: Option<Token![async]>,
  accessors_unsafety: Option<Token![unsafe]>,
  bracket_token: Bracket,
  accessors: Punctuated<Accessor, Token![,]>,
  for_token: Token![for],
  brace_token: Brace,
  exprs: Punctuated<AccessorsExpr, Token![,]>,
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
    let exprs = braces_content.parse_terminated(AccessorsExpr::parse)?;

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

// TODO: potential proc macro work?
#[derive(Default, Debug, Clone)]
struct ForEachAccessorIdent<T> {
  pub get: T,
  pub get_mut: T,
  pub get_copy: T,
  pub take: T,
  pub set: T,
  pub chain_set: T,
  pub replace: T,
}

impl<T: Clone> ForEachAccessorIdent<T> {
  pub fn new_with(value: T) -> Self {
    Self {
      get: value.clone(),
      get_mut: value.clone(),
      get_copy: value.clone(),
      take: value.clone(),
      set: value.clone(),
      chain_set: value.clone(),
      replace: value.clone(),
    }
  }
}

// TODO: format_tokens macro??

struct ItemGenAccessors {
  items: Vec<GenAccessorsItem>,
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

#[proc_macro]
pub fn generate_accessors(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = parse_macro_input!(tokens as ItemGenAccessors);
  let mut output = TokenStream::new();

  let def_suffixes: ForEachAccessorIdent<TokenStream> = ForEachAccessorIdent{ 
    take: TokenStream::from_str("take_").unwrap(),
    set: TokenStream::from_str("set_").unwrap(),
    chain_set: TokenStream::from_str("set_").unwrap(),
    replace: TokenStream::from_str("replace_").unwrap(),
    ..ForEachAccessorIdent::new_with(TokenStream::new())
  };
  let def_postfixes: ForEachAccessorIdent<TokenStream> = ForEachAccessorIdent{ 
    get_mut: TokenStream::from_str("_mut").unwrap(),
    ..ForEachAccessorIdent::new_with(TokenStream::new())
  };

  for item in input.items {
    let mut name = TokenStream::new();
    let mut suffixes = def_suffixes.clone();
    let mut postfixes = def_postfixes.clone();
    for attr in &item.attrs {
      attr.unpack_into(&mut name, &mut suffixes, &mut postfixes);
    }

    for accessor in item.accessors.iter() {
      for expr in item.exprs.iter() {
        let mut name = name.clone();
        let mut suffixes = suffixes.clone();
        let mut postfixes = postfixes.clone();
        for attr in &expr.attrs {
          attr.unpack_into(&mut name, &mut suffixes, &mut postfixes);
        }

        let name = if name.is_empty() {
          match expr.expr.expr.borrow() {
            Expr::Field(expr_field) => {
              match &expr_field.member {
                Member::Named(member_named) => member_named.to_token_stream().into(),
                Member::Unnamed(_) => panic!(),
              }
            },
            _ => panic!(),
          }
        } else { name };

        let method_modifiers = {
          let vis = match accessor.vis {
            Visibility::Inherited => item.accessors_vis.to_token_stream(),
            _ => accessor.vis.to_token_stream(),
          };
          let constness = accessor.constness.map_or(item.accessors_constness.map_or(
            TokenStream::new(), |some| some.into_token_stream()), |some| some.into_token_stream());
          let asyncness = accessor.asyncness.map_or(item.accessors_asyncness.map_or(
            TokenStream::new(), |some| some.into_token_stream()), |some| some.into_token_stream());
          let unsafety = accessor.unsafety.map_or(item.accessors_unsafety.map_or(
            TokenStream::new(), |some| some.into_token_stream()), |some| some.into_token_stream());

          quote!(#vis #constness #asyncness #unsafety)
        };

        let method_ident: TokenStream;
        let method_args: TokenStream;
        let method_ret_ty: TokenStream;
        let method_expr: TokenStream;
        let expr_ty = &expr.expr.ty;
        let expr_expr = &expr.expr.expr;
        match accessor.ty {
          AccessorIdent::Get { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.get.to_string(), name.to_string(),
              postfixes.get.to_string()).as_str()).unwrap();
            method_args = quote!(&self);
            method_ret_ty = quote!(&#expr_ty);
            method_expr = quote!(&#expr_expr);
          },
          AccessorIdent::GetMut { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.get_mut.to_string(), name.to_string(),
              postfixes.get_mut.to_string()).as_str()).unwrap();
            method_args = quote!(&mut self);
            method_ret_ty = quote!(&mut #expr_ty);
            method_expr = quote!(&mut #expr_expr);
          },
          AccessorIdent::GetCopy { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.get_copy.to_string(), name.to_string(),
              postfixes.get_copy.to_string()).as_str()).unwrap();
            method_args = quote!(&self);
            method_ret_ty = quote!(#expr_ty);
            method_expr = quote!(#expr_expr.clone());
          },
          AccessorIdent::Take { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.take.to_string(), name.to_string(),
              postfixes.take.to_string()).as_str()).unwrap();
            method_args = quote!(&mut self);
            method_ret_ty = quote!(#expr_ty);
            method_expr = quote!(#expr_expr);
          },
          AccessorIdent::Set { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.set.to_string(), name.to_string(),
              postfixes.set.to_string()).as_str()).unwrap();
            method_args = quote!(&mut self, value: #expr_ty);
            method_ret_ty = quote!(());
            method_expr = quote!(#expr_expr = value;);
          },
          AccessorIdent::ChainSet { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.chain_set.to_string(), name.to_string(),
              postfixes.chain_set.to_string()).as_str()).unwrap();
            method_args = quote!(&mut self, value: #expr_ty);
            method_ret_ty = quote!(&mut Self);
            method_expr = quote!(
              #expr_expr = value;
              self
            );
          },
          AccessorIdent::Replace { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.replace.to_string(), name.to_string(),
              postfixes.replace.to_string()).as_str()).unwrap();
            method_args = quote!(&mut self, value: #expr_ty);
            method_ret_ty = quote!(#expr_ty);
            method_expr = quote!(
              let output = #expr_expr;
              #expr_expr = value;
              output
            );
          },
        };

        output.extend(quote!(
          #method_modifiers fn #method_ident (#method_args) -> #method_ret_ty {
            #method_expr
          }
        ).into_iter());
      }
    }
  }

  output.into()
}
