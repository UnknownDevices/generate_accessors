#![allow(unused)]
mod accessor;
mod gen_accessors_expr;
mod gen_accessors_attr;
use std::borrow::Borrow;
use std::fmt::Debug;
use syn::parse::ParseBuffer;
use std::str::FromStr;
use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::{Colon, Brace, Bracket};
use syn::{Type, parse_macro_input, bracketed, braced, Token, Expr, Member, Visibility, ExprType, ItemImpl, ImplItem};
use crate::accessor::{IsAccessor, Accessor};
use crate::gen_accessors_expr::GenAccessorsExpr;
use crate::gen_accessors_attr::{IsGenAccessorsAttr, GenAccessorsAttr, GenAccessorsAttrDiscriminants, ProcGenAccessorsAttrs};

#[derive(Debug)]
struct GenAccessorsItem {
  pub attrs: Vec<GenAccessorsAttr>,
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

    Ok(GenAccessorsItem { attrs, accessors_vis, accessors_constness, accessors_asyncness, 
      accessors_unsafety, bracket_token, accessors, for_token, brace_token, exprs })
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

    Ok(ItemGenAccessors { items })
  }
}

#[proc_macro]
pub fn generate_accessors(tokens: TokenStream) -> TokenStream {
  let input = parse_macro_input!(tokens as ItemGenAccessors);
  let mut output = TokenStream2::new();

  for item in input.items {
    let proc_attrs = ProcGenAccessorsAttrs::new(&item);
    for accessor_index in 0..item.accessors.len() {
      let accessor = &item.accessors[accessor_index];
      for expr_index in 0..item.exprs.len() {
        let expr = &item.exprs[expr_index];

        let attrs = proc_attrs.attrs(expr_index);

        let modifiers = {
          let vis = match accessor.vis() {
            Visibility::Inherited => item.accessors_vis.to_token_stream(),
            _ => accessor.vis().to_token_stream(),
          };
          let constness = accessor.constness().map_or_else(|| item.accessors_constness.map_or(
            TokenStream2::new(), |some| some.into_token_stream()), |some| some.into_token_stream());
          let asyncness = accessor.asyncness().map_or_else(|| item.accessors_asyncness.map_or(
            TokenStream2::new(), |some| some.into_token_stream()), |some| some.into_token_stream());
          let unsafety = accessor.unsafety().map_or_else(|| item.accessors_unsafety.map_or(
            TokenStream2::new(), |some| some.into_token_stream()), |some| some.into_token_stream());

          quote!(#vis #constness #asyncness #unsafety)
        };

        let member_name = proc_attrs.name(expr_index).map_or_else(|| expr.deduce_name(),
          |some| some.to_string());
        let name = TokenStream2::from_str(format!("{}{}{}",
          proc_attrs.suffix(expr_index, accessor_index), member_name,
          proc_attrs.postfix(expr_index, accessor_index)).as_str()).unwrap();

        let expr_ty = expr.ty.to_token_stream();
        let expr_expr = expr.expr.to_token_stream();
        let receiver_and_args: TokenStream2;
        let ret_ty: TokenStream2;
        let braces_cont: TokenStream2;
        match accessor {
          Accessor::Get(_) => {
            receiver_and_args = proc_attrs.receiver(expr_index).map_or_else(|| quote!(&self),
              |some| some.clone());
            ret_ty = quote!(&#expr_ty);
            braces_cont = quote!(&#expr_expr);
          },
          Accessor::GetMut(_) => {
            receiver_and_args = proc_attrs.receiver(expr_index).map_or_else(|| quote!(&mut self),
              |some| some.clone());
            ret_ty = quote!(&mut #expr_ty);
            braces_cont = quote!(&mut #expr_expr);
          },
          Accessor::GetCopy(_) => {
            receiver_and_args = proc_attrs.receiver(expr_index).map_or_else(|| quote!(&self),
              |some| some.clone());
            ret_ty = quote!(#expr_ty);
            braces_cont = quote!(#expr_expr.clone());
          },
          Accessor::Take(_) => {
            receiver_and_args = proc_attrs.receiver(expr_index).map_or_else(|| quote!(&mut self),
              |some| some.clone());
            ret_ty = quote!(#expr_ty);
            braces_cont = quote!(#expr_expr);
          },
          Accessor::Set(_) => {
            receiver_and_args = proc_attrs.receiver(expr_index).map_or_else(
              || quote!(&mut self, value: #expr_ty),
              |some| if some.is_empty() {
                quote!(value: #expr_ty)
              } else { quote!(#some, value: #expr_ty)});
            ret_ty = quote!(());
            braces_cont = quote!(#expr_expr = value;);
          },
          Accessor::ChainSet(_) => {
            receiver_and_args = proc_attrs.receiver(expr_index).map_or_else(
              || quote!(&mut self, value: #expr_ty),
              |some| if some.is_empty() {
                quote!(value: #expr_ty)
              } else { quote!(#some, value: #expr_ty)});
            ret_ty = quote!(&mut Self);
            braces_cont = quote!(#expr_expr = value; self);
          },
          Accessor::Replace(_) => {
            receiver_and_args = proc_attrs.receiver(expr_index).map_or_else(
              || quote!(&mut self, value: #expr_ty),
              |some| if some.is_empty() {
                quote!(value: #expr_ty)
              } else { quote!(#some, value: #expr_ty)});
            ret_ty = quote!(#expr_ty);
            braces_cont = quote!(
              let output = #expr_expr;
              #expr_expr = value;
              output);
          },
        };

        output.extend(quote!(
          #attrs
          #modifiers fn #name (#receiver_and_args) -> #ret_ty {
            #braces_cont
          }
        ).into_iter());
      }
    }
  }

  output.into()
}

struct GenAccessorsOutput {
  items: Vec<ImplItem>,
}

impl Parse for GenAccessorsOutput {
  fn parse(input: ParseStream) -> Result<Self, syn::Error> {
    let mut items = Vec::<ImplItem>::new();
    while !input.is_empty() {
      items.push(ImplItem::Method(input.parse()?));
    }

    Ok(Self { items })
  }
}

#[proc_macro_attribute]
pub fn expand_generate_accessors(args_tokens: TokenStream, tokens: TokenStream) -> TokenStream {
  let mut input = parse_macro_input!(tokens as ItemImpl);

  for item_index in 0..input.items.len() {
    if let ImplItem::Macro(item_macro) = &input.items[item_index] {
      if item_macro.mac.path.to_token_stream().to_string() == "generate_accessors" {
        let gen_accessors_output = {
          let tokens = generate_accessors(item_macro.mac.tokens.clone().into());
          parse_macro_input!(tokens as GenAccessorsOutput)
        };

        let mut v = input.items.split_off(item_index);
        input.items.splice(item_index..item_index, gen_accessors_output.items);
      }
    }
  }

  input.into_token_stream().into()
}