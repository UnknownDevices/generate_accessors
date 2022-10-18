#![allow(unused)]
mod attr;
mod accessor;
use accessor::*;
use attr::*;
use syn::parse::ParseBuffer;
use std::borrow::Borrow;
use std::fmt::Debug;
use std::str::FromStr;
use proc_macro2::TokenStream;
use quote::{quote, ToTokens};
use syn::parse::{Parse, ParseStream};
use syn::punctuated::Punctuated;
use syn::token::{Colon, Brace, Bracket};
use syn::{Type, parse_macro_input, bracketed, braced, Token, Expr, Member, Visibility, ExprType};

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
  Vec<[String; 2]>, 
);

impl<'a> ProcAttrs<'a> {
  fn name(&self, expr_index: usize) -> &Option<&'a TokenStream> { 
    &self.0[expr_index][0] 
  }
  fn receiver(&self, expr_index: usize) -> &Option<&'a TokenStream> { 
    &self.0[expr_index][1] 
  }
  fn attrs(&self, expr_index: usize) -> &Option<&'a TokenStream> { 
    &self.0[expr_index][2]
  }
  fn suffix(&self, acessor_index: usize, expr_index: usize) -> &String {
    &self.1[self.1.len() / self.0.len() * acessor_index + expr_index][0]
  }
  fn postfix(&self, expr_index: usize, accessor_index: usize) -> &String {
    &self.1[self.1.len() / self.0.len() * expr_index + accessor_index][1]
  }

  fn proc_attr(attr: &'a Attr, name: &mut Option<&'a TokenStream>, 
    receiver: &mut Option<&'a TokenStream>, attrs: &mut Option<&'a TokenStream>, 
    fixes: &mut Vec<[String; 2]>, item: &GenAccessorsItem, 
    disc_of_fix_attrs_to_get: &Vec<[AttrDiscriminants; 2]>) 
  {
    match attr {
      Attr::Name(attr_name) => *name = Some(attr_name.arg()),
      Attr::Receiver(attr_receiver) => *receiver = Some(attr_receiver.arg()),
      Attr::Attrs(attr_attrs) => *attrs = Some(attr_attrs.arg()),
      _ => {
        let attr_disc = AttrDiscriminants::from(attr);
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

  fn new(item: &'a GenAccessorsItem) -> Self {
    let mut output = Self {0: Vec::new(), 1: Vec::new() };
    let mut name = Option::<&'a TokenStream>::None;
    let mut receiver = Option::<&'a TokenStream>::None;
    let mut attrs = Option::<&'a TokenStream>::None;
    let mut fixes = Vec::<[String; 2]>::new();
    let mut disc_of_fix_attrs_to_get = Vec::<[AttrDiscriminants; 2]>::new();

    output.0.reserve_exact(item.exprs.len());
    output.1.reserve_exact(item.exprs.len() * item.accessors.len());
    fixes.reserve_exact(item.accessors.len());
    disc_of_fix_attrs_to_get.reserve_exact(item.accessors.len());

    for accessor_index in 0..item.accessors.len() {
      let (fixes_elem, disc_of_fix_attrs_to_get_elem) = 
      match &item.accessors[accessor_index] {
        Accessor::Get(_) =>
          ([String::new(), String::new()],
          [AttrDiscriminants::GetSuf, AttrDiscriminants::GetPost]),
        Accessor::GetMut(_) =>
          ([String::new(), "_mut".to_string()],
          [AttrDiscriminants::GetMutSuf, AttrDiscriminants::GetMutPost]),
        Accessor::GetCopy(_) => 
          ([String::new(), String::new()],
          [AttrDiscriminants::GetCopySuf, AttrDiscriminants::GetCopyPost]),
        Accessor::Take(_) => 
          (["take_".to_string(), String::new()],
          [AttrDiscriminants::TakeSuf, AttrDiscriminants::TakePost]),
        Accessor::Set(_) => 
          (["set_".to_string(), String::new()],
            [AttrDiscriminants::SetSuf, AttrDiscriminants::SetPost]),
        Accessor::ChainSet(_) => 
          (["set_".to_string(), String::new()],
            [AttrDiscriminants::ChainSetSuf, AttrDiscriminants::ChainSetPost]),
        Accessor::Replace(_) => 
          (["replace_".to_string(), String::new()],
          [AttrDiscriminants::ReplaceSuf, AttrDiscriminants::ReplacePost]),
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

#[proc_macro]
pub fn generate_accessors(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = parse_macro_input!(tokens as ItemGenAccessors);
  let mut output = TokenStream::new();

  for item in input.items {
    let args = ProcAttrs::new(&item);
    
    for accessor_index in 0..item.accessors.len() {
      let accessor = &item.accessors[accessor_index];

      for expr_index in 0..item.exprs.len() {
        let expr = &item.exprs[expr_index];
      
        let member_name = args.name(expr_index).map_or_else(|| deduce_expr_name(&expr.expr), 
          |some| some.to_token_stream());

        let method_modifiers = {
          let vis = match accessor.vis() {
            Visibility::Inherited => item.accessors_vis.to_token_stream(),
            _ => accessor.vis().to_token_stream(),
          };
          let constness = accessor.constness().map_or_else(|| item.accessors_constness.map_or(
            TokenStream::new(), |some| some.into_token_stream()), |some| some.into_token_stream());
          let asyncness = accessor.asyncness().map_or_else(|| item.accessors_asyncness.map_or(
            TokenStream::new(), |some| some.into_token_stream()), |some| some.into_token_stream());
          let unsafety = accessor.unsafety().map_or_else(|| item.accessors_unsafety.map_or(
            TokenStream::new(), |some| some.into_token_stream()), |some| some.into_token_stream());

          quote!(#vis #constness #asyncness #unsafety)
        };

        let expr_ty = expr.ty.to_token_stream();
        let expr_expr = expr.expr.to_token_stream();
        let method_attrs = args.attrs(expr_index);
        let method_ident = TokenStream::from_str(format!("{}{}{}", 
          args.suffix(expr_index, accessor_index), 
          member_name.to_string(),
          args.postfix(expr_index, accessor_index)).as_str()).unwrap();  
        let method_args: TokenStream;
        let method_ret_ty: TokenStream;
        let method_expr: TokenStream;
        match accessor {
          Accessor::Get(_) => {
            method_args = args.receiver(expr_index).map_or_else(|| quote!(&self),
              |some| some.clone());
            method_ret_ty = quote!(&#expr_ty);
            method_expr = quote!(&#expr_expr);
          },
          Accessor::GetMut(_) => {
            method_args = args.receiver(expr_index).map_or_else(|| quote!(&mut self),
                |some| some.clone());
            method_ret_ty = quote!(&mut #expr_ty);
            method_expr = quote!(&mut #expr_expr);
          },
          Accessor::GetCopy(_) => {
            method_args = args.receiver(expr_index).map_or_else(|| quote!(&self),
                |some| some.clone());
            method_ret_ty = quote!(#expr_ty);
            method_expr = quote!(#expr_expr.clone());
          },
          Accessor::Take(_) => {
            method_args = args.receiver(expr_index).map_or_else(|| quote!(&mut self),
                |some| some.clone());
            method_ret_ty = quote!(#expr_ty);
            method_expr = quote!(#expr_expr);
          },
          Accessor::Set(_) => {
            method_args = args.receiver(expr_index).map_or_else(
              || quote!(&mut self, value: #expr_ty), 
              |some| if some.is_empty() { 
                quote!(value: #expr_ty) 
              } else { quote!(#some, value: #expr_ty)});
            method_ret_ty = quote!(());
            method_expr = quote!(#expr_expr = value;);
          },
          Accessor::ChainSet(_) => {
            method_args = args.receiver(expr_index).map_or_else(
              || quote!(&mut self, value: #expr_ty),
              |some| if some.is_empty() { 
                quote!(value: #expr_ty) 
              } else { quote!(#some, value: #expr_ty)});
            method_ret_ty = quote!(&mut Self);
            method_expr = quote!(#expr_expr = value; self);
          },
          Accessor::Replace(_) => {
            method_args = args.receiver(expr_index).map_or_else(
              || quote!(&mut self, value: #expr_ty), 
              |some| if some.is_empty() { 
                quote!(value: #expr_ty) 
              } else { quote!(#some, value: #expr_ty)});
            method_ret_ty = quote!(#expr_ty);
            method_expr = quote!(
              let output = #expr_expr;
              #expr_expr = value;
              output);
          },
        };

        output.extend(quote!(
          #method_attrs
          #method_modifiers fn #method_ident (#method_args) -> #method_ret_ty {
            #method_expr
          }
        ).into_iter());
      }
    }
  }

  output.into()
}
