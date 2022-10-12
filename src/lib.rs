#![allow(unused, unused_variables)]

mod gen_accessors_attr;
mod accessors;
use accessors::*;
use gen_accessors_attr::{GenAccessorsAttr, GenAccessorsAttrIdent};
use syn::token::Colon;
use syn::{Block, Stmt, Type};
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
struct GenAccessorsExpr {
  pub attrs: Vec<GenAccessorsAttr>,
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

// TODO: format_tokens macro??

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

fn deduce_member_name(expr: &Expr) -> TokenStream {
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
      deduce_member_name(expr_try.expr.borrow())
    },
    Expr::Call(expr_call) => {
      deduce_member_name(expr_call.func.borrow())
    },
    Expr::Unary(expr_unary) => {
      deduce_member_name(expr_unary.expr.borrow())
    },
    Expr::Cast(expr_cast) => {
      deduce_member_name(expr_cast.expr.borrow())
    },
    Expr::Reference(expr_reference) => {
      deduce_member_name(expr_reference.expr.borrow())
    },
    Expr::Box(expr_box) => {
      deduce_member_name(expr_box.expr.borrow())
    },
    Expr::Binary(expr_binary) => {
      deduce_member_name(expr_binary.left.borrow())
    },
    Expr::Paren(expr_paren) => {
      deduce_member_name(expr_paren.expr.borrow())
    }
    _ => panic!(),
  }
}

#[proc_macro]
pub fn generate_accessors(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let input = parse_macro_input!(tokens as ItemGenAccessors);
  let mut output = TokenStream::new();

  let default_suffixes: ForEachAccessorIdent<TokenStream> = ForEachAccessorIdent{ 
    take: TokenStream::from_str("take_").unwrap(),
    set: TokenStream::from_str("set_").unwrap(),
    chain_set: TokenStream::from_str("set_").unwrap(),
    replace: TokenStream::from_str("replace_").unwrap(),
    ..ForEachAccessorIdent::new_repeat(TokenStream::new())
  };
  let default_postfixes: ForEachAccessorIdent<TokenStream> = ForEachAccessorIdent{ 
    get_mut: TokenStream::from_str("_mut").unwrap(),
    ..ForEachAccessorIdent::new_repeat(TokenStream::new())
  };

  for item in input.items {
    let mut member_name = Option::<TokenStream>::None;
    let mut method_receiver = Option::<TokenStream>::None;
    let mut into_cast = Option::<TokenStream>::None;
    let mut method_attrs = TokenStream::new();
    let mut suffixes = default_suffixes.clone();
    let mut postfixes = default_postfixes.clone();
    for attr in &item.attrs {
      attr.unpack_into(&mut member_name, &mut method_receiver, &mut into_cast,
        &mut method_attrs, &mut suffixes, &mut postfixes);
    }

    for accessor in item.accessors.iter() {
      for expr in item.exprs.iter() {
        let mut member_name = member_name.clone();
        let mut method_receiver = method_receiver.clone();
        let mut into_cast = into_cast.clone();
        let mut method_attrs = method_attrs.clone();
        let mut suffixes = suffixes.clone();
        let mut postfixes = postfixes.clone();
        for attr in &expr.attrs {
          attr.unpack_into(&mut member_name, &mut method_receiver, &mut into_cast,
            &mut method_attrs,  &mut suffixes, &mut postfixes);
        }

        let member_name = member_name.map_or_else(|| deduce_member_name(&expr.expr), 
          |some| some.into_token_stream());

        let method_modifiers = {
          let vis = match accessor.vis {
            Visibility::Inherited => item.accessors_vis.to_token_stream(),
            _ => accessor.vis.to_token_stream(),
          };
          let constness = accessor.constness.map_or_else(|| item.accessors_constness.map_or(
            TokenStream::new(), |some| some.into_token_stream()), |some| some.into_token_stream());
          let asyncness = accessor.asyncness.map_or_else(|| item.accessors_asyncness.map_or(
            TokenStream::new(), |some| some.into_token_stream()), |some| some.into_token_stream());
          let unsafety = accessor.unsafety.map_or_else(|| item.accessors_unsafety.map_or(
            TokenStream::new(), |some| some.into_token_stream()), |some| some.into_token_stream());

          quote!(#vis #constness #asyncness #unsafety)
        };

        let method_ident: TokenStream;
        let method_args: TokenStream;
        let method_ret_ty: TokenStream;
        let method_expr: TokenStream;
        let do_into_cast = into_cast.is_some();
        let expr_ty = into_cast.unwrap_or_else(|| expr.ty.to_token_stream());
        let expr_expr = expr.expr.to_token_stream();
        match accessor.ty {
          AccessorIdent::Get { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.get.to_string(), member_name.to_string(),
              postfixes.get.to_string()).as_str()).unwrap();            
            method_args = method_receiver.unwrap_or_else(|| quote!(&mut self));
            method_ret_ty = quote!(&#expr_ty);
            method_expr = if do_into_cast { 
              quote!((&#expr_expr).into()) 
            } else { quote!(&#expr_expr) };
          },
          AccessorIdent::GetMut { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.get_mut.to_string(), member_name.to_string(),
              postfixes.get_mut.to_string()).as_str()).unwrap();
            method_args = method_receiver.unwrap_or_else(|| quote!(&mut self));
            method_ret_ty = quote!(&mut #expr_ty);
            method_expr = if do_into_cast { 
              quote!((&mut #expr_expr).into()) 
            } else { quote!(&mut #expr_expr) };
          },
          AccessorIdent::GetCopy { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.get_copy.to_string(), member_name.to_string(),
              postfixes.get_copy.to_string()).as_str()).unwrap();
            method_args = method_receiver.unwrap_or_else(|| quote!(&self));
            method_ret_ty = quote!(#expr_ty);
            method_expr = if do_into_cast { 
              quote!((#expr_expr.clone()).into()) 
            } else { quote!(#expr_expr.clone()) };
          },
          AccessorIdent::Take { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.take.to_string(), member_name.to_string(),
              postfixes.take.to_string()).as_str()).unwrap();
            method_args = method_receiver.unwrap_or_else(|| quote!(&mut self));
            method_ret_ty = quote!(#expr_ty);
            method_expr = if do_into_cast { 
              quote!((#expr_expr).into()) 
            } else { quote!(#expr_expr) };
          },
          AccessorIdent::Set { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.set.to_string(), member_name.to_string(),
              postfixes.set.to_string()).as_str()).unwrap();
              method_args = method_receiver.map_or_else(|| quote!(&mut self, value: #expr_ty), 
                |some| if some.is_empty() { 
                  quote!(value: #expr_ty) 
                } else { quote!(#some, value: #expr_ty) });
            method_ret_ty = quote!(());
            method_expr = if do_into_cast { 
              quote!(#expr_expr = value.into();) 
            } else { quote!(#expr_expr = value;)};
          },
          AccessorIdent::ChainSet { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.chain_set.to_string(), member_name.to_string(),
              postfixes.chain_set.to_string()).as_str()).unwrap();
            method_args = method_receiver.map_or_else(|| quote!(&mut self, value: #expr_ty), 
              |some| if some.is_empty() { 
                quote!(value: #expr_ty) 
              } else { quote!(#some, value: #expr_ty) });
            method_ret_ty = quote!(&mut Self);
            method_expr = if do_into_cast {
              quote!(#expr_expr = value.into(); self)
            } else { quote!(#expr_expr = value; self) };
          },
          AccessorIdent::Replace { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              suffixes.replace.to_string(), member_name.to_string(),
              postfixes.replace.to_string()).as_str()).unwrap();
            method_args = method_receiver.map_or_else(|| quote!(&mut self, value: #expr_ty), 
              |some| if some.is_empty() { 
                quote!(value: #expr_ty) 
              } else { quote!(#some, value: #expr_ty) } 
            );
            method_ret_ty = quote!(#expr_ty);
            method_expr = if do_into_cast { 
              quote!(
                let output = #expr_expr;
                #expr_expr = value.into(); 
                output.into()) 
            } else { 
              quote!(
                let output = #expr_expr;
                #expr_expr = value;
                output) 
            };
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
