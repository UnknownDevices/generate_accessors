#![allow(unused, unused_variables)]

mod gen_accessors_attr;
mod accessors;
use accessors::*;
use gen_accessors_attr::{GenAccessorsAttr, GenAccessorsAttrIdent, ProcessedGenAccessorsAttrs};
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

  let empty_token_stream = TokenStream::new();
  let def_get_mut_post = TokenStream::from_str("_mut").unwrap();
  let def_take_suf = TokenStream::from_str("take_").unwrap();
  let def_set_suf = TokenStream::from_str("set_").unwrap();
  let def_chain_set_suf = TokenStream::from_str("set_").unwrap();
  let def_replace_suf = TokenStream::from_str("replace_").unwrap();

  for item in input.items {
    let mut args = ProcessedGenAccessorsAttrs {
      get_mut_post: &def_get_mut_post,
      take_suf: &def_take_suf,
      set_suf: &def_set_suf,
      chain_set_suf: &def_chain_set_suf,
      replace_suf: &def_replace_suf,
      ..ProcessedGenAccessorsAttrs::new(&empty_token_stream)
    };
    for attr in &item.attrs { args.process(attr); }

    for accessor in item.accessors.iter() {
      for expr in item.exprs.iter() {
        let mut args = args.clone();
        for attr in &expr.attrs { args.process(attr); }

        let member_name = args.name.map_or_else(|| deduce_member_name(&expr.expr), 
          |some| some.to_token_stream());

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

        let expr_ty = expr.ty.to_token_stream();
        let expr_expr = expr.expr.to_token_stream();
        let method_attrs = args.attrs;
        let method_ident: TokenStream;
        let method_args: TokenStream;
        let method_ret_ty: TokenStream;
        let method_expr: TokenStream;
        match accessor.ty {
          AccessorIdent::Get { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              args.get_suf.to_string(), member_name.to_string(),
              args.get_post.to_string()).as_str()).unwrap();            
            method_args = args.receiver.map_or_else(|| quote!(&self),
              |some| some.clone());
            method_ret_ty = quote!(&#expr_ty);
            method_expr = quote!(&#expr_expr);
          },
          AccessorIdent::GetMut { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              args.get_mut_suf.to_string(), member_name.to_string(),
              args.get_mut_post.to_string()).as_str()).unwrap();           
              method_args = args.receiver.map_or_else(|| quote!(&mut self),
                |some| some.clone());
            method_ret_ty = quote!(&mut #expr_ty);
            method_expr = quote!(&mut #expr_expr);
          },
          AccessorIdent::GetCopy { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              args.get_copy_suf.to_string(), member_name.to_string(),
              args.get_copy_post.to_string()).as_str()).unwrap();           
              method_args = args.receiver.map_or_else(|| quote!(&self),
                |some| some.clone());
            method_ret_ty = quote!(#expr_ty);
            method_expr = quote!(#expr_expr.clone());
          },
          AccessorIdent::Take { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              args.take_suf.to_string(), member_name.to_string(),
              args.take_post.to_string()).as_str()).unwrap();           
              method_args = args.receiver.map_or_else(|| quote!(&mut self),
                |some| some.clone());
            method_ret_ty = quote!(#expr_ty);
            method_expr = quote!(#expr_expr);
          },
          AccessorIdent::Set { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              args.set_suf.to_string(), member_name.to_string(),
              args.set_post.to_string()).as_str()).unwrap();
            method_args = args.receiver.map_or_else(|| quote!(&mut self, value: #expr_ty), 
              |some| if some.is_empty() { 
                quote!(value: #expr_ty) 
              } else { quote!(#some, value: #expr_ty)});
            method_ret_ty = quote!(());
            method_expr = quote!(#expr_expr = value;);
          },
          AccessorIdent::ChainSet { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              args.chain_set_suf.to_string(), member_name.to_string(),
              args.chain_set_post.to_string()).as_str()).unwrap();
            method_args = args.receiver.map_or_else(|| quote!(&mut self, value: #expr_ty), 
              |some| if some.is_empty() { 
                quote!(value: #expr_ty) 
              } else { quote!(#some, value: #expr_ty)});
            method_ret_ty = quote!(&mut Self);
            method_expr = quote!(#expr_expr = value; self);
          },
          AccessorIdent::Replace { .. } => {
            method_ident = TokenStream::from_str(format!("{}{}{}", 
              args.replace_suf.to_string(), member_name.to_string(),
              args.replace_post.to_string()).as_str()).unwrap();
            method_args = args.receiver.map_or_else(|| quote!(&mut self, value: #expr_ty), 
              |some| if some.is_empty() { 
                quote!(value: #expr_ty) 
              } else { quote!(#some, value: #expr_ty)} 
            );
            method_ret_ty = quote!(#expr_ty);
            method_expr = quote!(
              let output = #expr_expr;
              #expr_expr = value ;
              output );
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
