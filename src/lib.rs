#![allow(unused, unused_variables)]

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
enum AccessorTy {
  Get { span: Span, },
  GetMut { span: Span, },
  GetCopy { span: Span, },
  Take { span: Span, },
  Set { span: Span, },
  ChainSet { span: Span, },
  Replace { span: Span, },
}

impl Display for AccessorTy {
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

impl Parse for AccessorTy {
  fn parse(input: ParseStream) -> Result<Self, syn::Error> {
    let ident = Ident::parse(input)?;
    Ok(
      match ident.to_string().as_str() {
        "get"       => AccessorTy::Get { span: ident.span() },
        "get_mut"   => AccessorTy::GetMut { span: ident.span() },
        "get_copy"  => AccessorTy::GetCopy { span: ident.span() },
        "take"      => AccessorTy::Take { span: ident.span() },
        "set"       => AccessorTy::Set { span: ident.span() },
        "chain_set" => AccessorTy::ChainSet { span: ident.span() },
        "replace"   => AccessorTy::Replace { span: ident.span() },
        _ => panic!(),
      }
    )
  }
}

#[derive(Debug)]
struct Accessor {
  vis: Visibility,
  constness: Option<Token![const]>,
  asyncness: Option<Token![async]>,
  unsafety: Option<Token![unsafe]>,
  ty: AccessorTy,
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

#[derive(Debug)]
struct AccessorsExpr {
  attrs: Vec<Attribute>,
  expr: ExprType,
}

impl Parse for AccessorsExpr {
  fn parse(input: ParseStream) -> Result<Self, syn::Error>  {
    Ok(AccessorsExpr { 
      attrs: input.call(Attribute::parse_outer)?,
      expr: input.parse()?,
    })
  }
}

#[derive(Debug)]
struct ItemAccessors {
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

impl Parse for ItemAccessors {
  fn parse(input: ParseStream) -> Result<Self, syn::Error> {
    let brackets_content;
    let braces_content;
    Ok(ItemAccessors {
      accessors_vis: input.parse()?,
      accessors_constness: input.parse().ok(),
      accessors_asyncness: input.parse().ok(),
      accessors_unsafety: input.parse().ok(),
      bracket_token: bracketed!(brackets_content in input),
      accessors: brackets_content.parse_terminated(Accessor::parse)?,
      for_token: input.parse()?,
      brace_token: braced!(braces_content in input),
      exprs: braces_content.parse_terminated(AccessorsExpr::parse)?
    })
  }
}

#[proc_macro]
pub fn generate_accessors(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
  let item = parse_macro_input!(tokens as ItemAccessors);
  let mut output = TokenStream::new();

  for accessor in item.accessors.iter() {
    for expr in item.exprs.iter() {
      // TODO: make var names match those of ImplItemMacro
      let mut member_ident_override = Option::<Ident>::None;
      let mut method_receiver_override = Option::<Receiver>::None;
      for attr in &expr.attrs {
        match attr.path.to_token_stream().to_string().as_str() {
          "name" => {
            assert!(member_ident_override.is_none(), "name attribute declared twice for expr");
            let mut paren_attr_args = attr.tokens.to_string();
            paren_attr_args.pop();
            paren_attr_args.remove(0);
            member_ident_override = Some(parse2::<Ident>(
              TokenStream::from_str(paren_attr_args.as_str()).unwrap()).unwrap());
          },
          "receiver" => {
            assert!(method_receiver_override.is_none(), 
              "receiver attribute declared twice for expr");
            let mut paren_attr_args = attr.tokens.to_string();
            paren_attr_args.pop();
            paren_attr_args.remove(0);
            method_receiver_override = Some(parse2::<Receiver>(
              TokenStream::from_str(paren_attr_args.as_str()).unwrap()).unwrap());
          },
          _ => (),
        }
      }

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

      let member_ident = member_ident_override.unwrap_or_else(|| {
        return match expr.expr.expr.borrow() {
          Expr::Field(expr_field) => {
            return match &expr_field.member {
              Member::Named(member_named) => member_named.clone(),
              Member::Unnamed(_) => panic!(),
            }
          },
          _ => panic!(),
        }
      });

      let method_receiver: TokenStream;
      let method_ident: TokenStream;
      let method_named_args: TokenStream;
      let method_ret_ty: TokenStream;
      let method_expr: TokenStream;
      let expr_ty = &expr.expr.ty;
      let expr_expr = &expr.expr.expr;
      match accessor.ty {
        AccessorTy::Get { .. } => {
          method_ident = member_ident.to_token_stream();
          method_receiver = method_receiver_override.map_or(
            quote!(&self), |some| some.to_token_stream());
          method_named_args = TokenStream::new();
          method_ret_ty = quote!(&#expr_ty);
          method_expr = quote!(&#expr_expr);
        },
        AccessorTy::GetMut { .. } => {
          method_ident = format_ident!("{}_mut", member_ident).to_token_stream();
          method_receiver = method_receiver_override.map_or(
            quote!(&mut self), |some| some.to_token_stream());
          method_named_args = TokenStream::new();
          method_ret_ty = quote!(&mut #expr_ty);
          method_expr = quote!(&mut #expr_expr);
        },
        AccessorTy::GetCopy { .. } => {
          method_ident = member_ident.to_token_stream();
          method_receiver = method_receiver_override.map_or(
            quote!(&self), |some| some.to_token_stream());
          method_named_args = TokenStream::new();
          method_ret_ty = quote!(#expr_ty);
          method_expr = quote!(#expr_expr.clone());
        },
        AccessorTy::Take { .. } => {
          method_ident = format_ident!("take_{}", member_ident).to_token_stream();
          method_receiver = method_receiver_override.map_or(
            quote!(&mut self), |some| some.to_token_stream());
          method_named_args = TokenStream::new();
          method_ret_ty = quote!(#expr_ty);
          method_expr = quote!(#expr_expr);
        },
        AccessorTy::Set { .. } => {
          method_ident = format_ident!("set_{}", member_ident).to_token_stream();
          method_receiver = method_receiver_override.map_or(
            quote!(&mut self), |some| some.to_token_stream());
          method_named_args = quote!(value: #expr_ty);
          method_ret_ty = quote!(());
          method_expr = quote!(#expr_expr = value;);
        },
        AccessorTy::ChainSet { .. } => {
          method_ident = format_ident!("set_{}", member_ident).to_token_stream();
          method_receiver = method_receiver_override.map_or(
            quote!(&mut self), |some| some.to_token_stream());
          method_named_args = quote!(value: #expr_ty);
          method_ret_ty = quote!(&mut Self);
          method_expr = quote!(
            #expr_expr = value;
            self
          );
        },
        AccessorTy::Replace { .. } => {
          method_ident = format_ident!("replace_{}", member_ident).to_token_stream();
          method_receiver = method_receiver_override.map_or(
            quote!(&mut self), |some| some.to_token_stream());
          method_named_args = quote!(value: #expr_ty);
          method_ret_ty = quote!(#expr_ty);
          method_expr = quote!(
            let output = #expr_expr;
            #expr_expr = value;
            output
          );
        },
      };

      output = quote!(
        #output
        #method_modifiers fn #method_ident (#method_receiver, #method_named_args) 
          -> #method_ret_ty
        {
          #method_expr
        }
      );
    }
  }

  output.into()
}
