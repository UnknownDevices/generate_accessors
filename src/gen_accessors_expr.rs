use std::borrow::Borrow;
use syn::parse::{Parse, ParseStream};
use syn::token::Colon;
use syn::{Expr, Type, Token, ExprType, Member};
use crate::gen_accessors_attr::GenAccessorsAttr;

#[derive(Debug)]
pub struct GenAccessorsExpr {
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

    Ok(GenAccessorsExpr { attrs, expr, colon_token, ty })
  }
}

impl GenAccessorsExpr {
  fn deduce_name_intern(expr: &Expr) -> String {
    return match expr {
      Expr::Field(expr_field) => {
        match &expr_field.member {
          Member::Named(member_named) => member_named.to_string(),
          Member::Unnamed(_) => panic!(),
        }
      },
      Expr::Path(expr_path) => {
        expr_path.path.segments.last().unwrap().ident.to_string()
      },
      Expr::MethodCall(expr_method_call) => {
        expr_method_call.method.to_string()
      },
      Expr::Try(expr_try) => {
        Self::deduce_name_intern(expr_try.expr.borrow())
      },
      Expr::Call(expr_call) => {
        Self::deduce_name_intern(expr_call.func.borrow())
      },
      Expr::Unary(expr_unary) => {
        Self::deduce_name_intern(expr_unary.expr.borrow())
      },
      Expr::Cast(expr_cast) => {
        Self::deduce_name_intern(expr_cast.expr.borrow())
      },
      Expr::Reference(expr_reference) => {
        Self::deduce_name_intern(expr_reference.expr.borrow())
      },
      Expr::Box(expr_box) => {
        Self::deduce_name_intern(expr_box.expr.borrow())
      },
      Expr::Binary(expr_binary) => {
        Self::deduce_name_intern(expr_binary.left.borrow())
      },
      Expr::Paren(expr_paren) => {
        Self::deduce_name_intern(expr_paren.expr.borrow())
      }
      _ => panic!(),
    }
  }

  pub fn deduce_name(&self) -> String {
    Self::deduce_name_intern(self.expr.borrow())
  }
}