#[derive(Debug, PartialEq)]
pub enum Expression {
    Int(i32),
    Float(f64),
    Bool(bool),
    Add(Box<Expression>, Box<Expression>),
    Subtract(Box<Expression>, Box<Expression>),
    Multiply(Box<Expression>, Box<Expression>),
    Divide(Box<Expression>, Box<Expression>),
    If(Box<Expression>, Box<Expression>, Box<Expression>),
    LEq(Box<Expression>, Box<Expression>),
}
