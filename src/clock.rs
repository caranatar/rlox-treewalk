use crate::callable::Callable;
use crate::value::Value;
use crate::eval::Eval;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Clone, Debug)]
pub struct Clock;

impl Callable for Clock {
    fn call(&self, _: &mut dyn Eval, _: Vec<crate::value::Value>) -> Result<Value, ()> {
        let dur = SystemTime::now().duration_since(UNIX_EPOCH).expect("time went backwards :(:(:(");

        let millis = dur.as_millis();
        let result = millis as f64;
        Ok(Value::Number(result))
    }
    
    fn arity(&self) -> usize {
        0
    }
}
