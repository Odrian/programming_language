use crate::parser::parse1_tokenize::token::TwoSidedOperation;
use super::context_window::ObjectContextWindow;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct Object<'text> {
    pub id: u32,
    pub name: &'text [char],
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ObjType {
    Unit,
    Number,
    Function { arguments: Vec<ObjType>, returns: Box<ObjType> }
}

#[derive(Default)]
pub struct ObjectFactory {
    next_id: u32,
    array: Vec<ObjType>,
}

impl ObjectFactory {
    pub fn create_object<'text>(&mut self, name: &'text [char], typee: ObjType, context: &mut ObjectContextWindow<'text>) -> Object<'text> {
        let object = Object { id: self.next_id, name };
        self.next_id += 1;
        self.array.push(typee);
        context.add(object);
        object
    }
    pub fn get_type(&self, object: Object) -> &ObjType {
        &self.array[object.id as usize]
    }
}

impl ObjType {
    pub fn from_two_op(type1: &ObjType, type2: &ObjType, two_sided_operation: &TwoSidedOperation) -> Option<ObjType> {
        match two_sided_operation {
            TwoSidedOperation::Plus | TwoSidedOperation::Minus => {
                if type1 == &ObjType::Number && type2 == &ObjType::Number {
                    Some(ObjType::Number)
                } else {
                    None
                }
            }
        }
    }
}
