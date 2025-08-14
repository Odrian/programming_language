use super::context_window::ObjectContextWindow;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct Object {
    pub id: u32,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ObjType {
    Unit,
    Char,
    Integer(IntObjType),
    Float(FloatObjType),
    Function { arguments: Vec<ObjType>, returns: Box<ObjType> }
}
impl ObjType {
    pub const BOOL: ObjType = ObjType::Integer(IntObjType::Bool);
    pub const DEFAULT_INTEGER: ObjType = ObjType::Integer(IntObjType::I32);
    pub const DEFAULT_FLOAT: ObjType = ObjType::Float(FloatObjType::F64);
    
    pub fn get_float_bits_or_panic(&self) -> u8 {
        let ObjType::Float(float) = self else {
            panic!();
        };
        match float {
            FloatObjType::F32 => 4,
            FloatObjType::F64 => 8,
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FloatObjType {
    F32, F64
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum IntObjType {
    Bool,
    U8, U16, U32, U64, U128, USize,
    I8, I16, I32, I64, I128, ISize,
}

impl IntObjType {
    pub fn is_signed(&self) -> bool {
        matches!(self, IntObjType::I8 | IntObjType::I16 | IntObjType::I32 | IntObjType::I64 | IntObjType::I128 | IntObjType::ISize)
    }
    pub fn is_bool(&self) -> bool {
        self == &IntObjType::Bool
    }
}

#[derive(Default)]
pub struct ObjectFactory {
    next_id: u32,
    array_type: Vec<ObjType>,
    array_name: Vec<String>,
}

impl ObjectFactory {
    pub fn create_object(&mut self, name: String, object_type: ObjType, context: &mut ObjectContextWindow) -> Object {
        let object = Object { id: self.next_id };
        self.next_id += 1;
        self.array_type.push(object_type);
        self.array_name.push(name.clone());
        context.add(name, object);
        object
    }
    pub fn get_type(&self, object: Object) -> &ObjType {
        &self.array_type[object.id as usize]
    }
    pub fn get_name(&self, object: Object) -> &String {
        &self.array_name[object.id as usize]
    }
}
