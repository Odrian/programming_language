use super::context_window::ObjectContextWindow;

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[non_exhaustive]
pub struct Object {
    pub id: u32,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ObjType {
    Unit,
    Integer(IntObjType),
    Float(FloatObjType),
    Bool,
    Function { arguments: Vec<ObjType>, returns: Box<ObjType> }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum FloatObjType {
    F32, F64
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum IntObjType {
    U8, U16, U32, U64, U128,
    I8, I16, I32, I64, I128,
}

impl IntObjType {
    pub fn is_signed(&self) -> bool {
        matches!(self, IntObjType::I8 | IntObjType::I16 | IntObjType::I32 | IntObjType::I64 | IntObjType::I128)
    }
}

#[derive(Default)]
pub struct ObjectFactory {
    next_id: u32,
    array_type: Vec<ObjType>,
    array_name: Vec<String>,
}

impl ObjectFactory {
    pub fn create_object(&mut self, name: String, typee: ObjType, context: &mut ObjectContextWindow) -> Object {
        let object = Object { id: self.next_id };
        self.next_id += 1;
        self.array_type.push(typee);
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
