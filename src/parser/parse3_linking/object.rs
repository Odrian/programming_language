
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[non_exhaustive]
pub struct Object {
    pub id: u32,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ObjType {
    Unknown,
    Void,
    Char,
    Integer(IntObjType),
    Float(FloatObjType),
    Reference(Box<Self>),
    Function { arguments: Vec<Self>, returns: Box<Self> },
}
impl ObjType {
    pub const BOOL: Self = Self::Integer(IntObjType::Bool);
    pub const DEFAULT_INTEGER: Self = Self::Integer(IntObjType::I32);
    pub const DEFAULT_FLOAT: Self = Self::Float(FloatObjType::F64);

    pub fn is_void(&self) -> bool {
        self == &Self::Void
    }
    pub const fn get_float_bits_or_panic(&self) -> u8 {
        let Self::Float(float) = self else {
            panic!();
        };
        match float {
            FloatObjType::F32 => 4,
            FloatObjType::F64 => 8,
        }
    }
    pub fn unwrap_ref(&self) -> &Self {
        let Self::Reference(obj_type) = self else {
            panic!()
        };
        obj_type.as_ref()
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
    pub const fn is_signed(&self) -> bool {
        matches!(self, Self::I8 | Self::I16 | Self::I32 | Self::I64 | Self::I128 | Self::ISize)
    }
    pub fn is_bool(&self) -> bool {
        self == &Self::Bool
    }
}

#[derive(Debug, Default)]
pub struct ObjectFactory {
    next_id: u32,
    array_type: Vec<ObjType>,
    array_name: Vec<String>,
}

impl ObjectFactory {
    pub fn create_object(&mut self, name: String, object_type: ObjType) -> Object {
        let object = Object { id: self.next_id };
        self.next_id += 1;
        self.array_type.push(object_type);
        self.array_name.push(name.clone());
        object
    }
    pub fn get_type(&self, object: Object) -> &ObjType {
        &self.array_type[object.id as usize]
    }
    pub fn get_type_mut(&mut self, object: Object) -> &mut ObjType {
        &mut self.array_type[object.id as usize]
    }
    pub fn get_name(&self, object: Object) -> &String {
        &self.array_name[object.id as usize]
    }
}
