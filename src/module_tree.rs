/*use std::collections::HashMap;
use std::path::PathBuf;
use crate::parser::parse3_linking::linked_statement::GlobalLinkedStatement;
use crate::parser::parse3_linking::object::Object;

#[derive(Eq, PartialEq, Hash, Copy, Clone)]
pub struct ModuleId {
    id: u32
}

pub struct ModuleMetadata {
    pub name: String,
    pub is_file: bool, // TODO: maybe use module system from rust
    pub declarations: HashMap<Object, GlobalLinkedStatement>,
}

impl ModuleMetadata {
    pub fn new(name: String, is_file: bool) -> Self {
        Self {
            name, is_file,
            declarations: HashMap::new(),
        }
    }
}

pub enum RootState {
    Done,
    Current,
    Waiting,
    NotUsed,
}

pub struct RootMetadata {
    pub path: PathBuf,
    pub state: RootState,
}

struct Node {
    parent: Option<ModuleId>,
    pub metadata: ModuleMetadata,
    childs: HashMap<String, ModuleId>,
}
impl Node {
    fn new(parent: Option<ModuleId>, metadata: ModuleMetadata) -> Self {
        Self { parent, metadata, childs: HashMap::new() }
    }
}

pub struct ModuleTree {
    roots: HashMap<String, (ModuleId, RootMetadata)>,
    nodes: Vec<Node>,
}

impl ModuleTree {
    pub fn new() -> Self {
        Self {
            roots: HashMap::new(),
            nodes: Vec::new(),
        }
    }
    fn new_node(&mut self, parent: Option<ModuleId>, metadata: ModuleMetadata) -> ModuleId {
        let new_id = u32::try_from(self.nodes.len()).expect("more than u32::MAX nodes");
        let module_id = ModuleId { id: new_id };
        self.nodes.push(Node::new(parent, metadata));
        module_id
    }
    fn get_node(&self, module_id: ModuleId) -> &Node {
        &self.nodes[module_id.id as usize]
    }
    pub fn get_node_mut(&mut self, module_id: ModuleId) -> &mut Node {
        &mut self.nodes[module_id.id as usize]
    }
    pub fn get_root(&self, string: &str) -> Option<&(ModuleId, RootMetadata)> {
        self.roots.get(string)
    }
    pub fn get_parent(&self, module_id: ModuleId) -> Option<ModuleId> {
        self.get_node(module_id).parent
    }
    pub fn get_child(&self, module_id: ModuleId, string: &str) -> Option<ModuleId> {
        self.nodes[module_id.id as usize].childs.get(string).copied()
    }
    pub fn get_metadata(&self, module_id: ModuleId) -> &ModuleMetadata {
        &self.nodes[module_id.id as usize].metadata
    }
    pub fn create_root_or_panic(&mut self, metadata: ModuleMetadata, root_metadata: RootMetadata) -> ModuleId {
        let name = metadata.name.clone();
        let module_id = self.new_node(None, metadata);
        if self.roots.insert(name, (module_id, root_metadata)).is_some() { panic!("root already exist") }
        module_id
    }
    pub fn create_child_or_panic(&mut self, module_id: ModuleId, metadata: ModuleMetadata) -> ModuleId {
        let name = metadata.name.clone();
        let module_id = self.new_node(Some(module_id), metadata);
        if self.get_node_mut(module_id).childs.insert(name, module_id).is_some() { panic!("child already exist") }
        module_id
    }
    pub fn get_full_path(&self, module_id: ModuleId) -> PathBuf {
        let mut modules = Vec::new();

        let mut module_id = module_id;
        while let Some(parent) = self.get_parent(module_id) {
            modules.push(module_id);
            module_id = parent;
        }
        let root_name = &self.get_metadata(module_id).name;

        let (_, root_metadata) = self.get_root(root_name).unwrap();
        let mut path = root_metadata.path.clone();
        for name in modules.into_iter().rev() {
            path.push(&self.get_metadata(name).name);
        }
        path
    }
}
*/