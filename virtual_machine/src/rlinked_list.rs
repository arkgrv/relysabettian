use std::{cell::RefCell, rc::Rc, ops::Deref};

use compiler::common::zero_init;

/// Linked List node implementation
pub struct RNode<T> {
    pub payload: Box<T>,
    pub next: Option<Rc<RefCell<RNode<T>>>>,
}

impl <T> RNode<T> {
    /// Creates a new node with given parameters
    /// 
    /// Parameters:
    /// * `payload`: value of this node
    /// * `next`: reference to next node
    pub fn new(payload: T, next: Option<Rc<RefCell<Self>>>) -> Option<Rc<RefCell<Self>>> {
        let node = RNode {
            payload: Box::new(payload),
            next: next.clone(),
        };

        Some(Rc::new(RefCell::new(node)))
    }

    /// Creates a new empty node
    pub fn new_empty() -> Option<Rc<RefCell<Self>>> {
        let node = RNode {
            payload: zero_init(),
            next: None,
        };

        Some(Rc::new(RefCell::new(node)))
    }
}

/// Linked List type definition
pub type RLinkedList<T> = Option<Rc<RefCell<RNode<T>>>>;

/// Inserts a new node as head of the list
/// 
/// Parameters:
/// * `list`: old list
/// * `value`: value to insert
pub fn rl_insert_head<T>(list: RLinkedList<T>, value: T) -> RLinkedList<T> {
    let new_list = RNode::<T>::new(value, list.clone());
    new_list
}

pub fn rl_insert_tail<T>(list: RLinkedList<T>, value: T) -> RLinkedList<T> {
    if list.is_none() {
        return rl_insert_head(list, value);
    }

    let old_list = list.clone();
    let mut start = list.clone();

    while start.is_some() && start.unwrap().deref().borrow().next.is_some() {
        start = start.unwrap().deref().borrow().next;
    }

    let mut new_list = RNode::<T>::new_empty();
    new_list.unwrap().deref().borrow_mut().next = None;
    new_list.unwrap().deref().borrow_mut().payload = Box::new(value);

    start.unwrap().deref().borrow_mut().next = new_list;

    old_list
}
