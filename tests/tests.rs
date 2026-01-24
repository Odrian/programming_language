mod compile_tests {
    mod common;

    #[cfg(test)]
    mod syntactic_tests;

    #[cfg(test)]
    mod primitive_tests;

    #[cfg(test)]
    mod pointer_tests;

    #[cfg(test)]
    mod big_program_tests;

    #[cfg(test)]
    mod r#struct;
    
    #[cfg(test)]
    mod r#extern;
}
