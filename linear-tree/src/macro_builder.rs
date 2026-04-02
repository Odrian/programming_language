#[macro_export]
macro_rules! build {
    // ... (elem, meta), ...
    (@seq $c:ident $elem:expr, $meta:expr; $($rest:tt)*) => {
        $c.add_elem($elem, $meta);
        build!(@seq $c $($rest)*);
    };

    // ... [ block; ... ] ...
    (@seq $c:ident [ $block:expr, $meta:expr; $($inner:tt)* ] $($rest:tt)*) => {
        $c.start_new_block();
        build!(@seq $c $($inner)*);
        $c.close_block($block, $meta);
        build!(@seq $c $($rest)*);
    };

    (@seq $c:ident) => {};

    ($($body:tt)*) => {
        {
            let mut builder = TreeBuilder::new();
            build!(@seq builder $($body)*);
            builder.finish_building()
        }
    };

}

#[macro_export]
macro_rules! build_no_meta {
    // ... [ block; ... ] ...
    (@seq $c:ident [ $block:expr; $($inner:tt)* ] $($rest:tt)*) => {
        $c.start_new_block();
        build_no_meta!(@seq $c $($inner)*);
        $c.close_block($block, Default::default());
        build_no_meta!(@seq $c $($rest)*);
    };

    // ... elem, ...
    (@seq $c:ident $elem:expr, $($rest:tt)*) => {
        $c.add_elem($elem, Default::default());
        build_no_meta!(@seq $c $($rest)*);
    };

    (@seq $c:ident) => {};

    ($($body:tt)*) => {
        {
            let mut builder = TreeBuilder::new();
            build_no_meta!(@seq builder $($body)*);
            builder.finish_building()
        }
    };
}
