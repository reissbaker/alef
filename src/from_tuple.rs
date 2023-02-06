#[macro_export]
macro_rules! from_tuple {
    ($l:expr, $r:expr)  => ( ($l, $r) );
    ($l:expr, $r:expr,) => ( ($l, $r) );
    ($o:expr, $($e:expr),*)  => ( ($o, $crate::from_tuple!($($e),*)) );
    ($o:expr, $($e:expr),*,) => ( ($o, $crate::from_tuple!($($e),*)) );
}
