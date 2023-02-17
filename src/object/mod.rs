type ObjectType = String;

trait Object {
    fn obtype() -> ObjectType;
    fn inspect() -> String;
}
