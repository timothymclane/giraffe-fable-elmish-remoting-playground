export var log = (o) => {
    console.log('object logged', new FormData(o))
    console.log('has get method', o.get)
}