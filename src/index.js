import { Elm } from "./Main.elm"

Elm.Main.init({
    node: document.querySelector("main"),
    flags: new Date().toLocaleDateString("en-us", { weekday: "long" }).toLowerCase()
})
