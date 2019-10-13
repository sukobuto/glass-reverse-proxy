import { Elm } from "../elm/Main.elm";
import { bind } from "./web-socket";

let app = Elm.Main.init({
    node: document.getElementById("app")
});

bind(app);
