import { Elm } from "../elm/Main.elm";
import { bind } from "./web-socket";
import "../css/app.scss";

let app = Elm.Main.init({
    node: document.getElementById("app")
});

bind(app);
