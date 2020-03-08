import 'typeface-cairo';
import './style/default.scss';
import './style/desktop.scss';
import './style/loader.scss';

import { Elm } from './Main.elm';

Elm.Main.init({
    node: document.getElementById('elm-app')
});
