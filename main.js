const { Speller } = Elm;

const app = Speller.init({
    node: document.getElementById('elm'),
});

function getAlreadyPlayed(key) {
    const item = localStorage.getItem(key);

    if (item) {
        return true;
    }

    return false
}

app.ports.checkAlreadyPlayed.subscribe(function(message) {
    const alreadyPlayedMessage = { alreadyPlayed: getAlreadyPlayed(message) }
    app.ports.messageReceiver.send(JSON.stringify(alreadyPlayedMessage))
});

app.ports.setAlreadyPlayed.subscribe(function(message) {
    localStorage.setItem(message, true);
});
