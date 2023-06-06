const { Speller } = Elm;

const app = Speller.init({
    node: document.getElementById('elm'),
});

function getAlreadyPlayed() {
    return false;
}

app.ports.checkAlreadyPlayed.subscribe(function(_message) {
    const message = { alreadyPlayed: getAlreadyPlayed() }
    app.ports.messageReceiver.send(JSON.stringify(message))
});
