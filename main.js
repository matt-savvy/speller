const { Speller } = Elm;

const app = Speller.init({
    node: document.getElementById('elm'),
});

app.ports.checkAlreadyPlayed.subscribe(function(_message) {
    app.ports.messageReceiver.send(JSON.stringify(""))
});
