const { Speller } = Elm;

const app = Speller.init({
    node: document.getElementById('elm'),
});

function getAlreadyPlayed(key) {
    const item = localStorage.getItem(key);

    if (item) {
        const data = JSON.parse(item);
        return { ...data, alreadyPlayed: true }
    }

    return { alreadyPlayed: false }
}

app.ports.checkAlreadyPlayed.subscribe(function(message) {
    const alreadyPlayedMessage = getAlreadyPlayed(message);
    app.ports.messageReceiver.send(JSON.stringify(alreadyPlayedMessage))
});

app.ports.setAlreadyPlayed.subscribe(function(message) {
    const key = message.key;
    const data = JSON.stringify({ score: message.score });

    localStorage.setItem(key, data);
});

app.ports.setHardMode.subscribe(function(value) {
    localStorage.setItem("hardMode", value);
});
