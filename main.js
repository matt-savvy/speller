const { Speller } = Elm;

function getHardMode() {
    const data = localStorage.getItem("hardMode");
    return data === "true";
}

function getOffset() {
    return 0;
}

const app = Speller.init({
    node: document.getElementById('elm'),
    flags: { hardMode: getHardMode(), offset: getOffset() }
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
