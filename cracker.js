let input, wordEl, word, solution;
function solveWord(word) {
    return word.split("").sort();
}

function solve() {
    if (input.value.length === 0) {
        wordEl = document.getElementById("word")
        word = wordEl.textContent;
        solution = solveWord(word);
    }

    if (wordEl) {
        applySolution(solution);
    }
}

function applySolution(solution) {
    const letter = solution[input.value.length];
    input.value += letter;
    input.dispatchEvent(new Event("input"));
}

function addKeyListener() {
    setTimeout(() => {
        input = document.getElementById("text-input");
        input.addEventListener("keydown", (e) => {
            e.preventDefault();
            solve(input);
        });
    });
}

function setup() {
    const startButton = document.getElementById("start-button");
    startButton.addEventListener("click", addKeyListener);
}

setTimeout(setup, 100);
