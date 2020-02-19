
const black = 'rgb(0,0,0)'
const grey = 'rgb(200,200,200)'

const size = 8

let board = [];

init()
reset()

function init() {
    for(let i = 0; i < size ; i++) {
        board[i] = [];
        for(let j = 0; j < size; j++) {
            const cell = document.createElement('div');
            cell.setAttribute('class','cell')
            cell.onclick = changeBgCol;
            board[i][j] = cell;
            document.body.appendChild(cell);
        }
        const lineBreak = document.createElement('div');
        lineBreak.setAttribute('class','break')
        document.body.appendChild(lineBreak);
    }
    document.getElementById('reset').onclick = reset;
}

function reset() {
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            cell = board[i][j]
            if ((i+j) % 2 === 0) {
                cell.style.backgroundColor = black
            } else {
                cell.style.backgroundColor = grey
            }
        }
    }
}

function changeBgCol(e) {
    const col = 'rgb(' + random(255) + ',' + random(255) + ',' + random(255) + ')';
    e.target.style.backgroundColor = col;
}

function random(number) {
    return Math.floor(Math.random() * number);
}
