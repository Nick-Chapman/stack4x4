
const white = 'rgb(255,255,255)'
const dark = 'rgb(130,130,130)'
const light = 'rgb(170,170,170)'
const purple = 'rgb(137,52,235)'
const green = 'rgb(83,225,56)'

const size = 8

let board = []
let lastCell = '??'
let nextPlayer = 1
let hover

function init() {

    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const canvas = document.createElement('canvas')
            canvas.width = 50
            canvas.height = 50
            canvas.setAttribute('class','cell')
            canvas.onclick = onClick(i,j)
            canvas.onmouseover = onMouseOver(i,j)
            canvas.onmouseout = onMouseOut(i,j)
            document.body.appendChild(canvas)
            cell = { }
            cell.canvas = canvas
            cell.player = 0;
            cell.name = cellName(i,j)
            board[i*size+j] = cell
            cell.ctx = canvas.getContext('2d')
        }
        const lineBreak = document.createElement('div')
        lineBreak.setAttribute('class','break')
        document.body.appendChild(lineBreak)
    }
    document.getElementById('reset').onclick = reset
}

function onMouseOver(i,j) { return function(e) { hover = [i,j]; redraw()} }
function onMouseOut(i,j) { return function(e) { hover = undefined; redraw()} }

function reset() {
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            let cell = board[i*size+j]
            cell.player = 0
        }
    }
    hover = undefined
    redraw()
}

function drawPiece(ctx,color) {
    ctx.beginPath()
    ctx.fillStyle = color
    ctx.arc (25, 25, 20, 0, 2 * Math.PI)
    ctx.fill()
}

function redraw() {
    document.getElementById('last').textContent = lastCell
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            let cell = board[i*size+j]
            const canvas = cell.canvas
            const ctx = cell.ctx
            ctx.clearRect(0, 0, canvas.width, canvas.height);
            if ((i+j) % 2 === 0) {
                canvas.style.backgroundColor = dark
            } else {
                canvas.style.backgroundColor = light
            }
            if (cell.player === 1) {
                drawPiece(ctx,purple)
            }
            if (cell.player === 2) {
                drawPiece(ctx,green)
            }
            if (hover) {
                let [hi,hj] = hover
                if (hi === i && hj === j) {
                    drawPiece(ctx,white)
                }
            }
        }
    }
}

function cellName(i,j) {
    return String.fromCharCode(65 + j) + String(size-i)
}

function onClick(i,j) { return function(e) {
    let cell = board[i*size+j]
    cell.player = nextPlayer
    lastCell = cellName(i,j)
    nextPlayer = 3-nextPlayer;
    redraw()
}}

init()
reset()
