
const white = 'rgb(255,255,255)'
const dark = 'rgb(130,130,130)'
const light = 'rgb(170,170,170)'
const purple = 'rgb(137,52,235)'
const green = 'rgb(83,225,56)'

const size = 8

init()

function init() {
    const board = []
    const hover = undefined
    const lastPos = undefined
    const nextPlayer = 1
    s = { board, hover, lastPos, nextPlayer }
    document.getElementById('reset').onclick = reset(s)
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            let pos = [i,j]
            const canvas = document.createElement('canvas')
            canvas.width = 50
            canvas.height = 50
            canvas.setAttribute('class','cell')
            canvas.onclick = click(s,pos)
            canvas.onmouseover = mouseOver(s,pos)
            canvas.onmouseout = mouseOut(s,pos)
            document.body.appendChild(canvas)
            cell = { }
            cell.canvas = canvas
            cell.player = 0;
            board[i*size+j] = cell
            cell.ctx = canvas.getContext('2d')
        }
        const lineBreak = document.createElement('div')
        lineBreak.setAttribute('class','break')
        document.body.appendChild(lineBreak)
    }
    redraw(s)
}

function drawPiece(ctx,color) {
    ctx.beginPath()
    ctx.fillStyle = color
    ctx.arc (25, 25, 20, 0, 2 * Math.PI)
    ctx.fill()
}

function drawStroke(ctx,color) {
    ctx.beginPath()
    ctx.lineWidth = 4
    ctx.strokeStyle = color
    ctx.arc (25, 25, 18, 0, 2 * Math.PI)
    ctx.stroke()
}

function redraw(s) {
    const {board,lastPos,hover,nextPlayer} = s
    const lastCellName = lastPos ? cellName(lastPos) : '??'
    document.getElementById('last').textContent = lastCellName
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const cell = board[i*size+j]
            const canvas = cell.canvas
            const ctx = cell.ctx
            ctx.clearRect(0, 0, canvas.width, canvas.height);
            if ((i+j) % 2 === 0) {
                canvas.style.backgroundColor = dark
            } else {
                canvas.style.backgroundColor = light
            }
            if (cell.player === 0) {
                if (hover) {
                    if (isLegalMove(s,hover)) {
                        const [hi,hj] = hover
                        if (hi === i && hj === j) {
                            drawStroke(ctx,colourOfPlayer(s.nextPlayer))
                        }
                    }
                }
            } else {
                drawPiece(ctx,colourOfPlayer(cell.player))
            }
        }
    }
}

function colourOfPlayer(player) {
    if (player === 1) return purple
    if (player === 2) return green
    alert("colourOfPlayer!")
}

function reset(s) { return function() {
    s.lastPos = undefined
    s.hover = undefined
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const pos = [i,j]
            cellAt(s,pos).player = 0
        }
    }
    redraw(s)
}}

function mouseOver(s,pos) { return function(e) {
    s.hover = pos
    redraw(s)
}}

function mouseOut(s,pos) { return function(e) {
    s.hover = undefined
    redraw(s)
}}

function click(s,pos) { return function(e) {
    if (isLegalMove(s,pos)) {
        cellAt(s,pos).player = s.nextPlayer
        s.lastPos = pos
        s.nextPlayer = 3 - s.nextPlayer;
        redraw(s)
    }
}}


function isLegalMove(s,pos) {
    return empty(s,pos) && supported(s,pos)
}

function empty(s,pos) {
    return playerAt(s,pos) === 0
}

function supported(s,pos) {
    const [i,j] = pos
    return (pillarX(s,0,i-1,j) || pillarX(s,i+1,size-1,j) ||
            pillarY(s,i,0,j-1) || pillarY(s,i,j+1,size-1))
}

function pillarX(s,x1,x2,y) {
    if (x1 > x2) return true
    else return !empty(s,[x1,y]) && pillarX(s,x1+1,x2,y)
}

function pillarY(s,x,y1,y2) {
    if (y1 > y2) return true
    else return !empty(s,[x,y1]) && pillarY(s,x,y1+1,y2)
}

function playerAt(s,pos) {
    return cellAt(s,pos).player
}

function cellAt(s,pos) {
    const [i,j] = pos
    return s.board[i*size+j]
}

function cellName(pos) {
    const [i,j] = pos
    return String.fromCharCode(65 + j) + String(size-i)
}
