
const black = 'rgb(0,0,0)'
const white = 'rgb(255,255,255)'
const dark = 'rgb(130,130,130)'
const light = 'rgb(170,170,170)'
const purple = 'rgb(137,52,235)'
const green = 'rgb(83,225,56)'

const size = 8
const winLineLength = 4

init()

function init() {
    const hover = undefined
    const lastPos = undefined
    const nextPlayer = 1
    const winByLastPlayer = false
    const moveNumber = 1
    const board = []
    s = { hover, lastPos, nextPlayer, winByLastPlayer, moveNumber, board }
    document.getElementById('reset').onclick = reset(s)
    gridTag = document.getElementById('GridTag')
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            let pos = [i,j]
            const canvas = document.createElement('canvas')
            gridTag.appendChild(canvas)
            canvas.width = 50
            canvas.height = 50
            canvas.setAttribute('class','cell')
            canvas.onclick = click(s,pos)
            canvas.onmouseover = mouseOver(s,pos)
            canvas.onmouseout = mouseOut(s,pos)
            if ((i+j) % 2 === 0) {
                canvas.style.backgroundColor = dark
            } else {
                canvas.style.backgroundColor = light
            }
            cell = { }
            board[i*size+j] = cell
            cell.player = 0;
            cell.canvas = canvas
            cell.ctx = canvas.getContext('2d')
        }
        const lineBreak = document.createElement('div')
        lineBreak.setAttribute('class','break')
        gridTag.appendChild(lineBreak)
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

function redrawStatus(s) {
    const {nextPlayer,winByLastPlayer} = s
    const p = document.getElementById('Status')
    if (gameOver(s)) {
        if (winByLastPlayer) {
            lastPlayer = otherPlayer(nextPlayer)
            p.style.color = colourOfPlayer(lastPlayer)
            p.textContent = 'Game Over. Player-' + String(lastPlayer) + ' wins!'
        } else {
            p.style.color = black
            p.textContent = 'Game Over. Draw'
        }
    } else {
        p.style.color = colourOfPlayer(nextPlayer)
        p.textContent = 'Player-' + String(nextPlayer) + ' to move.'
    }
}

function redrawLastCellName(s) {
    const {lastPos} = s
    const p = document.getElementById('Last')
    p.textContent = lastPos ? cellName(lastPos) : '??'

}

function redraw(s) {
    const {hover, nextPlayer, board} = s
    redrawLastCellName(s)
    redrawStatus(s)
    const finished = gameOver(s)
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const cell = board[i*size+j]
            const canvas = cell.canvas
            const ctx = cell.ctx
            ctx.clearRect(0, 0, canvas.width, canvas.height);
            if (cell.player === 0) {
                if (hover && !finished) {
                    if (isLegalMove(s,hover)) {
                        const [hi,hj] = hover
                        if (hi === i && hj === j) {
                            drawStroke(ctx,colourOfPlayer(nextPlayer))
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
    s.hover = undefined
    s.lastPos = undefined
    s.nextPlayer = 1
    s.winByLastPlayer = false
    s.moveNumber = 1
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
    const { nextPlayer } = s
    if (isLegalMove(s,pos)) {
        s.moveNumber ++
        cellAt(s,pos).player = nextPlayer
        s.lastPos = pos
        s.nextPlayer = otherPlayer(nextPlayer);
        s.winByLastPlayer = isWinline(s,pos,nextPlayer)
        redraw(s)
    }
}}

function otherPlayer(player) {
    return 3 - player
}

function isLegalMove(s,pos) {
    return !gameOver(s) && empty(s,pos) && supported(s,pos)
}

function gameOver(s) {
    return s.winByLastPlayer || s.moveNumber > size*size
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

function isWinline(s,pos,player) {
    return (isWinlineDir(s,pos,player,[0,1]) ||
            isWinlineDir(s,pos,player,[1,0]) ||
            isWinlineDir(s,pos,player,[1,1]) ||
            isWinlineDir(s,pos,player,[1,-1]))
}

function isWinlineDir(s,pos,player,dir) {
    const a = lengthPlayerLineDir(s,pos,player,dir)
    const b = lengthPlayerLineDir(s,pos,player,oppositeDir(dir))
    return 1+a+b >= winLineLength
}

function lengthPlayerLineDir(s,pos,player,dir) {
    const pos2 = stepDir(pos,dir)
    if (offBoard(pos2)) return 0
    if (playerAt(s,pos2) === player)
        return 1 + lengthPlayerLineDir(s,pos2,player,dir)
    else
        return 0
}

function offBoard(pos) {
    let [i,j] = pos
    return i < 0 || i >= size || j < 0 || j >= size
}

function oppositeDir(dir) {
    let [di,dj] = dir
    return [-di,-dj]
}

function stepDir(pos,dir) {
    let [i,j] = pos
    let [di,dj] = dir
    return [i+di,j+dj]
}
