
const black = 'rgb(0,0,0)'
const white = 'rgb(255,255,255)'
const dark = 'rgb(140,140,140)'
const light = 'rgb(160,160,160)'
const purple = 'rgb(137,52,235)'
const green = 'rgb(83,225,56)'

const size = 8
const winLineLength = 4

init()

function init() {
    const s = createState()
    const moves = JSON.parse(localStorage.getItem('SavedMoves'))
    if (moves) setup(s,moves)
    redraw(s)
}

function saveState(s) {
    localStorage.setItem('SavedMoves',JSON.stringify(s.moves))
}

function setup(s,moves) {
    for(let i = 0; i < moves.length ; i++) {
        const pos = moves[i]
        moveAtPosition(s,pos)
    }
}

function createState() {
    const hover = undefined
    const nextPlayer = 1
    const winByLastPlayer = false
    const board = []
    const moves = []
    s = { hover, nextPlayer, winByLastPlayer, board, moves }
    document.getElementById('Reset').onclick = reset(s)
    document.getElementById('Undo').onclick = undoLastMove(s)
    gridTag = document.getElementById('GridTag')
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const pos = [i,j]
            const canvas = document.createElement('canvas')
            gridTag.appendChild(canvas)
            canvas.width = 50
            canvas.height = 50
            canvas.setAttribute('class','cell')
            canvas.onclick = moveAtPositionAndUpdate(s,pos)
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
    return s
}

function reset(s) { return function() {
    s.hover = undefined
    s.nextPlayer = 1
    s.winByLastPlayer = false
    s.moves = []
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const pos = [i,j]
            cellAt(s,pos).player = 0
        }
    }
    redraw(s)
    saveState(s)
}}

function drawPiece(ctx,color) {
    ctx.beginPath()
    ctx.fillStyle = color
    ctx.arc (25, 25, 20, 0, 2 * Math.PI)
    ctx.fill()
}

function drawCircle(ctx,color,thickness) {
    const size = (25*4/5) - thickness/2
    ctx.beginPath()
    ctx.lineWidth = thickness
    ctx.strokeStyle = color
    ctx.arc (25, 25, size, 0, 2 * Math.PI)
    ctx.stroke()
}

function drawCircleSolid(ctx,color,thickness) {
    ctx.setLineDash([])
    drawCircle(ctx,color,thickness)
}

function drawCircleDashed(ctx,color,thickness) {
    ctx.setLineDash([1,2])
    drawCircle(ctx,color,thickness)
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

function createMoveText(player,pos) {
    const p = document.createElement('span')
    p.style.color = colourOfPlayer(player)
    p.textContent = ' ' + cellName(pos)
    return p
}

function removeAllChildren(node) {
    while(node.lastChild) {
        node.removeChild(node.lastChild)
    }
}

function redrawMoveList(s) {
    const p = document.getElementById('Moves')
    removeAllChildren(p)
    var player = 1
    for (let i = 0; i < s.moves.length; i++) {
        const pos = s.moves[i]
        p.appendChild(createMoveText(player,pos))
        player = otherPlayer(player)
    }
}

function redraw(s) {
    const {hover, nextPlayer, board} = s
    redrawStatus(s)
    redrawMoveList(s)
    const finished = gameOver(s)
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const pos = [i,j]
            const cell = board[i*size+j]
            const canvas = cell.canvas
            const ctx = cell.ctx
            ctx.clearRect(0, 0, canvas.width, canvas.height);
            if (cell.player === 0) {
                if (!finished) {
                    if (isLegalMove(s,pos)) {
                        if (hover && eqPos(pos,hover)) {
                            drawCircleSolid(ctx,colourOfPlayer(nextPlayer),4)
                        } else {
                            drawCircleDashed(ctx,white,1)
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

function mouseOver(s,pos) { return function() {
    s.hover = pos
    redraw(s)
}}

function mouseOut(s,pos) { return function() {
    s.hover = undefined
    redraw(s)
}}

function moveAtPositionAndUpdate(s,pos) { return function() {
    moveAtPosition(s,pos)
    redraw(s)
    saveState(s)
}}

function moveAtPosition(s,pos) {
    const { moves, nextPlayer } = s
    if (isLegalMove(s,pos)) {
        cellAt(s,pos).player = nextPlayer
        s.nextPlayer = otherPlayer(nextPlayer);
        s.winByLastPlayer = isWinline(s,pos,nextPlayer)
        s.moves.push(pos)
    }
}

function undoLastMove(s) { return function() {
    const { moves, nextPlayer } = s
    if (s.moves.length > 0) {
        const pos = s.moves.pop()
        cellAt(s,pos).player = 0
        s.nextPlayer = otherPlayer(nextPlayer);
        s.winByLastPlayer = false
        redraw(s)
        saveState(s)
    }
}}


function allLegalMoves(s) {
    var acc = []
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const pos = [i,j]
            if (isLegalMove(s,pos)) {
                acc.push(pos)
            }
        }
    }
}

function isLegalMove(s,pos) {
    return !gameOver(s) && empty(s,pos) && supported(s,pos)
}

function gameOver(s) {
    return s.winByLastPlayer || s.moves.length === size*size
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
    return String.fromCharCode(97 + j) + String(size-i)
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
    const [i,j] = pos
    return i < 0 || i >= size || j < 0 || j >= size
}

function oppositeDir(dir) {
    const [di,dj] = dir
    return [-di,-dj]
}

function stepDir(pos,dir) {
    const [i,j] = pos
    const [di,dj] = dir
    return [i+di,j+dj]
}

function eqPos(pos1,pos2) {
    const [i1,j1] = pos1
    const [i2,j2] = pos2
    return i1===i2 && j1===j2
}

function otherPlayer(player) {
    return 3 - player
}
