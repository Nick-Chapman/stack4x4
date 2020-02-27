
const black = 'rgb(0,0,0)'
const white = 'rgb(255,255,255)'
const dark = 'rgb(140,140,140)'
const light = 'rgb(160,160,160)'
const purple = 'rgb(137,52,235)'
const green = 'rgb(25,170,25)'

const size = 8
const winLineLength = 4

const aiMoveHighlightTime = 100 // flash white circle

const timeLimits = [ 0, 300, 700, 1000, 2000, 3000, 5000, 8000, 13000, 21000, 34000 ]
const initTimeLimitIndex = 3

function timeLimit(s) {
    return timeLimits[s.timeLimitIndex]
}

function pauseThen(ms,f) {
    setTimeout(f,ms)
}

function saveState(s) {
    localStorage.setItem('SavedMoves',JSON.stringify(s.moves))
}

function init() {
    const s = newState()
    setupDOM(s)
    const moves = JSON.parse(localStorage.getItem('SavedMoves'))
    if (moves) {
        s.restoring = true
        lockoutHuman(s,true)
        restoreState(s,moves,0)
    }
}

function restoreState(s,moves,i) {
    if (i < moves.length) {
        const totalDuration = 200 * Math.sqrt(moves.length)
        const duration = totalDuration / moves.length
        pauseThen(duration,() => {
            const pos = moves[i]
            moveAtPosition(s,pos)
            redraw(s)
            restoreState(s,moves,i+1)
        })
    } else {
        s.restoring = false
        lockoutHuman(s,false)
        endOfInteraction(s)
    }
}

// we only disable those button which affect the board state
const buttonsToDisable = ['NewGame','Undo','Undo2']

// we call lockoutHuman when the AI is running or during initial restore
// we disable the UI and mark that the AI is running
function lockoutHuman(s,bool) {
    for (let i = 0; i < buttonsToDisable.length; i++) {
        const name = buttonsToDisable[i]
        document.getElementById(name).disabled = bool
    }
}

const canvasSize = 100

function setupDOM(s) {
    document.getElementById('TimeLimit').onclick = cycleTimeLimit(s)
    document.getElementById('NewGame').onclick = newGame(s)
    document.getElementById('Undo').onclick = undoLastMoveAndUpdate(s)
    document.getElementById('Undo2').onclick = undoTwoLastMovesAndUpdate(s)
    document.getElementById('Player1switch').onclick = switchP1(s)
    document.getElementById('Player2switch').onclick = switchP2(s)
    document.getElementById('SwitchPlayers').onclick = switchBothPlayers(s)
    document.getElementById('StopAI').onclick = stopAI(s)
    gridTag = document.getElementById('GridTag')
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const pos = [i,j]
            const canvas = document.createElement('canvas')
            gridTag.appendChild(canvas)
            canvas.width = canvasSize
            canvas.height = canvasSize
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
            s.board[i*size+j] = cell
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

function mouseOver(s,pos) { return function() {
    if (!s.aiRunning && !s.restoring) {
        s.hover = pos
        redraw(s)
    }
}}

function mouseOut(s,pos) { return function() {
    if (!s.aiRunning && !s.restoring) {
        s.hover = undefined
        redraw(s)
    }
}}

function cycleTimeLimit(s) { return function() {
    s.timeLimitIndex = (s.timeLimitIndex + 1) % timeLimits.length
    endOfInteraction(s)
}}

function newGame(s) { return function() {
    resetState(s)
    endOfInteraction(s)
}}

function undoLastMoveAndUpdate(s) { return function() {
    undoLastMove(s)
    endOfInteraction(s)
}}

function undoTwoLastMovesAndUpdate(s) { return function() {
    undoLastMove(s)
    undoLastMove(s)
    endOfInteraction(s)
}}

function switchP1(s) { return function() {
    s.player1isAI = !s.player1isAI
    endOfInteraction(s)
}}

function switchP2(s) { return function() {
    s.player2isAI = !s.player2isAI
    endOfInteraction(s)
}}

function switchBothPlayers(s) { return function() {
    s.player1isAI = !s.player1isAI
    s.player2isAI = !s.player2isAI
    endOfInteraction(s)
}}

function stopAI(s) { return function() {
    s.stop = true
    endOfInteraction(s)
}}

function moveAtPositionAndUpdate(s,pos) { return function() {
    // make an explicit check because there is no button to disable
    if (!s.aiRunning && !s.restoring) {
        moveAtPosition(s,pos)
        endOfInteraction(s)
    }
}}

// This is called even if the AI is running
function endOfInteraction(s) {
    redraw(s)
    saveState(s) //Should this be done here?
    maybeRunAI(s)
}

function maybeRunAI(s) {
    if (!s.aiRunning && !s.restoring && isPlayerAI(s,s.nextPlayer) && !gameOver(s)) {
        s.aiRunning = true
        document.getElementById('StopAI').textContent = 'Stop AI'
        lockoutHuman(s,true)
        pauseThen(100,() => {
            var timeoutAlive = true
            setTimeout(() => {
                if (timeoutAlive) {
                    s.stop = true
                }
            },timeLimit(s))
            chooseMoveAI(s,pos => {
                timeoutAlive = false
                s.hover = pos
                redraw(s)
                pauseThen(aiMoveHighlightTime,() => {
                    s.hover = undefined
                    moveAtPosition(s,pos)
                    s.aiRunning = false
                    lockoutHuman(s,false)
                    document.getElementById('StopAI').textContent = ''
                    endOfInteraction(s)
                })
            })
        })
    }
}

function cloneBoardCells(b0) {
    const b = []
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const cell0 = b0[i*size+j]
            cell = {}
            cell.player = cell0.player
            b[i*size+j] = cell
        }
    }
    return b
}

function chooseMoveAI(s,k) {
    // TODO: Properly distiguish Game state from UI state
    g = {}
    g.nextPlayer = s.nextPlayer
    g.moves = s.moves.slice(0) //array of immutable position objects
    g.board = cloneBoardCells(s.board) //array of mutable cells
    g.winByLastPlayer = s.winByLastPlayer
    s.movesConsidered = 0
    return candidateMovesAI(s,g, (rationale,weighted) => {
        console.log(s.movesConsidered, rationale, weighted.map(([p,w]) => cellName(p)+'-'+w))
        return k(randomWeightedPick(weighted))
    })
}

function randomWeightedPick(weighted) {
    const ws = weighted.map(([_,w]) => w)
    const moves = weighted.map(([m,_]) => m)
    function add(x,y) { return x + y }
    const sum = ws.reduce(add,0)
    const roll = random(sum)
    var acc = 0
    var index = 0
    while(ws) {
        const w = ws.shift() // pop front array
        acc += w
        if (acc > roll) {
            return moves[index]
        }
        index++
    }
    alert("randomWeightedPick: shouldn't reach here")
}

function random(number) {
    return Math.floor(Math.random() * number);
}

function candidateMovesAI(s,g,k) {
    const all = allLegalMoves(g)
    s.stop = false
    const scored = all.map(p => [p,1])
    return searchMoveDeepeningConsider(s,g,1,all,scored,k)
}

function searchMoveDeepeningConsider(s,g,depth,consider,lastScored,k) { //depth>=1
    redraw(s)
    console.log('AI depth ' + depth + '... [considering:' + consider.map(cellName) + ']')
    searchMoveDepthConsider(
        s,g,depth,consider,
        () => {
            s.lastAiMoveDepth = depth-1
            const cube = x => x*x*x
            //const weighted = consider.map(p => [p,cube(scorePos(p))]) //OLD
            const scores = lastScored.map(([_,s]) => s)
            function min(x,y) { return x < y ? x : y }
            const worstScore = scores.reduce(min,win)
            const weighted = lastScored.map(([p,s]) => [p, cube(1 + s - worstScore)])
            return k("Timeout, using depth="+(depth-1),weighted)
        },
        (victory,avoidLoss,scored) => {
            //console.log('Victory:' + victory.map(cellName))
            //console.log('AvoidLoss:' + avoidLoss.map(cellName))
            //console.log('Scored:' + scored.map(([p,s]) => cellName(p) + '=' + s))
            s.lastAiMoveDepth = depth
            if (victory.length > 0) {
                return k("Victory",victory.map(p => [p,1]))
            }
            if (avoidLoss.length === 0) {
                return k("Can't avoid loss", consider.map(p => [p,1]))
            }
            if (avoidLoss.length === 1) {
                return k("Single move forced", avoidLoss.map(p => [p,1]))
            }
            const n = consider.length - avoidLoss.length
            if (n > 0) {
                console.log("Avoiding " + n + " places")
            }
            return pauseThen(0,() => {
                return searchMoveDeepeningConsider(s,g,depth+1,avoidLoss,scored,k)
            })
        }
    )
}

const win = 999 //infinity
const loss = -win
const draw = 0

function searchMoveDepthConsider(s,g,depth,consider,stop,k) { //depth>=1
    const victory = []
    const avoidLoss = []
    const scored = []
    const loop = i => {
        if (i === consider.length) {
            return k(victory,avoidLoss,scored)
        } else {
            const m = consider[i]
            s.movesConsidered ++
            moveAtPosition(g,m)
            return scoreDepth(s,g, depth-1, win, stop, invScore => {
                const score = - invScore
                undoLastMove(g)
                if (score === win) victory.push(m)
                if (score > loss) {
                    avoidLoss.push(m)
                    scored.push([m,score]) //same length as avoidLoss
                }
                return loop(i+1)
            })
        }
    }
    return loop(0)
}

function scoreDepth(s,g,depth,cutoff,stop,k) { //depth>=0
    if (g.winByLastPlayer) return k(loss)
    if (g.moves.length === size*size) return k(draw)
    if (depth === 0) {
        //const score = 0 //no heuristic
        const score = scoreGameForCurrentPlayer(g) // heuristic
        return k(score)
    }
    const all = allLegalMoves(g)
    const best = loss
    const i = 0
    return considerMovesScore(s,g,depth,cutoff,all,i,best,stop,k)
}

function considerMovesScore(s,g,depth,cutoff,all,i,best,stop,k) {
    if (i === all.length) {
        return k(best)
    } else {
        const m = all[i]
        s.movesConsidered ++
        pauseMaybe(s,stop,() => {
            moveAtPosition(g,m)
            return scoreDepth(s,g, depth-1, -best, stop, invScore => {
                const score = - invScore
                undoLastMove(g)
                if (score >= cutoff) return k(score) //alpha-beta prune here!
                if (score > best) {
                    const newBest = score
                    return considerMovesScore(s,g,depth,cutoff,all,i+1,newBest,stop,k)
                } else {
                    return considerMovesScore(s,g,depth,cutoff,all,i+1,best,stop,k)
                }
            })
        })
    }
}

function pauseMaybe(s,stop,k) {
    if (s.movesConsidered % 500 === 0) {
        return pauseThen(0,() => {
            if (s.stop) {
                return stop()
            } else {
                return k()
            }
        })
    }
    return k()
}

function newState() {
    return {
        hover : undefined,
        nextPlayer : 1,
        winByLastPlayer : false ,
        board : [],
        moves : [],
        player1isAI : false,
        player2isAI : true,
        timeLimitIndex : initTimeLimitIndex,
        lastAiMoveDepth : 0
    }
}

function resetState(s) {
    s.hover = undefined
    s.nextPlayer = 1
    s.winByLastPlayer = false
    s.lastAiMoveDepth = 0
    s.moves = []
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const pos = [i,j]
            cellAt(s,pos).player = 0
        }
    }
}

function drawPiece(ctx,color) {
    const m = canvasSize/2
    const size = m*4/5
    ctx.beginPath()
    ctx.fillStyle = color
    ctx.arc (m, m, size, 0, 2*Math.PI)
    ctx.fill()
}

function drawCircle(ctx,color,thickness) {
    const m = canvasSize/2
    const size = m*4/5 - thickness/2
    ctx.beginPath()
    ctx.lineWidth = thickness
    ctx.strokeStyle = color
    ctx.arc (m, m, size, 0, 2*Math.PI)
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

function playerKindName(isAI) {
    return isAI ? "AI" : "Human"
}

function playerName(s,player) {
    return 'Player-' + player
}

function redrawStatus(s) {
    const {nextPlayer,winByLastPlayer} = s
    const p = document.getElementById('Status')
    if (gameOver(s)) {
        if (winByLastPlayer) {
            lastPlayer = otherPlayer(nextPlayer)
            p.style.color = colourOfPlayer(lastPlayer)
            p.textContent = 'Game Over. ' + playerName(s,lastPlayer) + ' wins!'
        } else {
            p.style.color = black
            p.textContent = 'Game Over. Draw'
        }
    } else {
        p.style.color = colourOfPlayer(nextPlayer)
        const thinking = isPlayerAI(s,nextPlayer) && s.aiRunning
        p.textContent = (playerName(s,nextPlayer) + ' to move'
                         + (thinking ? ' (AI thinking...)' : ''))
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

function redrawTimeLimit(s) {
    const limit = timeLimit(s)
    document.getElementById('TimeLimit') .textContent = limit/1000 + 's'
    document.getElementById('LastAiMoveDepth')
        .textContent = (s.lastAiMoveDepth > 0)
        ? 'Depth = ' + String(s.lastAiMoveDepth) : ''
    {
        const elem = document.getElementById('GameScore')
        const score = scoreGameForPlayer1(s)
        if (score === 0) {
            elem.textContent = '0'
            elem.style.color = black
        } else if (score > 0) {
            elem.textContent = '+' + score
            elem.style.color = purple
        } else {
            elem.textContent = '+' + -score
            elem.style.color = green
        }
    }
}

function redrawPlayerInfo(s) {
    const p1 = document.getElementById('Player1')
    const p2 = document.getElementById('Player2')
    p1.textContent = playerKindName(s.player1isAI)
    p2.textContent = playerKindName(s.player2isAI)
}

function redraw(s) {
    const {hover, nextPlayer, board} = s
    redrawStatus(s)
    redrawTimeLimit(s)
    redrawPlayerInfo(s)
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
                            const col =
                                  isPlayerAI(s,nextPlayer)
                                  ? white
                                  : colourOfPlayer(nextPlayer)
                            drawCircleSolid(ctx,col,4)
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

function isPlayerAI(s,player) {
    if (player === 1) return s.player1isAI
    if (player === 2) return s.player2isAI
    alert("isPlayerAI!")
}

function colourOfPlayer(player) {
    if (player === 1) return purple
    if (player === 2) return green
    alert("colourOfPlayer!")
}

function moveAtPosition(s,pos) {
    const { moves, nextPlayer } = s
    if (isLegalMove(s,pos)) {
        cellAt(s,pos).player = nextPlayer
        s.nextPlayer = otherPlayer(nextPlayer);
        s.winByLastPlayer = isWinline(s,pos,nextPlayer)
        s.moves.push(pos)
    }
}

function undoLastMove(s) {
    const { moves, nextPlayer } = s
    if (s.moves.length > 0) {
        const pos = s.moves.pop()
        cellAt(s,pos).player = 0
        s.nextPlayer = otherPlayer(nextPlayer);
        s.winByLastPlayer = false
    }
}

function scoreGameForCurrentPlayer(s) {
    const score = scoreGameForPlayer1(s)
    const player = s.nextPlayer
    if (player === 1) return score
    if (player === 2) return -score
    alert("scoreGameForCurrentPlayer!")
}

function scoreGameForPlayer1(s) {
    var acc = 0
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const pos = [i,j]
            const player = playerAt(s,pos)
            if (player === 1) acc += scorePos(pos)
            if (player === 2) acc -= scorePos(pos)
        }
    }
    return acc
}

const quartile = [
    [ 3,  4,  5,  7 ],
    [ 4,  6,  8, 10 ],
    [ 5,  8, 11, 13 ],
    [ 7, 10, 13, 16 ]
]

function scorePos(pos) {
    const [i,j] = pos
    const ii = i<4 ? i : 7-i
    const jj = j<4 ? j : 7-j
    return quartile[ii][jj]
}

function allMoves() { //TODO: cache this?
    acc = []
    for(let i = 0; i < size ; i++) {
        for(let j = 0; j < size; j++) {
            const pos = [i,j]
            const score = scorePos(pos)
            acc.push([pos,score])
        }
    }
    acc.sort(([_,n1], [__,n2]) => n1-n2).reverse()
    return acc
}

//console.log(allMoves().map( ([[i,j],n]) => 'scoredPos(' + i + ',' + j + ')=' + n))

function allLegalMoves(s) {
    const res = []
    const all = allMoves()
    for (let i = 0; i < size*size; i++) {
        const [pos,_] = all[i]
        if (isLegalMove(s,pos)) {
            res.push(pos)
        }
    }
    return res
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

init()

