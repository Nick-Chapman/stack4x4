
const black = 'rgb(0,0,0)'
const white = 'rgb(255,255,255)'
const dark = 'rgb(140,140,140)'
const light = 'rgb(160,160,160)'
const purple = 'rgb(137,52,235)'
const green = 'rgb(25,170,25)'

const gold = 'rgb(255,255,0)'
const blue = 'rgb(0,0,255)'
const red = 'rgb(255,0,0)'

const size = 8
const winLineLength = 4

const aiMoveHighlightTime = 100 // flash white/gold/red/blue circle(s)

const timeLimits = [ 0, 1000, 2000, 3000, 5000, 8000, 13000, 21000, 34000 ]
const initTimeLimitIndex = 1

const humanFirst = true

const useH2 = true

const scoreGameForPlayer1 =
      useH2
      ? scoreGameForPlayer1_H2
      : scoreGameForPlayer1_H1

function timeLimit(s) {
    return timeLimits[s.timeLimitIndex]
}

function pauseThen(ms,f) {
    setTimeout(f,ms)
}

function saveState(s) {
    localStorage.setItem('NewSavedMoves',JSON.stringify(s.moves))
}

function init() {
    const s = newState()
    setupDOM(s)
    const moves = JSON.parse(localStorage.getItem('NewSavedMoves'))
    if (moves) {
        s.restoring = true
        lockoutHuman(s,true)
        restoreState(s,moves,0)
    }
}

function restoreState(s,moves,n) {
    if (n < moves.length) {
        const totalDuration = 200 * Math.sqrt(moves.length)
        const duration = totalDuration / moves.length
        pauseThen(duration,() => {
            const pos = moves[n]
            moveAtPosition(s,pos)
            redraw(s)
            restoreState(s,moves,n+1)
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
    for (const name of buttonsToDisable) {
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
    for(let pos = 0; pos < size*size ; pos++) {
        const [i,j] = positionCoords(pos)
        const canvas = document.createElement('canvas')
        gridTag.appendChild(canvas)
        canvas.width = canvasSize
        canvas.height = canvasSize
        canvas.setAttribute('class','cell')
        canvas.onclick = moveAtPositionAndUpdate(s,pos)
        canvas.onmouseover = mouseOver(s,pos)
        canvas.onmouseout = mouseOut(s)
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
        if (j === 7) {
            const lineBreak = document.createElement('div')
            lineBreak.setAttribute('class','break')
            gridTag.appendChild(lineBreak)
        }
    }
    return s
}

function mouseOver(s,pos) { return function() {
    if (!s.aiRunning && !s.restoring) {
        s.hover = [pos]
        redraw(s)
    }
}}

function mouseOut(s) { return function() {
    if (!s.aiRunning && !s.restoring) {
        s.hover = []
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
            chooseMoveAI(s,(rationaleColour,rationale,weighted0) => {
                const weighted = weighted0.slice(0,3) //random pick from best 3
                const pos = randomWeightedPick(weighted)
                console.log(weighted0.map(([p,w]) => cellName(p)+':'+w))
                console.log(s.movesConsidered, rationale, cellName(pos))
                s.lastAiConsidered = s.movesConsidered
                timeoutAlive = false
                s.hoverColor = rationaleColour
                s.hover = weighted.map(([p,_]) => p)
                redraw(s)
                pauseThen(aiMoveHighlightTime,() => {
                    s.hover = [pos]
                    redraw(s)
                    pauseThen(2*aiMoveHighlightTime,() => {
                        s.hover = []
                        moveAtPosition(s,pos)
                        s.aiRunning = false
                        lockoutHuman(s,false)
                        document.getElementById('StopAI').textContent = ''
                        endOfInteraction(s)
                    })
                })
            })
        })
    }
}

function randomWeightedPick(weighted) {
    const ws = weighted.map(([_,w]) => w)
    const moves = weighted.map(([m,_]) => m)
    function add(x,y) { return x + y }
    const sum = ws.reduce(add,0)
    const roll = random(sum)
    var acc = 0
    var index = 0
    while(ws.length > 0) {
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
    g.moves = s.moves.slice(0)
    g.board = cloneBoardCells(s.board)
    g.winByLastPlayer = s.winByLastPlayer
    s.movesConsidered = 0
    return candidateMovesAI(s,g,k)
}

function candidateMovesAI(s,g,k) {
    const all = allLegalMoves(g)
    s.stop = false
    const scored = all.map(p => [p,scorePosH1(p)])
    return searchMoveDeepeningConsider(s,g,1,scored,k)
}

function searchMoveDeepeningConsider(s,g,depth,scored,k) { //depth>=1
    redraw(s)
    const sortedScored =
          scored
          .sort(([_,n1], [__,n2]) => n1-n2)
          .reverse()
    const considerOrdered = sortedScored.map(([pos,_]) => pos)
    console.log('AI depth ' + depth + '... [#' + considerOrdered.length + '] (' + s.movesConsidered + ')')
    const stop = () => {
        s.lastAiDepth = depth-1

        const cube = x => x*x*x
        const scores = scored.map(([_,s]) => s)
        function min(x,y) { return x < y ? x : y }
        const worstScore = scores.reduce(min,win)
        const weighted =
              sortedScored
              .map(([pos,s]) => [pos, cube(1 + s - worstScore)])
        return k(white,"Timeout, using depth="+(depth-1),weighted)
    }
    searchMoveDepthConsider(
        s,g,depth,considerOrdered,stop,
        (victory,avoidLoss,scoredAgain) => {
            s.lastAiDepth = depth
            if (victory.length > 0) {
                return k(gold,"Victory",victory.map(p => [p,1]))
            }
            if (avoidLoss.length === 0) {
                return k(red,"Can't avoid loss", considerOrdered.map(p => [p,1]))
            }
            if (avoidLoss.length === 1) {
                return k(blue,"Single move forced", avoidLoss.map(p => [p,1]))
            }
            const n = considerOrdered.length - avoidLoss.length
            if (n > 0) {
                console.log("Avoiding " + n + " places")
            }
            return checkStop(s,stop,() => {
                return searchMoveDeepeningConsider(s,g,depth+1,scoredAgain,k)
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
            const pos = consider[i]
            s.movesConsidered ++
            moveAtPosition(g,pos)
            return scoreDepth(s,g, depth-1, undefined, win, stop, (_km,invScore) => {
                const score = - invScore
                undoLastMove(g)
                if (score === win) victory.push(pos)
                if (score > loss) {
                    avoidLoss.push(pos)
                    scored.push([pos,score]) //same length as avoidLoss
                }
                return loop(i+1)
            })
        }
    }
    return loop(0)
}

function scoreDepth(s,g,depth,killer,cutoff,stop,k) { //depth>=0
    if (g.winByLastPlayer) return k(undefined,loss)
    if (g.moves.length === size*size) return k(undefined,draw)
    if (depth === 0) {
        //const score = 0 //no heuristic
        const score = scoreGameForCurrentPlayer(g) // heuristic
        return k(undefined,score)
    }

    if (killer) {
        const all = allLegalMoves_withKiller(g,killer)
        const best = loss
        const i = 0
        return considerMovesScore(s,g,depth,undefined,cutoff,all,i,best,stop,k)
    } else {
        const all = allLegalMoves(g)
        const best = loss
        const i = 0
        return considerMovesScore(s,g,depth,undefined,cutoff,all,i,best,stop,k)
    }
}

function considerMovesScore(s,g,depth,killer,cutoff,all,i,best,stop,k) {
    if (i === all.length) {
        return k(undefined,best)
    } else {
        const pos = all[i]
        s.movesConsidered ++
        checkStopMaybe(s,stop,() => {
            moveAtPosition(g,pos)
            return scoreDepth(s,g, depth-1, killer, -best, stop, (km,invScore) => {
                const score = - invScore
                undoLastMove(g)
                if (score >= cutoff) {
                    return k(pos,score) //alpha-beta prune here! passing back the killer move
                }
                if (score > best) {
                    const newBest = score
                    return considerMovesScore(s,g,depth,km,cutoff,all,i+1,newBest,stop,k)
                } else {
                    return considerMovesScore(s,g,depth,km,cutoff,all,i+1,best,stop,k)
                }
            })
        })
    }
}

function checkStopMaybe(s,stop,k) {
    if (s.movesConsidered % 1000 === 0) {
        return checkStop(s,stop,k)
    }
    return k()
}

function checkStop(s,stop,k) {
    return pauseThen(0,() => {
        if (s.stop) {
            return stop()
        } else {
            return k()
        }
    })
}

function newState() {
    return {
        hover : [],
        hoverColor : white,
        nextPlayer : 1,
        winByLastPlayer : false ,
        board : [],
        moves : [],
        player1isAI : !humanFirst,
        player2isAI : humanFirst,
        timeLimitIndex : initTimeLimitIndex,
        lastAiDepth : 0,
        lastAiConsidered : 0
    }
}

function resetState(s) {
    s.hover = []
    s.nextPlayer = 1
    s.winByLastPlayer = false
    s.lastAiDepth = 0
    s.moves = []
    for(let pos = 0; pos < size*size ; pos++) {
        cellAt(s,pos).player = 0
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
    for (const pos of s.moves) {
        p.appendChild(createMoveText(player,pos))
        player = otherPlayer(player)
    }
}

function redrawTimeLimit(s) {
    const limit = timeLimit(s)
    document.getElementById('TimeLimit') .textContent = limit/1000 + 's'
    document.getElementById('LastAiDepth')
        .textContent = (s.lastAiDepth > 0)
        ? 'Depth = ' + String(s.lastAiDepth) : ''
    document.getElementById('LastAiConsidered').textContent = '[' + String(s.lastAiConsidered) + ']'
    {
        const elem = document.getElementById('GameScoreH1')
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
    {
        const elem = document.getElementById('GameScoreH2')
        const tup = analyzeGameByH2(s)
        elem.textContent = String(tup)
    }
}

function redrawPlayerInfo(s) {
    const p1 = document.getElementById('Player1')
    const p2 = document.getElementById('Player2')
    p1.textContent = playerKindName(s.player1isAI)
    p2.textContent = playerKindName(s.player2isAI)
}

function redraw(s) {
    redrawStatus(s)
    redrawTimeLimit(s)
    redrawPlayerInfo(s)
    redrawMoveList(s)
    const finished = gameOver(s)
    for(let pos = 0; pos < size*size ; pos++) {
        const cell = s.board[pos]
        const canvas = cell.canvas
        const ctx = cell.ctx
        ctx.clearRect(0, 0, canvas.width, canvas.height);
        if (cell.player === 0) {
            if (!finished) {
                if (isLegalMove(s,pos)) {
                    if (posInList(pos,s.hover)) {
                        const col =
                              isPlayerAI(s,s.nextPlayer)
                              ? s.hoverColor
                              : colourOfPlayer(s.nextPlayer)
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

function posInList(pos,list) {
    for(const elem of list) {
        if (pos === elem) {
            return true
        }
    }
    return false
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

function scoreGameForPlayer1_H1(s) {
    var acc = 0
    for(let pos = 0; pos < size*size ; pos++) {
        const player = playerAt(s,pos)
        if (player === 1) acc += scorePosH1(pos)
        if (player === 2) acc -= scorePosH1(pos)
    }
    return acc
}

const quartile = [
    [ 3,  4,  5,  7 ],
    [ 4,  6,  8, 10 ],
    [ 5,  8, 11, 13 ],
    [ 7, 10, 13, 16 ]
]

//TODO: create full 4xquartile score info, as a single 64 long array, to be indexed by a (new)pos

function scorePosH1(pos) { //heuristic-1
    //const [i,j] = positionCoords(pos)
    const i = Math.floor (pos / size)
    const j = pos % size
    const ii = i<4 ? i : 7-i
    const jj = j<4 ? j : 7-j
    return quartile[ii][jj]
}

function allMovesOrderedByH1() {
    acc = []
    for(let pos = 0; pos < size*size ; pos++) {
        const score = scorePosH1(pos)
        acc.push([pos,score])
    }
    acc.sort(([_,n1], [__,n2]) => n1-n2).reverse()
    return acc
}

const allMovesOrderedByH1_cached = allMovesOrderedByH1()

function allLegalMoves(s) {
    const res = []
    const all = allMovesOrderedByH1_cached
    for (let i = 0; i < size*size; i++) {
        const [pos,_] = all[i]
        if (isLegalMove(s,pos)) {
            res.push(pos)
        }
    }
    return res
}

function allLegalMoves_withKiller(s,killer) {
    const res = []
    if (isLegalMove(s,killer)) {
        res.push(killer)
    }
    const all = allMovesOrderedByH1_cached
    for (let i = 0; i < size*size; i++) {
        const [pos,_] = all[i]
        if (isLegalMove(s,pos)) { //TODO: and not killer
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
    //const [i,j] = positionCoords(pos)
    const i = Math.floor (pos / size)
    const j = pos % size
    return (pillarX(s,0,i-1,j) || pillarX(s,i+1,size-1,j) ||
            pillarY(s,i,0,j-1) || pillarY(s,i,j+1,size-1))
}

function pillarX(s,x1,x2,y) {
    if (x1 > x2) return true
    else return !empty(s,makePos(x1,y)) && pillarX(s,x1+1,x2,y)
}

function pillarY(s,x,y1,y2) {
    if (y1 > y2) return true
    else return !empty(s,makePos(x,y1)) && pillarY(s,x,y1+1,y2)
}

function playerAt(s,pos) {
    return cellAt(s,pos).player
}

function cellAt(s,pos) {
    return s.board[pos]
}

function isWinline(s,pos,player) {
    return (isWinlineDir(s,pos,player, 0, 1, 0,-1) ||
            isWinlineDir(s,pos,player, 1, 0,-1, 0) ||
            isWinlineDir(s,pos,player, 1, 1,-1,-1) ||
            isWinlineDir(s,pos,player, 1,-1,-1, 1))
}

function isWinlineDir(s,pos,player,di1,dj1,di2,dj2) {
    const a = lengthPlayerLineDir(s,pos,player,di1,dj1)
    const b = lengthPlayerLineDir(s,pos,player,di2,dj2)
    return 1+a+b >= winLineLength
}

function lengthPlayerLineDir(s,pos,player,di,dj) {
    const i = Math.floor (pos / size)
    const j = pos % size
    if ((i===7 && di===1) || (i===0 && di===-1) || (j===7 && dj===1) || (j===0 && dj===-1)) return 0
    const pos2 = makePos(i+di,j+dj)
    if (playerAt(s,pos2) === player)
        return 1 + lengthPlayerLineDir(s,pos2,player,di,dj)
    else
        return 0
}

function makePos(i,j) {
    return i*size + j
}

function cellName(pos) {
    const [i,j] = positionCoords(pos)
    return String.fromCharCode(97 + j) + String(size-i)
}

function positionCoords(pos) { //TODO: avoid: creates tuples!
    const i = Math.floor (pos / size)
    const j = pos % size
    return [i,j]
}

function otherPlayer(player) {
    return 3 - player
}

function setupLinesForH2() {
    const lines = []
    function add(pos,line) { lines[pos].push(line) }
    for(let pos = 0; pos < size*size ; pos++) lines[pos] = []
    let line = 0
    for(let i = 0; i < size ; i++) { //0..7
        for(let j = 0; j <= winLineLength; j++) { //0..4
            for(let x = 0; x < winLineLength; x++) { //0..3
                add(makePos(i,j+x),line)
                add(makePos(j+x,i),line+1)
            }
            line+=2
        }
    }
    for(let a = 0; a < winLineLength ; a++) { //0..3
        for(let b = 0; b <= a; b++) {
            for(let x = 0; x < winLineLength; x++) { //0..3
                const i = b+x
                const j = 3+a-b-x
                const ii = size-1-i
                const jj = size-1-j
                add(makePos(i,j),line)
                add(makePos(jj,i),line+1)
                add(makePos(ii,jj),line+2)
                add(makePos(j,ii),line+3)
            }
            line+=4
        }
    }
    for(let a = 0; a <= winLineLength ; a++) { //0..4
        for(let x = 0; x < winLineLength; x++) { //0..3
                const i = a+x
                const ii = size-1-i
                add(makePos(i,ii),line)
                add(makePos(i,i),line+1)
            }
        line+=2
    }
    return lines
}

const linesByPos = setupLinesForH2()


function analyzeGameByH2(s) {
    let pByLine = []
    let gByLine = []
    for(let x = 0; x<130; x++) {
        pByLine[x] = 0
        gByLine[x] = 0
    }
    for(let pos = 0; pos < size*size ; pos++) {
        const player = playerAt(s,pos)
        if (player === 1) {
            for(const line of linesByPos[pos]) {
                pByLine[line]++
            }
        }
        if (player === 2) {
            for(const line of linesByPos[pos]) {
                gByLine[line]++
            }
        }
    }
    let u = 0 //unit
    let a = 0 //A
    let b = 0 //B
    for(let x = 0; x<130; x++) {
        const p = pByLine[x]
        const g = gByLine[x]
        if (g === 0) {
            if      (p === 1) u++
            else if (p === 2) a++
            else if (p === 3) b++
        }
        if (p === 0) {
            if      (g === 1) u--
            else if (g === 2) a--
            else if (g === 3) b--
        }
    }
    return [u,a,b]
}

// heuristic parameters
const A = 10
const B = 20

function scoreGameForPlayer1_H2(s) {
    const [u,a,b] = analyzeGameByH2(s)
    const res = u + A*a + B*b
    return res
}

init()
