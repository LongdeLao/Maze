/*
Rectangular Maze Canvas
by poypoyan

Distributed under the MIT software license. See the accompanying
file LICENSE or https://opensource.org/license/mit/.
*/

function createCanvasObj(vertexX, vertexY, cellPixel, animateDelay, borderColor, domLocation) {
    // derive cell width (X), cell height (Y), and number of all cells
    const cellsX = 2 * vertexX - 1,
    cellsY = 2 * vertexY - 1,
    cellsAll = cellsX * cellsY;

    // initialize vertex and edge indices
    // notes:
    // 1) vertices and edges should not have duplicates (i.e. Set-like)
    // but is set to Arrays for more flexible use by the algorithm.
    // 2) The algorithm must remove elements in vertices. Termination occurs
    // when the vertices array becomes empty.
    let vertices = new Array();
    let edges = new Array();
    for (let i = 0; i < cellsY; i++) {
        for (let j = 0; j < cellsX; j++) {
            if (i % 2 === 0 && j % 2 === 0) vertices.push(cellsX * i + j);
            if ((i + j) % 2 === 1) edges.push(cellsX * i + j);
        }
    }

    // derive graph traversal structure for maze generation algorithm
    // structure: {<vertex index>: [[<connecting edge index>, <other vertex connected to that edge>], ...], ...}
    // type: map of int to array of pairs.
    let graphStruct = new Map();
    vertices.forEach((vertex) => {
        graphStruct.set(vertex, new Array());
        if (vertex - cellsX >= 0) graphStruct.get(vertex).push([vertex - cellsX, vertex - 2 * cellsX]);
        if (vertex + cellsX < cellsAll) graphStruct.get(vertex).push([vertex + cellsX, vertex + 2 * cellsX]);
        if (vertex % cellsX !== 0) graphStruct.get(vertex).push([vertex - 1, vertex - 2]);
        if (vertex % cellsX !== cellsX - 1) graphStruct.get(vertex).push([vertex + 1, vertex + 2]);
    });

    // create canvas
    const canvas = document.createElement('canvas');
    canvas.width = cellsX * cellPixel;
    canvas.height = cellsY * cellPixel;
    canvas.style = 'border: 3px solid ' + borderColor;
    canvas.innerHTML = 'Your browser does not support the canvas element.';
    document.body.insertBefore(canvas, domLocation);

    return {
        'canvas': canvas,
        'cellsX': cellsX,
        'cellsY': cellsY,
        'cellPixel': cellPixel,
        'animateDelay': animateDelay,
        'freshVertices': vertices,   // for re-initialization
        'edges': edges,
        'graphStruct': graphStruct,
        'vertices': null,   // this and the rest are mutable
        'running': false,
        'justEvented': false   // event guard to prevent parallel runs
    };   // canvasObj
}

// background fill. can also be used for clearing canvas
function clearCanvas(canvasObj, color) {
    const canvas = canvasObj.canvas;

    const ctxBack = canvas.getContext('2d');
    ctxBack.fillStyle = color;
    ctxBack.fillRect(0, 0, canvas.width, canvas.height);
}

// cell fill
function fillCell(canvasObj, color, idx) {
    const canvas = canvasObj.canvas,
        cellsX = canvasObj.cellsX,
        cellsY = canvasObj.cellsY,
        cellPixel = canvasObj.cellPixel;

    const ctxCell = canvas.getContext('2d');
    ctxCell.fillStyle = color;
    ctxCell.fillRect(cellPixel * (idx % cellsX), cellPixel * Math.floor(idx / cellsX), cellPixel, cellPixel);
}

// animation functions
function initDraw(canvasObj, blankColor, colorsObj, algoStart, algoLoop) {
    const graphStruct = canvasObj.graphStruct;

    // initialize maze generation
    canvasObj.running = true;
    canvasObj.vertices = Array.from(canvasObj.freshVertices);
    let [updateCellsInit, algoData] = algoStart(canvasObj.vertices, canvasObj.edges, canvasObj.cellsX, canvasObj.cellsY, graphStruct);

    // updates change cell colors
    clearCanvas(canvasObj, blankColor);
    Object.keys(updateCellsInit).forEach((name) => {
        updateCellsInit[name].forEach((vertex) => {fillCell(canvasObj, colorsObj[name], vertex);});
    });

    reDraw(canvasObj, blankColor, colorsObj, algoData, algoStart, algoLoop);
}

function reDraw(canvasObj, blankColor, colorsObj, algoData, algoStart, algoLoop) {
    const animateDelay = canvasObj.animateDelay,
        graphStruct = canvasObj.graphStruct;

    let updateCells = algoLoop(algoData, canvasObj.vertices, canvasObj.cellsX, canvasObj.cellsY, graphStruct);

    // updates change cell colors
    if ('blankColor' in updateCells) updateCells['blankColor'].forEach((vertex) => {fillCell(canvasObj, blankColor, vertex);});
    Object.keys(updateCells).forEach((name) => {
        updateCells[name].forEach((vertex) => {fillCell(canvasObj, colorsObj[name], vertex);});
    });

    if (!canvasObj.running) {
        initDraw(canvasObj, blankColor, colorsObj, algoStart, algoLoop);
    } else if (canvasObj.vertices.length) {
        setTimeout(() => {reDraw(canvasObj, blankColor, colorsObj, algoData, algoStart, algoLoop)}, animateDelay);
    } else {
        canvasObj.running = false;
    }

    canvasObj.justEvented = false;
}