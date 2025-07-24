/*
Wilson's algorithm
by poypoyan

Distributed under the MIT software license. See the accompanying
file LICENSE or https://opensource.org/license/mit/.
*/

// helper functions
function randomElementInArr(inputArr) {
    const rndIdx = Math.floor(Math.random() * inputArr.length);
    return [inputArr[rndIdx], rndIdx];
}
function includedButNotLast(inputArr, element) {
    return ![-1, inputArr.length - 1].includes(inputArr.indexOf(element));
}

// algorithm proper
function wilsonStart(vertices, edges, cellsX, cellsY, graphStruct) {   // run once
    let update = {frontColor: new Array()};
    // custom data of algorithm
    let data = {temp: new Array(), currVert: null, verticesFinal: new Array()};
    // make a random vertex final
    let [firstVert, randIdx] = randomElementInArr(vertices);
    vertices.splice(randIdx, 1);
    data['verticesFinal'].push(firstVert);
    update['frontColor'].push(firstVert);
    return [update, data];
}
function wilsonLoop(data, vertices, cellsX, cellsY, graphStruct) {   // run inside loop
    let update = {blankColor: new Array(), frontColor: new Array(), extraColor: new Array()};

    if (!data['temp'].length) {
        // make another random vertex temporary
        let [newVert,] = randomElementInArr(vertices);
        data['temp'].push(newVert);
        data['currVert'] = newVert;
        update['extraColor'].push(newVert);
    } else if (includedButNotLast(data['temp'], data['currVert'])) {
        // turn off the temp vertices right of currVert
        let sliceIdxVert = data['temp'].indexOf(data['currVert']) + 1;
        update['blankColor'].push.apply(update['blankColor'], data['temp'].slice(sliceIdxVert, -1));
        // revert the temp vertices to left vertices of currVert
        data['temp'].splice(sliceIdxVert);
    } else if (data['verticesFinal'].includes(data['currVert'])) {
        // make temp vertices final
        update['frontColor'].push.apply(update['frontColor'], data['temp']);
        newFinalVerts = data['temp'].filter((cell, idx) => {return idx % 2 === 0 && idx !== data['temp'].length - 1;});
        newFinalVerts.forEach((cell) => {
            vertices.splice(vertices.indexOf(cell), 1);
            data['verticesFinal'].push(cell);
        });
        data['temp'] = new Array();
    } else {
        // traverse through random edge and next vertex of currVert
        let [newEdgeVert,] = randomElementInArr(graphStruct.get(data['currVert']));
        data['temp'].push.apply(data['temp'], newEdgeVert);
        data['currVert'] = newEdgeVert[1];
        update['extraColor'].push.apply(update['extraColor'], newEdgeVert);
    }
    return update;
}
