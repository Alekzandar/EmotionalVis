//python -m SimpleHTTPServer

// --unsafely-treat-insecure-origin-as-secure="http://eg.bucknell.edu/~drt008/EmotionalVis/AffectivaSkeleton/"


// SDK Needs to create video and canvas nodes in the DOM in order to function
// Here we are adding those nodes a predefined div.
var divRoot = $("#affdex_elements")[0];
var width = 680;
var height = 480;
var faceMode = affdex.FaceDetectorMode.LARGE_FACES;
//Construct a CameraDetector and specify the image width / height and face detector mode.
var detector = new affdex.CameraDetector(divRoot, width, height, faceMode);

var safeData = null;


//Initialize arrays to hold the JSON data..... honestly idk what I'm doing
var appearance = [];
var emotions = [];
var expressions = [];
var emojis = [];
var times = [];



//Enable detection of all Expressions, Emotions and Emojis classifiers.
detector.detectAllEmotions();
detector.detectAllExpressions();
detector.detectAllEmojis();
detector.detectAllAppearance();

//Add a callback to notify when the detector is initialized and ready for runing.
detector.addEventListener("onInitializeSuccess", function() {
  log('#logs', "The detector reports initialized");
  //Display canvas instead of video feed because we want to draw the feature points on it
  $("#face_video_canvas").css("display", "block");
  $("#face_video").css("display", "none");
});

function log(node_name, msg) {
  $(node_name).append("<span>" + msg + "</span><br />")
}

//function executes when Start button is pushed.
function onStart() {
  if (detector && !detector.isRunning) {
    $("#logs").html("");
    detector.start();
  }
  log('#logs', "Clicked the start button");
}

//function executes when the Stop button is pushed.
function onStop() {
  log('#logs', "Clicked the stop button");
  if (detector && detector.isRunning) {
	prepCSV();
    detector.removeEventListener();
    detector.stop();

    
    
  }
};

//function executes when the Reset button is pushed.
function onReset() {
  log('#logs', "Clicked the reset button");
  if (detector && detector.isRunning) {
    detector.reset();

    $('#results').html("");
  }
};

//Add a callback to notify when camera access is allowed
detector.addEventListener("onWebcamConnectSuccess", function() {
  log('#logs', "Webcam access allowed");
  console.log("Webcam access allowed");
});

//Add a callback to notify when camera access is denied
detector.addEventListener("onWebcamConnectFailure", function() {
  log('#logs', "webcam denied");
  console.log("Webcam access denied");
});

//Add a callback to notify when detector is stopped
detector.addEventListener("onStopSuccess", function() {
  log('#logs', "The detector reports stopped");
  $("#results").html("");
});

//Add a callback to receive the results from processing an image.
//The faces object contains the list of the faces detected in an image.
//Faces object contains probabilities for all the different expressions, emotions and appearance metrics
detector.addEventListener("onImageResultsSuccess", function(faces, image,
  timestamp) {
  $('#results').html("");
  log('#results', "Timestamp: " + timestamp.toFixed(2));
  log('#results', "Number of faces found: " + faces.length);
  if (faces.length > 0) {
  
  	//JSONs of each feature
  	var cApp = JSON.stringify(faces[0].appearance);
  	
  	var cEmotions = JSON.stringify(faces[0].emotions,
      function(key, val) {
        return val.toFixed ? Number(val.toFixed(0)) : val;
      });
      
    var cExpr = JSON.stringify(faces[0].expressions,
      function(key, val) {
        return val.toFixed ? Number(val.toFixed(0)) : val;
      });
      
    var cEmoji = faces[0].emojis.dominantEmoji;
      
    
  
    // Gets gender, age, facial features
    log('#results', "Appearance: " + cApp);

    log('#results', "Emotions: " + cEmotions);
    log('#results', "Expressions: " + cExpr);

    // Return an emoji of face
    log('#results', "Emojis: " + cEmoji);
    
    drawFeaturePoints(image, faces[0].featurePoints);
    



    
    if (((timestamp.toFixed() % 5) == 0) & (timestamp.toFixed(2) - (timestamp.toFixed() < .2))){
    	//Add data to arrays
		//appearance.push(cApp);
		emotions.push(faces[0].emotions);
		//expressions.push(cExpr);
		//emojis.push(cEmoji);  
		times.push(timestamp);
    }
     
 } 
});




//My new code
function convertToCSV(objArray) {
	//log('#logs', "dataArr:"+ itemsFormatted);
	
	var array = typeof objArray != 'object' ? JSON.parse(objArray) : objArray;
    var str = '';

    for (var i = 0; i < objArray.length; i++) {
        var line = '';
        for (var index in objArray[i]) {
            if (line != ''){
            	line += ',';
            	line += objArray[i][index].toFixed(0);
            }
            else{
            	line += objArray[i][index].toFixed(0);
            }
        }

        str += line + '\r\n';
    }

    return str;
}

//My new code
function timeConvertToCSV(objArray) {
	//log('#logs', "dataArr:"+ itemsFormatted);
	var array = typeof objArray != 'object' ? JSON.parse(objArray) : objArray;
	
    var line = '';

    for (var i = 0; i < objArray.length; i++) {
    	if (line != ''){
            line += ',';
            line += objArray[i].toFixed(0);
        }
        else{
        	line += objArray[i].toFixed(0);
        }
    }

    line +=  '\r\n';

    return line;
}


function prepCSV(){
	
	//{"joy":0,"sadness":0,"disgust":0,"contempt":0,"anger":0,"fear":0,"surprise":0,"valence":0,"engagement":0},
    var headers = "Joy, Sadness, Disgust, Contempt, Anger, Fear, Surprise, Valence, Engagement, Time \r\n";
    
    	/*}
    	joy: "Joy",
    	sadness: "Sadness",
   		disgust: "Disgust",
    	contempt: "Contempt",
    	anger: "Anger",
    	fear: "Fear",
   		surprise: "Surprise",
    	valence: "Valence",
		engagement: "Engagement",
		time: "Time"
    
	};*/
	
	var fileTitle = "affecttest"; // or 'my-unique-title'

	
	//Format for CSV
	emotFormatted = convertToCSV(emotions);
	timeFormatted = timeConvertToCSV(times);
	//var dataArr = new Array(emotions, times);
	
	
	dataFormatted = emotFormatted + timeFormatted;
	//log('#logs', "data:"+ dataFormatted);
	
	exportCSVFile( headers, dataFormatted, fileTitle); 
	// call the exportCSVFile() function to process the JSON and trigger the download
	

}


function exportCSVFile(headers, items, fileTitle) {
    if (headers) {
        items = headers + items;
        
    }
    

     //Convert Object to JSON
    //var jsonObject = JSON.stringify(items);

    var csv = items;

    var exportedFilename = fileTitle + '.csv' || 'export.csv';

    var blob = new Blob([csv], { type: 'text/csv;charset=utf-8;' });
    if (navigator.msSaveBlob) { // IE 10+
        navigator.msSaveBlob(blob, exportedFilenmae);
    } else {
        var link = document.createElement("a");
        if (link.download !== undefined) { // feature detection
            // Browsers that support HTML5 download attribute
            var url = URL.createObjectURL(blob);
            link.setAttribute("href", url);
            link.setAttribute("download", exportedFilename);
            link.style.visibility = 'hidden';
            document.body.appendChild(link);
            link.click();
            document.body.removeChild(link);
        }
    }
}

//End of my new code



//Draw the detected facial feature points on the image
function drawFeaturePoints(img, featurePoints) {
  var contxt = $('#face_video_canvas')[0].getContext('2d');

  var hRatio = contxt.canvas.width / img.width;
  var vRatio = contxt.canvas.height / img.height;
  var ratio = Math.min(hRatio, vRatio);

  contxt.strokeStyle = "#FFFFFF";
  for (var id in featurePoints) {
    contxt.beginPath();
    contxt.arc(featurePoints[id].x,
      featurePoints[id].y, 2, 0, 2 * Math.PI);
    contxt.stroke();

  }
}
