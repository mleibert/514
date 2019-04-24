remark.macros.scaleimgpct = function (percentage) {
  var url = this;
  return '<img src="' + url + '" style="width: ' + percentage + '" />';
};
remark.macros.scaleimgpxl = function (pixels) {
  var url = this;
  return '<img src="' + url + '" width="' + pixels + '" />';
}
remark.macros.boxedtttext = function(text,color){
return '<br><br> <tt><style>div.blue { background-color:' + color + '; border-radius: 5px; padding: 20px;}</style> <div class = "blue"> <center>' + text + '</center> </div></tt>'
};
remark.macros.boxedtttextblue = function(text){
return '<tt><style>div.blue { background-color:#E6F0FF; border-radius: 0px; padding: 20px;}</style> <div class = "blue"> <left>' + text + '</left> </div></tt>'
};
remark.macros.boxedtext = function(text,color){
return '<br><br><style>div.blue { background-color:' + color + '; border-radius: 5px; padding: 20px;}</style> <div class = "blue"> <center>' + text + '</center> </div>'
};
remark.macros.boxedtextblue = function(text){
return '<style>div.blue { background-color:#E6F0FF; border-radius: 0px; padding: 20px;}</style> <div class = "blue"> <left>' + text + '</left> </div>'
};
remark.macros.ref = function(text){
  return '<sup>' + text + '</sup>'
};
