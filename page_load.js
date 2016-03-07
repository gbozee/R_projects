$(document).ready(function(){
  
  $('#loadDataset').click(function(e){
    $('#l_dataset').removeClass("hidden");
    $('#u_dataset').removeClass('hidden').addClass("hidden");
    $('#model_section').removeClass("hidden").addClass("hidden");
  });
  $("#uploadDataset").click(function(e){
    console.log("Clicked");
    $('#u_dataset').removeClass("hidden");
    $('#l_dataset').removeClass("hidden").addClass("hidden");
    $('#model_section').removeClass("hidden").addClass("hidden");
  });
  $(".displayAction").click(function(e){
    $('#model_section').removeClass("hidden");
  })
});