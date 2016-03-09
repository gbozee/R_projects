$(document).ready(function(){
  
  $('#loadDataset').click(function(e){
    $('.set_container').removeClass("hidden");
    $('#l_dataset').removeClass("hidden");
    $('#u_dataset').removeClass('hidden').addClass("hidden");
    $('#model_section').removeClass("hidden").addClass("hidden");
  });
  $("#uploadDataset").click(function(e){
    $('.set_container').removeClass("hidden");
    console.log("Clicked");
    $('#u_dataset').removeClass("hidden");
    $('#l_dataset').removeClass("hidden").addClass("hidden");
    $('#model_section').removeClass("hidden").addClass("hidden");
  });
  $(".displayAction").click(function(e){
    $('#model_section').removeClass("hidden");
  })
  $("#visualizeAction").click(function(e){
    var tabs = $('.tabbable .nav.nav-tabs li a');
    console.log(tabs);
    $(tabs[1]).click();
  })
});