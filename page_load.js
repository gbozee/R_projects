$(document).ready(function(){
//   $('#visual_sidebar').toggleClass("hidden");
//Get the visualization tag and disable it;
// $('.tabbable .nav.nav-tabs li:last-child').addClass('disabled'); // Bootstrap has a class that automatically disables a tab.
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
  // code when the visualize button is clicked
  $("#visualizeAction").click(function(e){
    var tabs = $('.tabbable .nav.nav-tabs li a'); // it tries to find all the list of tabs 
    console.log(tabs);
    $(tabs[1]).click(); //0 indexing of the list of tags e.g x = [1,2,3,4], x[0] = 1. it selects the second tab  
  })
  // Event listener for the tabs 
  $('.tabbable .nav.nav-tabs li a').click(function(){
      var tab_text = $(this).text(); // Gets the name of the currently click tab and extract the content;
        $('#table_sidebar').toggleClass("hidden");
        $('#visual_sidebar').toggleClass("hidden")
      
  })
});