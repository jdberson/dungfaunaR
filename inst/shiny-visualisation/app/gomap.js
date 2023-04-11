// When locator icon in datatable is clicked, go to that spot on the map
$(document).on("click", ".go-map", function(e) {
  e.preventDefault();
  $el = $(this);
  var lat = $el.data("lat");
  var long = $el.data("long");
  var data = $el.data("data");
  $($("#nav a")[1]).tab("show");
  Shiny.onInputChange("goto", {
    lat: lat,
    lng: long,
    data: data,
    nonce: Math.random()
  });
});