setInterval(function(){
  if ($('html').attr('class')=='shiny-busy') {
	updateBusy();
    $('div.busy').show()
  } else {
	updateBusy();
    $('div.busy').hide()
  }
},100)