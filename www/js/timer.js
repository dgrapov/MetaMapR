<script type="text/javascript">
var wasBusy = false;
var elapsedTimer = null;
var startTime = null;
function updateBusy() {
  var isBusy = $('html').hasClass('shiny-busy');
  if (isBusy && !wasBusy) {
    startTime = new Date().getTime();
    elapsedTimer = setInterval(function() {
      var millisElapsed = new Date().getTime() - startTime;
      $('#progress').text(Math.round(millisElapsed/1000) + ' seconds have elapsed');
    }, 1000);
  }
  else if (!isBusy && wasBusy) {
    clearInterval(elapsedTimer);
  }
  wasBusy = isBusy;
}
</script>