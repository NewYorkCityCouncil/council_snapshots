(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

ga('create', 'UA-111461633-2', 'auto');
ga('send', 'pageview');



// Add event trackers here (https://shiny.rstudio.com/articles/google-analytics.html)

// Track council district selection
$(document).on('change', '#coun_dist', function(e) {
  ga('send', {
              'hitType': 'event',
              'eventCategory': 'coun_dist',
              'eventAction': 'select',
              'eventLabel': $(e.currentTarget).val()
            });
});

// Track week selection
$(document).on('change', '#week', function(e) {
  ga('send', {
              'hitType': 'event',
              'eventCategory': 'week',
              'eventAction': 'select',
              'eventLabel': $(e.currentTarget).val()
            });
});

// Track report download
$(document).on('click', '#pdf_report', function(e) {
  ga('send', {
              'hitType': 'event',
              'eventCategory': 'report download',
              'eventAction': 'download',
              'eventLabel': 1
            });
});

// Track link clicks
$(document).on('click', 'a', function(e) {
  ga('send', {
              'hitType': 'event',
              'eventCategory': 'link',
              'eventAction': 'click',
              'eventLabel': $(e).attr('href')
            });
});



