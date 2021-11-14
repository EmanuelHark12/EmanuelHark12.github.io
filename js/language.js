/*jslint browser: true*/
/*global $, jQuery, alert*/

$(document).ready(function () {

    $(function () {
        $('.translate').change(function () {
            var lg = $(this).val();
            var path = 'js/json/' + lg + '.json';
            fetch(path)
                .then(response => response.json())
                .then(function(data){
                    $('.lang').each(function (index, element) {
                        $(this).text(data[$(this).attr('key')])
                });
        });
        })
    });
    $(function () {
        $('.translated').change(function () {
            var lg = $(this).val();
            if(lg == 'en'){
                $('.eng-table').show();
                $('.eng-image').show();
                $('.pt-table').hide();
                $('.pt-image').hide();
            }
            else{
                $('.eng-table').hide();
                $('.eng-image').hide();
                $('.pt-table').show();
                $('.pt-image').show();
            }
            var path = '../../js/json/' + lg + '.json';
            fetch(path)
                .then(response => response.json())
                .then(function(data){
                    $('.lang').each(function (index, element) {
                        $(this).text(data[$(this).attr('key')])
                });
        });
        })
    });
    $(function () {
        $('#js-navbar-toggle').click(function () {
            var disp = $('#js-menu').css('display');
            if(disp == 'null' || disp == 'none'){
                $('#js-menu').css('display','block');
            }
            else{
                $('#js-menu').css('display','none');  
            }
        })
    });
    $('.eng-table').show();
    $('.eng-image').show();
    $('.pt-table').hide();
    $('.pt-image').hide();
    $('.translated').val('en');
    $('.translate').val('en');

});
$(window).resize(function() {
    if ($(this).width() > 768) {
      $('#js-menu').hide();
    }
  });