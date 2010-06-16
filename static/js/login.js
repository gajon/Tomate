$(document).ready(function() {
	$('#id_username').focus();
	$('#id_username').select();


	var timezone = (new Date()).getTimezoneOffset() / 60;
	$('#id_timezone').val(timezone);
});
