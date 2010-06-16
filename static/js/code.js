$(document).ready(function() {

	var tableFound = $(this).find("#task-listing").find("tbody").find("tr");
	if (tableFound && tableFound.length != 0) {
		//$("#task-listing").tablesorter({sortList: [[0,0]], widgets: ['zebra']});
		$("#task-listing").tablesorter({sortList: [[0,0]]});
		$("#task-listing tbody tr").hover(function() {
			$(this).find("td").each(function () {
				$(this).addClass("highlight");
				$(this).find("span.edit").removeClass("hidden");
			});
		}, function() {
			$(this).find("td").each(function () {
				$(this).removeClass("highlight");
				$(this).find("span.edit").addClass("hidden");
			});
		});



		var editTaskBind = function (originalData) {
			var taskId = $(originalData).find("a").attr("href");
			taskId = taskId.substr(15);


			var task = document.createElement("input");
			$(task).attr("type", "text");
			$(task).attr("name", "task");
			$(task).attr("id", "id_task");

			var estimated = document.createElement("input");
			$(estimated).attr("type", "text");
			$(estimated).attr("name", "estimations");
			$(estimated).attr("id", "id_estimations");

			var real = document.createElement("input");
			$(real).attr("type", "text");
			$(real).attr("name", "real");
			$(real).attr("id", "id_real");

			var tags = document.createElement("input");
			$(tags).attr("type", "text");
			$(tags).attr("name", "tags");
			$(tags).attr("id", "id_tags");

			var id = document.createElement("input");
			$(id).attr("type", "hidden");
			$(id).attr("name", "id");
			$(id).attr("value", taskId);

			//var submit = document.createElement("button");
			var submit = $('<button type="submit"/>');
			$(submit).text("Save");
			$(submit).attr("id", "id_submit");
			$(submit).attr("name", "submit");
			$(submit).attr("value", "Save");

			var close = document.createElement("a");
			$(close).text("Cancel");
			$(close).attr("href", "#");
			$(close).css("margin-left", "2em");
			$(close).click(function() {
				$(div).remove();
				$(originalData).show();
				return false;
			});

			var div = document.createElement("div");
			$(div).addClass("add-task");

			var form = document.createElement("form");
			$(form).attr("method", "post");
			$(form).attr("action", "/edit-task/");

			var firstRow = document.createElement("div");
			var secondRow = document.createElement("div");
			var thirdRow = document.createElement("div");

			$(firstRow).append(task);
			$(firstRow).append(estimated);
			$(firstRow).append(real);
			$(secondRow).append(tags);
			$(thirdRow).append(submit);
			$(thirdRow).append(close);
			$(thirdRow).append(id);

			$(form).append(firstRow);
			$(form).append(secondRow);
			$(form).append(thirdRow);

			$(div).append(form);

			$(originalData).hide();
			$(originalData).parent().append(div);

			$.getJSON("/fetch-task-json/", {"id": taskId},
				function(data) {
					if(data._id) {
						$(task).attr("value", data.name);
						$(estimated).attr("value", data.estimations);
						$(real).attr("value", data.real);
						$(tags).attr("value", data.tags);
					}
				});
		};

		$("#task-listing tbody tr").each(function () {
			var cell = $(this).find(".editable-task");

			$(cell).find("a").click(function(e) {
				e.preventDefault();
				editTaskBind(cell);
			});
		});
	}

	var newTaskFound = $(this).find("section.add-task");
	if (newTaskFound && newTaskFound.length != 0) {
		$('input[type="text"]').addClass("idle");
		$('input[type="text"]').focus(function() {
			$(this).removeClass("idle").addClass("focused");
			if (this.value == this.defaultValue) {
				this.value = '';
			}
			if (this.value != this.defaultValue) {
				this.select();
			}
		});

		$('input[type="text"]').blur(function() {
			if ($.trim(this.value) == '') {
				this.value = (this.defaultValue ? this.defaultValue : '');
				$(this).removeClass("focused").addClass("idle");
			}
		});
	}
});
