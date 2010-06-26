/* Copyright (c) 2010 Jorge Gajon
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */
$(document).ready(function() {
	// PULL DOWN CALENDAR TO GO TO A SPECIFIC DATE
	$('#id_datepicker').datepicker({
		dateFormat: "yy-mm-dd",
		buttonText: "[select date]",
		buttonImageOnly: false,
		showOn: "button",
		changeMonth: true,
		changeYear: true,
		//showButtonPanel: true,
		//closeText: "Cancel",
		onSelect: function(dateText, inst) {
			window.location = "/listing/?d=" + dateText;
		}
	});

	// SET UP THE TABLESORTER PLUGIN.
	var tableFound = $(this).find("#task-listing").find("tbody").find("tr");
	if (tableFound && tableFound.length != 0) {
		$("#task-listing").tablesorter({sortList: [[0,0]]});

		// WE ALSO ADD BINDINGS TO MAKE THE ROW BACKGROUND COLOR CHANGE
		// WHEN THE MOUSE MOVES OVER IT.
		//
		// WE ALSO SHOW OR HIDE AN "edit" LINK TO SHOW A FORM TO UPDATE
		// THE TASK INFORMATION. SEE THE "editTaskBind" FUNCTION BELOW.
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

		// WHEN THE "edit" LINK IS CLICKED, WE BUILD AND SHOW
		// A FORM TO UPDATE THE TASK INFORMATION.
		var editTaskBind = function (originalData) {
			var taskId = $(originalData).find("a").attr("href");
			taskId = taskId.substr(15);

			// The task name
			var task = document.createElement("input");
			$(task).attr("type", "text");
			$(task).attr("name", "task");
			$(task).attr("id", "id_task");

			// The estimations
			var estimated = document.createElement("input");
			$(estimated).attr("type", "text");
			$(estimated).attr("name", "estimations");
			$(estimated).attr("id", "id_estimations");

			// The real number of pomodoros
			var real = document.createElement("input");
			$(real).attr("type", "text");
			$(real).attr("name", "real");
			$(real).attr("id", "id_real");

			// The list of tags
			var tags = document.createElement("input");
			$(tags).attr("type", "text");
			$(tags).attr("name", "tags");
			$(tags).attr("id", "id_tags");

			// The id of the task
			var id = document.createElement("input");
			$(id).attr("type", "hidden");
			$(id).attr("name", "id");
			$(id).attr("value", taskId);

			// We use the $('<button>') syntax to work around
			// an Internet Explorer bug.
			var submit = $('<button type="submit"/>');
			$(submit).text("Save");
			$(submit).attr("id", "id_submit");
			$(submit).attr("name", "submit");
			$(submit).attr("value", "Save");

			// Cancel link
			var close = document.createElement("a");
			$(close).text("Cancel");
			$(close).attr("href", "#");
			$(close).css("margin-left", "2em");
			$(close).click(function(e) {
				$(div).remove();
				$(originalData).show();
				e.preventDefault();
			});

			// A link to delete the task.
			// When click will show a message with a
			// button so that the user can confirm.
			var deltask = document.createElement("a");
			$(deltask).text("Delete task");
			$(deltask).attr("href", "#");
			$(deltask).css("margin-left", "2em");
			$(deltask).click(function(e) {
				var formdelete = document.createElement("form");
				$(formdelete).attr("method", "post");
				$(formdelete).attr("action", "/delete-this-task/");

				var p1 = document.createElement("p");
				var p2 = document.createElement("p");
				$(p1).text("Are you sure you want to delete this task?")
				$(p2).text("This is undoable.")

				var yes = $('<button type="submit"/>');
				$(yes).text("Yes");
				$(yes).attr("name", "submit");
				$(yes).attr("value", "Yes");
				$(yes).css("margin-left", "2em");
				$(p2).append(yes);

				var id = document.createElement("input");
				$(id).attr("type", "hidden");
				$(id).attr("name", "id");
				$(id).attr("value", taskId);

				$(formdelete).append(id);
				$(formdelete).append(p1);
				$(formdelete).append(p2);

				var divdelete = document.createElement("div");
				$(divdelete).css("padding", "1em");
				$(divdelete).append(formdelete);

				$(this).parent().append(divdelete);

				e.preventDefault();
			});

			// Now groups all the elements togheter.
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
			$(thirdRow).append(deltask);
			$(thirdRow).append(id);

			$(form).append(firstRow);
			$(form).append(secondRow);
			$(form).append(thirdRow);

			$(div).append(form);

			$(originalData).hide();
			$(originalData).parent().append(div);

			// Populate the input fields by asking the server for the
			// information. I did this out of laziness, as it would've been
			// pretty tedious to parse or mark the information from the
			// original rendered html. And editing a task should be a rare
			// action so the extra roundtrip is not that bad.
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

	// AFTER THE TASK LISTING THERE'S A SECTION WITH A FORM TO
	// ADD NEW TASKS. HERE WE SET UP BINDINGS TO STYLE THE TEXT
	// INSIDE THE FORM INPUTS IF THE USER HAS NOT ENTERED SOMETHING.
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
