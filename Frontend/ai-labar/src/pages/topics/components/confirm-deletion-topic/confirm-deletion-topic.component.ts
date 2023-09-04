import { Component, Inject } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

/**
 * Component to display a topic deletion confirmation dialog.
 */
@Component({
  selector: 'app-confirm-deletion-topic',
  templateUrl: './confirm-deletion-topic.component.html',
  styleUrls: ['./confirm-deletion-topic.component.scss']
})
export class ConfirmDeletionTopicComponent {

  /**
   * Component builder.
   *
   * @param dialogRef Reference to the dialog box.
   * @param data Data passed to the dialog box.
   */
  constructor(
    public dialogRef: MatDialogRef<ConfirmDeletionTopicComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) { }

  /**
   * Handles the click event on the cancel button.
   * Closes the dialog with the result "false".
   */
  onCancelClick(): void {
    this.dialogRef.close(false);
  }

  /**
   * Handles the click event on the confirm button.
   * Closes the dialog with the result "true".
   */
  onConfirmClick(): void {
    this.dialogRef.close(true);
  }

}
