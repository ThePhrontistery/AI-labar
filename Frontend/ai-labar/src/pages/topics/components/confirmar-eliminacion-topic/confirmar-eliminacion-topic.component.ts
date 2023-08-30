import { Component, Inject } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

/**
 * Component to display a topic deletion confirmation dialog.
 */
@Component({
  selector: 'app-confirmar-eliminacion-topic',
  templateUrl: './confirmar-eliminacion-topic.component.html',
  styleUrls: ['./confirmar-eliminacion-topic.component.scss']
})
export class ConfirmarEliminacionTopicComponent {

  /**
   * Component builder.
   *
   * @param dialogRef Reference to the dialog box.
   * @param data Data passed to the dialog box.
   */
  constructor(
    public dialogRef: MatDialogRef<ConfirmarEliminacionTopicComponent>,
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
