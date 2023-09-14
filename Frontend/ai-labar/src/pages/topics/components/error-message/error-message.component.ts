import { Component, Inject } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

/**
 * Component for displaying error messages in a dialog box.
 */
@Component({
  selector: 'app-error-message',
  templateUrl: './error-message.component.html',
  styleUrls: ['./error-message.component.scss'],
})
export class ErrorMessageComponent {
  /**
   * Constructor of the ErrorMessageComponent component.
   *
   * @param {object} data - Error message data, including the message text.
   * @param {MatDialogRef<ErrorMessageComponent>} dialogRef - Reference to the current dialog box.
   */
  constructor(
    @Inject(MAT_DIALOG_DATA) public data: { message: string },
    private dialogRef: MatDialogRef<ErrorMessageComponent>
  ) {}

  /**
   * Close the current dialog box.
   */
  closeDialog(): void {
    this.dialogRef.close();
  }
}
