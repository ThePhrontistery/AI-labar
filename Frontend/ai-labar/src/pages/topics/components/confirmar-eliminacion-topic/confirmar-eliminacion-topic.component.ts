import { Component, Inject } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';

@Component({
  selector: 'app-confirmar-eliminacion-topic',
  templateUrl: './confirmar-eliminacion-topic.component.html',
  styleUrls: ['./confirmar-eliminacion-topic.component.scss']
})
export class ConfirmarEliminacionTopicComponent {

  constructor(
    public dialogRef: MatDialogRef<ConfirmarEliminacionTopicComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) { }

  onCancelClick(): void {
    this.dialogRef.close(false); // Cierra el diálogo con valor "false"
  }

  onConfirmClick(): void {
    this.dialogRef.close(true); // Cierra el diálogo con valor "true"
  }

}
