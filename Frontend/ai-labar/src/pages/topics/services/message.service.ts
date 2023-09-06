import { Injectable } from '@angular/core';
import { MatSnackBar } from '@angular/material/snack-bar';
import { ErrorMessageComponent } from '../components/error-message/error-message.component';
import { MatDialog } from '@angular/material/dialog';

@Injectable({
  providedIn: 'root'
})
export class MessageService {

  constructor(private snackBar: MatSnackBar, private dialog: MatDialog) { }

  showSuccessMessage(message: string) {
    this.snackBar.open(message, '', {
      duration: 2000,
      verticalPosition: 'top',
      panelClass: ['success-message', 'centered-text']
    });
  }

  showErrorMessage(message: string) {
      this.dialog.open(ErrorMessageComponent, {
        width: '300px',
        data: { message: message },
        disableClose: true,
        panelClass: 'borderless-dialog',
      });
    }
}
