import { TestBed, inject } from '@angular/core/testing';

import { MessageService } from './message.service';
import { MatSnackBar, MatSnackBarModule } from '@angular/material/snack-bar';
import { MatDialog, MatDialogModule,MatDialogRef  } from '@angular/material/dialog';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { of } from 'rxjs';
import { ErrorMessageComponent } from '../components/error-message/error-message.component';

class MatDialogRefMock {
  afterClosed() {
    return of(true);
  }
  close() {}
}

describe('MessageService', () => {
  let service: MessageService;
  let messageService: MessageService;
  let snackBar: MatSnackBar;
  let dialog: MatDialog;

  beforeEach(() => {
    TestBed.configureTestingModule({
      providers: [MessageService, MatSnackBar, MatDialog,      
        { provide: MatDialogRef, useClass: MatDialogRefMock },],
      imports: [BrowserAnimationsModule,MatDialogModule,MatSnackBarModule]
    });
    service = TestBed.inject(MessageService);
  });
  beforeEach(inject(
    [MessageService, MatSnackBar, MatDialog],
    (service: MessageService, snack: MatSnackBar, dlg: MatDialog) => {
      messageService = service;
      snackBar = snack;
      dialog = dlg;
    }
  ));

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should show success message using MatSnackBar', () => {
    spyOn(snackBar, 'open');
    const message = 'Success Message';
    messageService.showSuccessMessage(message);
    expect(snackBar.open).toHaveBeenCalledWith(message, '', {
      duration: 2000,
      verticalPosition: 'top',
      panelClass: ['success-message', 'centered-text'],
    });
  });

  it('should show error message using MatDialog', () => {
    spyOn(dialog, 'open').and.returnValue({
      afterClosed: () => of(true),
      close: () => {},
    } as MatDialogRef<unknown>);

    const message = 'Error Message';
    messageService.showErrorMessage(message);

    expect(dialog.open).toHaveBeenCalledWith(ErrorMessageComponent, {
      width: '300px',
      data: { message: message },
      disableClose: true,
      panelClass: 'borderless-dialog',
    });
  });
});
