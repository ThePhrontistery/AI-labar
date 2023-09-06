import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ErrorMessageComponent } from './error-message.component';
import { MAT_DIALOG_DATA, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('ErrorMessageComponent', () => {
  let component: ErrorMessageComponent;
  let fixture: ComponentFixture<ErrorMessageComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ErrorMessageComponent ],
      imports: [MatDialogModule,HttpClientTestingModule, TranslateModule.forRoot()],
      providers: [TranslateService,
        { provide: MatDialogRef, useValue: {} }, // Puedes proporcionar un objeto vacío para MatDialogRef
        { provide: MAT_DIALOG_DATA, useValue: {} } // Puedes proporcionar un objeto vacío para MAT_DIALOG_DATA
      ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ErrorMessageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
