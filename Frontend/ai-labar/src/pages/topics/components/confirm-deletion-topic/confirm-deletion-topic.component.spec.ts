import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ConfirmDeletionTopicComponent } from './confirm-deletion-topic.component';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('ConfirmDeletionTopicComponent', () => {
  let component: ConfirmDeletionTopicComponent;
  let fixture: ComponentFixture<ConfirmDeletionTopicComponent>;
  let mockMatDialogRef: jasmine.SpyObj<
    MatDialogRef<ConfirmDeletionTopicComponent>
  >;

  beforeEach(() => {
    mockMatDialogRef = jasmine.createSpyObj(['close']);

    TestBed.configureTestingModule({
      declarations: [ConfirmDeletionTopicComponent],
      imports: [HttpClientTestingModule, TranslateModule.forRoot()],
      providers: [TranslateService,
        { provide: MatDialogRef, useValue: mockMatDialogRef },
        { provide: MAT_DIALOG_DATA, useValue: {} },
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(ConfirmDeletionTopicComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should close dialog with false value on cancel click', () => {
    component.onCancelClick();
    expect(mockMatDialogRef.close).toHaveBeenCalledWith(false);
  });

  it('should close dialog with true value on confirm click', () => {
    component.onConfirmClick();
    expect(mockMatDialogRef.close).toHaveBeenCalledWith(true);
  });
});
