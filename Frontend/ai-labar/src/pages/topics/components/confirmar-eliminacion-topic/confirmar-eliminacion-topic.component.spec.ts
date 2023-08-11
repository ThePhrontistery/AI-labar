import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ConfirmarEliminacionTopicComponent } from './confirmar-eliminacion-topic.component';

describe('ConfirmarEliminacionTopicComponent', () => {
  let component: ConfirmarEliminacionTopicComponent;
  let fixture: ComponentFixture<ConfirmarEliminacionTopicComponent>;
  let mockMatDialogRef: jasmine.SpyObj<MatDialogRef<ConfirmarEliminacionTopicComponent>>;

  beforeEach(() => {
    mockMatDialogRef = jasmine.createSpyObj(['close']);

    TestBed.configureTestingModule({
      declarations: [ConfirmarEliminacionTopicComponent],
      providers: [
        { provide: MatDialogRef, useValue: mockMatDialogRef },
        { provide: MAT_DIALOG_DATA, useValue: {} }
      ]
    }).compileComponents();

    fixture = TestBed.createComponent(ConfirmarEliminacionTopicComponent);
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
