import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ModalVotacionComponent } from './modal-votacion.component';
import { ModalVotacionService } from './modal-votacion.service';
import { CookieService } from 'ngx-cookie-service';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MatDialogModule } from '@angular/material/dialog';
import { MatCheckboxModule } from '@angular/material/checkbox';

describe('ModalVotacionComponent', () => {
  let component: ModalVotacionComponent;
  let fixture: ComponentFixture<ModalVotacionComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ModalVotacionComponent],
      providers: [ModalVotacionService, CookieService],
      imports: [HttpClientTestingModule,
        MatCheckboxModule,MatDialogModule]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ModalVotacionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should close modal on cancel button click', () => {
    const fixture = TestBed.createComponent(ModalVotacionComponent);
    const component = fixture.componentInstance;
    component.isOpen = true;
    component.isSurveyOpinionSimple = true;
    fixture.detectChanges();

    spyOn(component, 'closeModal');

    const cancelButton = fixture.nativeElement.querySelector('.wizard-navigation button[mat-button][color="accent"]');

    cancelButton.click();
    expect(component.closeModal).toHaveBeenCalled();

  });

  it('should render title', () => {
    component.isOpen = true;
    component.isSurveyOpinionSimple = true;
    component.title = 'Test Title';
    fixture.detectChanges();

    const titleElement: HTMLElement = fixture.nativeElement.querySelector('mat-dialog-actions');
    expect(titleElement.textContent).toContain('Test Title');
  });

  it('should select an option', () => {
    // Simulate user interaction
    component.isOpen = true;
    component.isSurveyOpinionMultiple = true;
    component.valuesVoting = ['Option 1', 'Option 2'];
    fixture.detectChanges();

    const checkbox: HTMLInputElement = fixture.nativeElement.querySelector('input[type="checkbox"]');
    expect(component.voteSurvey.length).toBe(0);

    checkbox.click();
    fixture.detectChanges();

    expect(component.voteSurvey.length).toBe(1);
    expect(component.voteSurvey[0]).toBe('Option 1');
  });

});
