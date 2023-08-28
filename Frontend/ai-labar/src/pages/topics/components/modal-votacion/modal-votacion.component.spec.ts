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
    // Crear el componente y detectar cambios
    const fixture = TestBed.createComponent(ModalVotacionComponent); // Reemplaza "TuComponente" con el nombre real de tu componente
    const component = fixture.componentInstance;
    component.isOpen = true;
    component.isEncuestaOpinionSimple = true;
    fixture.detectChanges();

    // Espiar el método closeModal
    spyOn(component, 'closeModal');

    // Intentar encontrar el botón y hacer clic en él
    const cancelButton = fixture.nativeElement.querySelector('.wizard-navigation button[mat-button][color="accent"]');

    cancelButton.click();
    expect(component.closeModal).toHaveBeenCalled();

  });

  it('should render title', () => {    
    component.isOpen = true;
    component.isEncuestaOpinionSimple = true;
    component.title = 'Test Title';
    fixture.detectChanges();
    
    const titleElement: HTMLElement = fixture.nativeElement.querySelector('mat-dialog-actions');
    expect(titleElement.textContent).toContain('Test Title');
  });

  it('should select an option', () => {
    // Simulate user interaction
    component.isOpen = true;
    component.isEncuestaOpinionMultiple = true;
    component.valoresVotacion = ['Option 1', 'Option 2'];
    fixture.detectChanges();

    const checkbox: HTMLInputElement = fixture.nativeElement.querySelector('input[type="checkbox"]'); 
    expect(component.votoEncuesta.length).toBe(0);

    checkbox.click(); 
    fixture.detectChanges(); 

    expect(component.votoEncuesta.length).toBe(1);
    expect(component.votoEncuesta[0]).toBe('Option 1');
  });

  // Add more test cases for other functionality
});
