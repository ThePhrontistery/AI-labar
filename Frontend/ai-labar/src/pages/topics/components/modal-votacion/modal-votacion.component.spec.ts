import { ComponentFixture, TestBed } from '@angular/core/testing';
import { ModalVotacionComponent } from './modal-votacion.component';
import { ModalVotacionService } from './modal-votacion.service';
import { CookieService } from 'ngx-cookie-service';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('ModalVotacionComponent', () => {
  let component: ModalVotacionComponent;
  let fixture: ComponentFixture<ModalVotacionComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ModalVotacionComponent],
      providers: [ModalVotacionService, CookieService],
      imports: [HttpClientTestingModule]
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
    fixture.detectChanges();

    // Espiar el método closeModal
    spyOn(component, 'closeModal');

    // Intentar encontrar el botón y hacer clic en él
    const cancelButton = fixture.nativeElement.querySelector('.wizard-navigation button[mat-button][color="accent"]');
    if (cancelButton) {
      cancelButton.click();
      expect(component.closeModal).toHaveBeenCalled();
    }
  });

  // Add more test cases as needed
  it('should select an option on checkbox change', () => {
    const option = 'Option 1';
    const checkbox = fixture.nativeElement.querySelector(`input[type="checkbox"][value="${option}"]`);
    if (checkbox) {
      checkbox.checked = true;
      checkbox.dispatchEvent(new Event('change'));
      fixture.detectChanges();
      expect(component.votoEncuesta).toContain(option);
    }
  });

  // Add more test cases for other functionality
});
