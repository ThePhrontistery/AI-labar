import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatDialog, MatDialogRef } from '@angular/material/dialog';
import { MomentDateAdapter } from '@angular/material-moment-adapter';
import { MatNativeDateModule } from '@angular/material/core';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { FormsModule } from '@angular/forms';
import { of } from 'rxjs';

import { PasoDosComponent, MY_FORMATS } from './paso-dos.component';
import { AnyadirGruposTopicComponent } from '../../../anyadir-grupos-topic/anyadir-grupos-topic.component';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input'

describe('PasoDosComponent', () => {
  let component: PasoDosComponent;
  let fixture: ComponentFixture<PasoDosComponent>;
  let mockMatDialog: jasmine.SpyObj<MatDialog>;

  beforeEach(async () => {
    mockMatDialog = jasmine.createSpyObj('MatDialog', ['open']);

    await TestBed.configureTestingModule({
      declarations: [PasoDosComponent],
      imports: [
        BrowserAnimationsModule,
        FormsModule,
        MatDatepickerModule,MatFormFieldModule,MatInputModule,//importaciones
        MatNativeDateModule
      ],
      providers: [
        { provide: MatDialog, useValue: mockMatDialog },
        { provide: MomentDateAdapter, useClass: MomentDateAdapter },
        { provide: MY_FORMATS, useValue: MY_FORMATS }
      ]
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(PasoDosComponent);
    component = fixture.componentInstance;
    component.imagenSeleccionada = { nombre: 'Opinion' }; // Aqui configuras el valor de imagenSeleccionada
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should open AnyadirParticipantes dialog', () => {
    const mockDialogRef: MatDialogRef<any, any> = {
      afterClosed: () => of({ grupoSeleccionado: [], usuariosSeleccionados: [] })
    } as MatDialogRef<any, any>;

    mockMatDialog.open.and.returnValue(mockDialogRef);

    component.openAnyadirParticipantes();

    expect(mockMatDialog.open).toHaveBeenCalledWith(AnyadirGruposTopicComponent, {
      width: '400px',
      data: {}
    });
  });

  it('should set selectedGroup and users on AnyadirParticipantes dialog close', () => {
    const mockDialogRef = jasmine.createSpyObj('MatDialogRef', ['afterClosed']);
    mockMatDialog.open.and.returnValue(mockDialogRef);
    mockDialogRef.afterClosed.and.returnValue(of({ grupoSeleccionado: ['Group1'], usuariosSeleccionados: ['User1'] }));

    component.openAnyadirParticipantes();

    expect(component.selectedGroup).toEqual(['Group1']);
    expect(component.users).toEqual(['User1']);
  });

  it('should update fechaCierre on date selection', () => {
    const event = { value: new Date(2023, 8, 8) };
    component.onDateSelected(event);
    expect(component.fechaCierre).toBe('08/09/2023');
  });

  it('should correctly format fechaString', () => {
    const formattedDate = component.formatearFecha('2023-08-28');
    expect(formattedDate).toBe('28/08/2023');
  });

  // More tests can be added for other component methods and functionalities
});
