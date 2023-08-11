import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { Emoji } from '../../../interfaces/emoji.model';
import { AnyadirGruposTopicComponent } from '../../../anyadir-grupos-topic/anyadir-grupos-topic.component';
import { MatDialog } from '@angular/material/dialog';
import { AnyadirCandidatosTopicComponent } from '../../../anyadir-candidatos-topic/anyadir-candidatos-topic.component';
import { DateAdapter, MAT_DATE_FORMATS, MAT_DATE_LOCALE } from '@angular/material/core';
import { MomentDateAdapter } from '@angular/material-moment-adapter';

export const MY_FORMATS = {
  parse: {
    dateInput: 'DD/MM/YYYY',
  },
  display: {
    dateInput: 'DD/MM/YYYY',
    monthYearLabel: 'MMM YYYY',
    dateA11yLabel: 'LL',
    monthYearA11yLabel: 'MMMM YYYY',
  },
};

@Component({
  selector: 'app-paso-dos',
  templateUrl: './paso-dos.component.html',
  styleUrls: ['./paso-dos.component.scss'],
  providers: [{ provide: DateAdapter, useClass: MomentDateAdapter, deps: [MAT_DATE_LOCALE] },
  { provide: MAT_DATE_FORMATS, useValue: MY_FORMATS }]
})

export class PasoDosComponent implements OnInit, OnDestroy {

  @Input() sharedData: any;

  @Input() imagenSeleccionada: any;

  topicOpinion = "Checkbox";
  topicValoracion = "Emoji";
  topicVotacion = "Empleado";
  topicImagenTexto = "ImagenTexto";

  isEncuestaOpinion = false;
  isEncuestaVotacion = false;
  isEncuestaValoracion = false;
  isEncuestaImagenTexto = false;

  valorTextbox: string = '';

  valorEncuesta1: string = '';
  valorEncuesta2: string = '';
  valorEncuesta3: string = '';
  valorEncuesta4: string = '';

  valorImagenTexto1: string = '';
  valorImagenTexto2: string = '';
  valorImagenTexto3: string = '';
  valorImagenTexto4: string = '';

  emojis: Emoji[] = [
    { id: 1, icon: '😄', name: 'Emoji 1', selected: false },
    { id: 2, icon: '🙂', name: 'Emoji 2', selected: false },
    { id: 3, icon: '😐', name: 'Emoji 3', selected: false },
    { id: 4, icon: '😔', name: 'Emoji 4', selected: false },
    { id: 5, icon: '😭', name: 'Emoji 5', selected: false }
  ];

  selectedEmoji: Emoji | null = null;

  objects: any[] = [];

  objectsToBack: string[] = [];

  selectedGender: string = ''; // Puede ser 'male' o 'female'
  textToAdd: string = '';
  imageUrl: string = '';
  sexo: string = '';

  users: string[] = [];
  selectedGroup: string[] = [];

  fechaCierre: string = "";

  selectedFiles: { [key: number]: File | null } = {};
  selectedFilesBase64: string[] = [];

  selectedType: string = 'simple';

  usersCandidatos: string[] = [];
  selectedGroupCandidatos: string[] = [];
  minDate:Date;
  // Función para filtrar fechas anteriores al día actual
  dateFilter = (date: Date | null): boolean => {
    if (date === null) {
      return false; // Otra opción sería permitir todas las fechas si date es null
    }
    
    const currentDate = new Date();
    return date >= currentDate;
  }

  constructor(private dialog: MatDialog) { 
    this.minDate = new Date();}

  ngOnInit(): void {
    this.opcionesAMostrar();
  }

  opcionesAMostrar(){
    if(this.imagenSeleccionada.nombre == this.topicOpinion) {
      this.isEncuestaOpinion = true;
    } else if (this.imagenSeleccionada.nombre == this.topicValoracion){
      this.isEncuestaValoracion = true;
    } else if(this.imagenSeleccionada.nombre == this.topicVotacion) {
      this.isEncuestaVotacion = true;
    } else if(this.imagenSeleccionada.nombre == this.topicImagenTexto){
      this.isEncuestaImagenTexto = true;
    }
  }

  quitarValoresOpciones(){
    this.isEncuestaOpinion = false;
    this.isEncuestaValoracion = false;
    this.isEncuestaVotacion = false;
    this.isEncuestaImagenTexto = false;
  }

  openAnyadirParticipantes() {
    const dialogRef = this.dialog.open(AnyadirGruposTopicComponent, {
      width: '400px',
      data: {} // Puedes pasar datos al diálogo si es necesario
    });

    dialogRef.afterClosed().subscribe(result => {
      // Aquí puedes realizar acciones después de cerrar el diálogo si es necesario
      console.log('Diálogo cerrado');
    });

    dialogRef.afterClosed().subscribe(result => {
      this.selectedGroup = result.grupoSeleccionado;
      this.users = result.usuariosSeleccionados;
    });
  }

  onDateSelected(event: any) {
    // Aquí puedes obtener la fecha seleccionada del evento y enviarla mediante tu web service (ws)
    this.fechaCierre = this.formatearFecha(event.value);
    const isFechaValida = this.esFechaMayorQueActual(this.fechaCierre);
    if(!isFechaValida){
      this.fechaCierre='';
      event.target.value = '';
    }
   // console.log('Fecha seleccionada:',  this.fechaCierre);
   // console.log('Fecha valida:', isFechaValida);

    // Llama a tu web service (ws) para enviar la fecha seleccionada
    // Escribe aquí el código para enviar la fecha al ws
  }

  formatearFecha(fechaString: string): string {
    // Crear un objeto Date con la fecha proporcionada
    const fecha = new Date(fechaString);

    // Obtener los componentes de la fecha
    const dia = fecha.getDate().toString().padStart(2, '0');
    const mes = (fecha.getMonth() + 1).toString().padStart(2, '0');
    const anio = fecha.getFullYear();

    // Formatear la fecha como "dd/mm/yyyy"
    const fechaFormateada = `${dia}/${mes}/${anio}`;
    return fechaFormateada;
  }

  esFechaMayorQueActual(fechaString: string): boolean {
    // Obtener la fecha actual del sistema
    const fechaActual = new Date();

    // Convertir la fecha ingresada a un objeto Date
    const partesFecha = fechaString.split('/');
    const dia = parseInt(partesFecha[0]);
    const mes = parseInt(partesFecha[1]) - 1; // Restamos 1 porque los meses en JavaScript van de 0 a 11
    const anio = parseInt(partesFecha[2]);
    const fechaIngresada = new Date(anio, mes, dia);

    // Comparar ambas fechas
    return fechaIngresada > fechaActual;
  }

  onFileSelected(event: any, imageNumber: number) {
    this.selectedFiles[imageNumber] = event.target.files[0];
    const file = this.selectedFiles[imageNumber];
    if (file) {
      const reader = new FileReader();
      reader.onloadend = () => {
        const base64data = reader.result?.toString()?.split(',')[1];
        if (base64data) {
          this.sendBase64ToWebService(base64data, imageNumber);
        }
      };
      reader.readAsDataURL(file);
    }
  }

  sendBase64ToWebService(base64data: string, imageNumber: number) {
    this.selectedFilesBase64[imageNumber] = "data:image/png;base64," + base64data;
  }

  openAnyadirCandidatos() {
    const dialogRef = this.dialog.open(AnyadirCandidatosTopicComponent, {
      width: '500px',
      data: {} // Puedes pasar datos al diálogo si es necesario
    });

    dialogRef.afterClosed().subscribe(result => {
      // Aquí puedes realizar acciones después de cerrar el diálogo si es necesario
      console.log('Diálogo cerrado');
    });

    dialogRef.afterClosed().subscribe(result => {
      this.selectedGroupCandidatos = result.grupoSeleccionado;
      this.usersCandidatos = result.usuariosSeleccionados;
      this.objectsToBack = this.usersCandidatos;
    });

  }

  ngOnDestroy(): void{
    this.quitarValoresOpciones();
  }

}
