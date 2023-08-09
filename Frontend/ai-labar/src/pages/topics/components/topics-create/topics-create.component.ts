import { AfterViewInit, Component, OnInit, ViewChild } from '@angular/core';
import { PasoDosComponent } from './pasos/paso-dos/paso-dos.component';
import { CookieService } from 'ngx-cookie-service';
import { TopicsCreateService } from './topics-create.service';
import { Router } from '@angular/router';
import { MatDialog } from '@angular/material/dialog';

@Component({
  selector: 'app-topics-create',
  templateUrl: './topics-create.component.html',
  styleUrls: ['./topics-create.component.scss']
})
export class TopicsCreateComponent implements OnInit {

  @ViewChild(PasoDosComponent)
  componenteHijo: PasoDosComponent = new PasoDosComponent(this.dialog);

  currentStep = 1;
  sharedData: any = {};
  imagenSeleccionadaPasoUno: any = {};
  tituloEncuesta: string = "";
  opcionesEncuesta: any[] = [];
  typeTM = "TEXT_MULTIPLE";
  typeTS = "TEXT_SINGLE";
  typeIM = "IMAGE_MULTIPLE";
  typeIS = "IMAGE_SINGLE";
  typeRating = "RATING";
  typeAs= "AS";
  resultadoOpciones: string = '';
  tipoSeleccionado: string = '';

  members: string[] = [];

  constructor(private cookie: CookieService, private topicsCreateService: TopicsCreateService, private router: Router, private dialog: MatDialog) { }

  ngOnInit(): void {
  }

  goToNextStep() {
    this.currentStep++;
  }

  goToPreviousStep() {
    this.currentStep--;
  }

  metodoPadre(objeto: any) {
    this.imagenSeleccionadaPasoUno = objeto;
    this.goToNextStep();
  }

  cancelarGuardar(){
    this.router.navigate(['/topics/topics-list']);
  }

  guardarEncuesta() {
    this.tituloEncuesta = this.componenteHijo.valorTextbox;
    if(this.componenteHijo.isEncuestaOpinion){
      this.opcionesEncuesta = [];
      if(this.componenteHijo.selectedType == "simple"){
        this.tipoSeleccionado = this.typeTS;
      }else {
        this.tipoSeleccionado = this.typeTM;
      }
      this.opcionesEncuesta.push({option: this.componenteHijo.valorEncuesta1});
      this.opcionesEncuesta.push({option: this.componenteHijo.valorEncuesta2});
      if(this.componenteHijo.valorEncuesta3 != null && this.componenteHijo.valorEncuesta3 != ""){
        this.opcionesEncuesta.push({option: this.componenteHijo.valorEncuesta3});
      }
      if(this.componenteHijo.valorEncuesta4 != null && this.componenteHijo.valorEncuesta4 != ""){
        this.opcionesEncuesta.push({option: this.componenteHijo.valorEncuesta4});
      }
    }else if(this.componenteHijo.isEncuestaValoracion){
      this.tipoSeleccionado = this.typeRating;
      this.opcionesEncuesta = [];
      for (const emoji of this.componenteHijo.emojis) {
          const option = {option: emoji.id.toString()}
          this.opcionesEncuesta.push(option);
      }
    }
    else if(this.componenteHijo.isEncuestaVotacion){
      this.tipoSeleccionado = this.typeAs;
      this.opcionesEncuesta = [];
      for (const object of this.componenteHijo.objectsToBack) {
          const option = {option: object}
          this.opcionesEncuesta.push(option);
      }
    } else if(this.componenteHijo.isEncuestaImagenTexto){
      if(this.componenteHijo.selectedType == "simple"){
        this.tipoSeleccionado = this.typeIS;
      }else {
        this.tipoSeleccionado = this.typeIM;
      }
      this.opcionesEncuesta = [];
      this.opcionesEncuesta.push({image: this.componenteHijo.selectedFilesBase64[1], option: this.componenteHijo.valorImagenTexto1});
      this.opcionesEncuesta.push({image: this.componenteHijo.selectedFilesBase64[2], option: this.componenteHijo.valorImagenTexto2});
      if(this.componenteHijo.valorImagenTexto3 != null && this.componenteHijo.valorImagenTexto3 != ""){
        this.opcionesEncuesta.push({image: this.componenteHijo.selectedFilesBase64[3], option: this.componenteHijo.valorImagenTexto3});
      }
      if(this.componenteHijo.valorImagenTexto4 != null && this.componenteHijo.valorImagenTexto4 != ""){
        this.opcionesEncuesta.push({image: this.componenteHijo.selectedFilesBase64[4], option: this.componenteHijo.valorImagenTexto4});
      }
    }

    if(this.componenteHijo.users.length > 0){
      this.members = this.componenteHijo.users
    }
    this.createTopics();
  }

  concatenarConComas(strings: string[]): string {
    return strings.join(', ');
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

  validaciónValores(): boolean {
    const isValid = false;
    if(this.componenteHijo.fechaCierre){
      return true;
    }else {
      return false;
    }
    return isValid;
  }


  createTopics(){
    if(this.validaciónValores()){
      const createTopicsBody = {
        "title": this.tituloEncuesta,
        "type": this.tipoSeleccionado,
        "question": "prueba",
        "options": this.opcionesEncuesta,
        "user": this.cookie.get("user"),
        "members": this.members,
        "closeDate": this.componenteHijo.fechaCierre,
        "token": this.cookie.get("token")
    }
      this.topicsCreateService.createTopics(createTopicsBody).subscribe(
          response => {
            if (response){
              this.router.navigate(['/topics/topics-list']);
            }
          },
          error=> {
            alert("Se produjo un error al crear el topic, compruebe que todo los datos estan rellenos incluido los participantes.")
          }
      );
    }
  }
}
