import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';

@Component({
  selector: 'app-paso-uno',
  templateUrl: './paso-uno.component.html',
  styleUrls: ['./paso-uno.component.scss']
})
export class PasoUnoComponent implements OnInit {

  @Input() sharedData: any;

  imagenes = [
    { nombre: 'Valores\n(Selección simple/multiple)', ruta: 'assets/images/imageUno.png' },
    { nombre: 'Valores con imagen\n(Selección simple/multiple)', ruta: 'assets/images/imageCuatro.png' },
    { nombre: 'Emojis\n(Selección simple)', ruta: 'assets/images/imageDos.jpg' },
    { nombre: 'Empleado\n(Selección simple)', ruta: 'assets/images/imageTres.png' }
  ];

  imagenSeleccionada = {ruta : null, nombre: null};

  @Output() metodoPadreInvocado = new EventEmitter<any>();

  constructor() { }

  ngOnInit(): void {
  }

  seleccionarImagen(imagen: any){
    this.imagenSeleccionada.ruta = imagen.ruta;
    this.imagenSeleccionada.nombre = imagen.nombre;
    this.llamarMetodoPadre();
  }

  llamarMetodoPadre() {
    this.metodoPadreInvocado.emit(this.imagenSeleccionada);
  }


}
