import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';

@Component({
  selector: 'app-paso-uno',
  templateUrl: './paso-uno.component.html',
  styleUrls: ['./paso-uno.component.scss']
})
export class PasoUnoComponent implements OnInit {

  @Input() sharedData: any;

  imagenes = [
    { nombre: 'Checkbox', ruta: 'assets/images/imageUno.png' },
    { nombre: 'Emoji', ruta: 'assets/images/imageDos.jpg' },
    { nombre: 'Empleado', ruta: 'assets/images/imageTres.png' },
    { nombre: 'ImagenTexto', ruta: 'assets/images/imageCuatro.png' }
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
