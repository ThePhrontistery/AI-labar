import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';

@Component({
  selector: 'app-paso-uno',
  templateUrl: './paso-uno.component.html',
  styleUrls: ['./paso-uno.component.scss']
})
export class PasoUnoComponent implements OnInit {

  @Input() sharedData: any;

  imagenes = [
    { name: 'Valores\n(Selección simple/multiple)', route: 'assets/images/imageUno.png', code:'VAL' },
    { name: 'Valores con imagen\n(Selección simple/multiple)', route: 'assets/images/imageCuatro.png', code:'VCI' },
    { name: 'Emojis\n(Selección simple)', route: 'assets/images/imageDos.jpg', code:'EMO' },
    { name: 'Empleado\n(Selección simple)', route: 'assets/images/imageTres.png', code:'EMP' }
  ];

  imagenSeleccionada = {route : null, name: null, code: null};

  @Output() metodoPadreInvocado = new EventEmitter<any>();

  constructor() { }

  ngOnInit(): void {
  }

  seleccionarImagen(imagen: any){
    this.imagenSeleccionada.route = imagen.route;
    this.imagenSeleccionada.name = imagen.name;
    this.imagenSeleccionada.code = imagen.code;
    this.llamarMetodoPadre();
  }

  llamarMetodoPadre() {
    this.metodoPadreInvocado.emit(this.imagenSeleccionada);
  }


}
