import { Component, EventEmitter, Input, Output } from '@angular/core';

@Component({
  selector: 'app-restultados-votacion',
  templateUrl: './resultados-votacion.component.html',
  styleUrls: ['./resultados-votacion.component.scss']
})
export class ResultadosVotacionComponent {

  @Input() items: any[] | undefined;
  @Input() titleEncuesta: string = '';
  @Output() closeModal = new EventEmitter<void>();

  onCloseModal() {
    this.closeModal.emit();
  }

}
