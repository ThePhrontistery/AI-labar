/**
 * Component to display the voting results.
 */
import { Component, EventEmitter, Input, Output } from '@angular/core';

@Component({
  selector: 'app-restultados-votacion',
  templateUrl: './resultados-votacion.component.html',
  styleUrls: ['./resultados-votacion.component.scss'],
})
export class ResultadosVotacionComponent {
  /**
   * List of elements that represent the voting results.
   */
  @Input() items: any[] | undefined;

  /**
   * Title of the poll or voting whose results are displayed.
   */
  @Input() titleSurvey: string = '';

  /**
   * Event emitted when the results modal is requested to close.
   */
  @Output() closeModal = new EventEmitter<void>();

  /**
   * Method invoked when closing the results modal.
   * Emit the `closeModal` event to notify the parent component.
   */
  onCloseModal() {
    this.closeModal.emit();
  }
}
