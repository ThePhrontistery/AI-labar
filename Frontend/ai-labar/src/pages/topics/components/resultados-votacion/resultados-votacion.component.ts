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
   * List of items representing the voting results.
   */
  @Input() items: any[] | undefined;

  /**
   * Title of the survey or voting whose results are being displayed.
   */
  @Input() titleSurvey: string = '';

  /**
   * Event emitted when the closure of the results modal is requested.
   */
  @Output() closeModal = new EventEmitter<void>();

  /**
   * Method invoked when closing the results modal.
   * Emits the closeModal event to notify the parent component.
   */
  onCloseModal() {
    this.closeModal.emit();
  }
}
