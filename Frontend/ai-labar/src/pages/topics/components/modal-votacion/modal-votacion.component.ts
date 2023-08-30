import { Component, EventEmitter, Input, OnChanges, Output } from '@angular/core';
import { ModalVotacionService } from './modal-votacion.service';
import { CookieService } from 'ngx-cookie-service';
import { Emoji } from '../interfaces/emoji.model';

@Component({
  selector: 'app-modal-votacion',
  templateUrl: './modal-votacion.component.html',
  styleUrls: ['./modal-votacion.component.scss']
})
export class ModalVotacionComponent implements OnChanges{

  @Input() isOpen: boolean = false;
  @Input() options: string[] = [];
  @Input() selectedOptions: string[] = [];
  @Input() title: string = '';
  @Input() idVotation: any;
  @Input() typeVotacion: string = '';
  @Input() isEnquestaVotacion: Boolean = false;
  @Input() isEnquestaValoracion: Boolean = false;
  @Input() isEncuestaOpinionSimple: Boolean = false;
  @Input() isEncuestaOpinionMultiple: Boolean = false;
  @Input() isEnquestaImagenTextoSimple: Boolean = false;
  @Input() isEnquestaImagenTextoMultiple: Boolean = false;
  @Output() onClose = new EventEmitter<void>();
  @Output() onOptionChange = new EventEmitter<string>();
  @Output() onItemChange = new EventEmitter<any>();

  votoEncuesta: string[] = [];
  selectedEmoji: Emoji | null = null;

  emojis: Emoji[] = [
    { id: 1, icon: '😄', name: 'Emoji 1', selected: false },
    { id: 2, icon: '🙂', name: 'Emoji 2', selected: false },
    { id: 3, icon: '😐', name: 'Emoji 3', selected: false },
    { id: 4, icon: '😔', name: 'Emoji 4', selected: false },
    { id: 5, icon: '😭', name: 'Emoji 5', selected: false }
  ];

  objeto: any | undefined;

  emojisVotacion: Emoji[] = [];

  itemsVotacion: any[] = [];

  selectedItem: any | null = null;

  selectedImagenTexto: any | null = null;

  valoresVotacion: any = []

  valoresVotacionImagenTexto: any = [];

  selectedOption: any | null = null;

  selectedImagenesTexto: any | null = null;

  constructor(private modalVotacionService: ModalVotacionService, private cookie: CookieService) { }

  ngOnChanges(): void {
    if(this.isEnquestaValoracion){
      //this.emojisVotacion = this.emojis.filter((objeto) => this.options.includes(objeto.id.toString()));
      for (let i = 0; i < this.options.length; i++) {
        this.objeto = this.options[i];
        const id = this.objeto.option;
        const emojisEncontrados = this.emojis.filter(obj => obj.id.toString() === id.toString());
       this.emojisVotacion.push(emojisEncontrados[0]);
      }
    }else if(this.isEnquestaVotacion){
      for(let i = 0; i < this.options.length; i++){
        this.objeto = this.options[i];
        const item = this.objeto.option;
        if(this.objeto.image == "" || this.objeto.image == null){
          this.objeto.image = "assets/images/questionMark.png";
        }
        const resultadoFinal = {text:item, imageSrc:this.objeto.image, id:i}
        this.itemsVotacion.push(resultadoFinal);
      }
    } else if(this.isEncuestaOpinionMultiple || this.isEncuestaOpinionSimple){
      for (let i = 0; i < this.options.length; i++) {
        this.objeto = this.options[i];
        const valor = this.objeto.option;
       this.valoresVotacion.push(valor);
      }
    } else if(this.isEnquestaImagenTextoSimple || this.isEnquestaImagenTextoMultiple){
      for (let i = 0; i < this.options.length; i++) {
        this.objeto = this.options[i];
        const valor = this.objeto.option;
        const resultadoFinal = {text:this.objeto.option, imageSrc:this.objeto.image, id:i}
        this.valoresVotacionImagenTexto.push(resultadoFinal);
      }
    }
  }

  closeModal(): void {
    this.itemsVotacion = [];
    this.votoEncuesta = [];
    this.selectedOptions = [];
    this.valoresVotacion = [];
    this.emojisVotacion = [];
    this.valoresVotacionImagenTexto = [];
    this.onClose.emit();
  }

  selectOption(option: string, event: any): void {
    if (event.target.checked) {
      this.votoEncuesta.push(option)
    this.onOptionChange.emit(option);
    } else {
      const index = this.votoEncuesta.indexOf(option);
      if (index !== -1) {
        this.votoEncuesta.splice(index, 1);
      }
    }
  }

  selectImagenesTexto(item: any, event: any): void {
    if (event.target.checked) {
      this.votoEncuesta.push(item.text)
    this.onItemChange.emit(item.text);
    } else {
      const index = this.votoEncuesta.indexOf(item.text);
      if (index !== -1) {
        this.votoEncuesta.splice(index, 1);
      }
    }
  }

  sendSelection(): void {
    const voteTopicBody = {
      "id": this.idVotation,
      "votation": this.votoEncuesta,
      "user": this.cookie.get("user"),
      "token": this.cookie.get("token")
  }
    this.modalVotacionService.voteTopics(voteTopicBody).subscribe(
      (response) => {
        console.log('Selección enviada con éxito:', response);
        this.closeModal();
      },
      (error) => {
        console.error('Error al enviar la selección:', error);
      }
    );
  }

  selectEmoji(emoji: Emoji): void {
    this.votoEncuesta = [];
    this.selectedEmoji = emoji;
    this.votoEncuesta.push(emoji.id.toString());
  }

  selectOpinion(option: any): void {
    this.votoEncuesta = [];
    this.selectedOption = option;
    this.votoEncuesta.push(option);
  }


  selectItem(item: any): void {
    this.votoEncuesta = [];
    this.selectedItem = item;
    this.votoEncuesta.push(this.selectedItem.text);
  }

  selectImagenTexto(item: any): void {
    this.votoEncuesta = [];
    this.selectedImagenTexto = item;
    this.votoEncuesta.push(this.selectedImagenTexto.text);
  }

}
