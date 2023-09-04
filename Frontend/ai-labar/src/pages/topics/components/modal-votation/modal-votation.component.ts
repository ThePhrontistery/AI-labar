/**
 * This component represents a voting modal that allows users to participate in various types of surveys.
 * Handles survey options, user selections, and sends voting data to the server.
 */

import {
  Component,
  EventEmitter,
  Input,
  OnChanges,
  OnDestroy,
  Output,
} from '@angular/core';
import { ModalVotationService } from './modal-votation.service';
import { CookieService } from 'ngx-cookie-service';
import { Emoji } from '../interfaces/emoji.model';
import { TranslateService } from '@ngx-translate/core';
import { Subject, takeUntil } from 'rxjs';

@Component({
  selector: 'app-modal-votation',
  templateUrl: './modal-votation.component.html',
  styleUrls: ['./modal-votation.component.scss'],
})
export class ModalVotationComponent implements OnChanges, OnDestroy {
  // Input properties for configuring the modal.
  @Input() isOpen: boolean = false;
  @Input() options: string[] = [];
  @Input() selectedOptions: string[] = [];
  @Input() title: string = '';
  @Input() idVotation: any;
  @Input() typeVoting: string = '';
  @Input() isSurveyVoting: Boolean = false;
  @Input() isSurveyRating: Boolean = false;
  @Input() isSurveyOpinionSimple: Boolean = false;
  @Input() isSurveyOpinionMultiple: Boolean = false;
  @Input() isSurveyImageTextSimple: Boolean = false;
  @Input() isSurveyImageTextMultiple: Boolean = false;

  // Output events
  @Output() onClose = new EventEmitter<void>();
  @Output() onOptionChange = new EventEmitter<string>();
  @Output() onItemChange = new EventEmitter<any>();

  // Arrays and variables for managing user selections
  voteSurvey: string[] = [];
  selectedEmoji: Emoji | null = null;
  selectedOption: any | null = null;
  selectedItem: any | null = null;
  selectedImagesText: any | null = null;

  // Array of emoji objects for rating surveys
  emojis: Emoji[] = [
    // Define emojis with id, icon, name, and selection status
    { id: 1, icon: '😄', name: 'Emoji 1', selected: false },
    { id: 2, icon: '🙂', name: 'Emoji 2', selected: false },
    { id: 3, icon: '😐', name: 'Emoji 3', selected: false },
    { id: 4, icon: '😔', name: 'Emoji 4', selected: false },
    { id: 5, icon: '😭', name: 'Emoji 5', selected: false },
  ];

  // Other variables
  object: any | undefined;
  emojisVoting: Emoji[] = [];
  itemsVoting: any[] = [];
  selectedImageText: any | null = null;
  valuesVoting: any = [];
  valuesVotingImageText: any = [];

  private ngUnsubscribe = new Subject();

  constructor(
    private modalVotationService: ModalVotationService,
    private cookie: CookieService,
    private translate: TranslateService
  ) {}

  ngOnDestroy(): void {
    this.ngUnsubscribe.complete();
  }

  /**
   * Reacts to changes in input properties and populates the corresponding arrays based on survey type.
   */
  ngOnChanges(): void {
    if (this.isSurveyRating) {
      for (let i = 0; i < this.options.length; i++) {
        this.object = this.options[i];
        const id = this.object.option;
        const emojisFound = this.emojis.filter(
          (obj) => obj.id.toString() === id.toString()
        );
        this.emojisVoting.push(emojisFound[0]);
      }
    } else if (this.isSurveyVoting) {
      for (let i = 0; i < this.options.length; i++) {
        this.object = this.options[i];
        const item = this.object.option;
        if (this.object.image == '' || this.object.image == null) {
          this.object.image = 'assets/images/questionMark.png';
        }
        const result = { text: item, imageSrc: this.object.image, id: i };
        this.itemsVoting.push(result);
      }
    } else if (this.isSurveyOpinionMultiple || this.isSurveyOpinionSimple) {
      for (let i = 0; i < this.options.length; i++) {
        this.object = this.options[i];
        const value = this.object.option;
        this.valuesVoting.push(value);
      }
    } else if (this.isSurveyImageTextSimple || this.isSurveyImageTextMultiple) {
      for (let i = 0; i < this.options.length; i++) {
        this.object = this.options[i];
        const result = {
          text: this.object.option,
          imageSrc: this.object.image,
          id: i,
        };
        this.valuesVotingImageText.push(result);
      }
    }
  }

  /**
   * Closes the modal and resets arrays and selections.
   */
  closeModal(isCancel: any): void {
    this.itemsVoting = [];
    this.voteSurvey = [];
    this.selectedOptions = [];
    this.valuesVoting = [];
    this.emojisVoting = [];
    this.valuesVotingImageText = [];
    this.onClose.emit(isCancel);
  }

  /**
   * Handles the selection of an option in different types of surveys.
   */
  selectOption(option: string, event: any): void {
    if (event.target.checked) {
      this.voteSurvey.push(option);
      this.onOptionChange.emit(option);
    } else {
      const index = this.voteSurvey.indexOf(option);
      if (index !== -1) {
        this.voteSurvey.splice(index, 1);
      }
    }
  }

  selectImagesText(item: any, event: any): void {
    if (event.target.checked) {
      this.voteSurvey.push(item.text);
      this.onItemChange.emit(item.text);
    } else {
      const index = this.voteSurvey.indexOf(item.text);
      if (index !== -1) {
        this.voteSurvey.splice(index, 1);
      }
    }
  }

  /**
   * Sends the user's selections to the server.
   */
  sendSelection(): void {
    const voteTopicBody = {
      id: this.idVotation,
      votation: this.voteSurvey,
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
    };
    this.modalVotationService
      .voteTopics(voteTopicBody)
      .pipe(takeUntil(this.ngUnsubscribe))
      .subscribe({
        next: (response) => {
          this.closeModal(true);
          alert(this.translate.instant('OK_MESSAGES.OK_SEND_SELECTION'));
        },
        error: (error) => {
          alert(
            this.translate.instant('ERROR_MESSAGES.ERROR_SEND_SELECTION') +
              '\n' +
              error.error.message
          );
        },
      });
  }

  /**
   * Handles the selection of an emoji for rating surveys.
   */
  selectEmoji(emoji: Emoji): void {
    this.voteSurvey = [];
    this.selectedEmoji = emoji;
    this.voteSurvey.push(emoji.id.toString());
  }

  /**
   * Handles the selection of an option for opinion surveys.
   */
  selectOpinion(option: any): void {
    this.voteSurvey = [];
    this.selectedOption = option;
    this.voteSurvey.push(option);
  }

  /**
   * Handles the selection of an item for image-text surveys.
   */
  selectItem(item: any): void {
    this.voteSurvey = [];
    this.selectedItem = item;
    this.voteSurvey.push(this.selectedItem.text);
  }

  /**
   * Handles the selection of an image-text combination for image-text surveys.
   */
  selectImageText(item: any): void {
    this.voteSurvey = [];
    this.selectedImageText = item;
    this.voteSurvey.push(this.selectedImageText.text);
  }
}
