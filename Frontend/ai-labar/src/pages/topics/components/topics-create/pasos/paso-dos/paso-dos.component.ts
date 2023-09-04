import { Component, Input, OnDestroy, OnInit } from '@angular/core';
import { Emoji } from '../../../interfaces/emoji.model';
import { AnyadirGruposTopicComponent } from '../../../anyadir-grupos-topic/anyadir-grupos-topic.component';
import { MatDialog } from '@angular/material/dialog';
import { AnyadirCandidatosTopicComponent } from '../../../anyadir-candidatos-topic/anyadir-candidatos-topic.component';
import {
  DateAdapter,
  MAT_DATE_FORMATS,
  MAT_DATE_LOCALE,
} from '@angular/material/core';
import { MomentDateAdapter } from '@angular/material-moment-adapter';

/**
 * Second-stage form for creating surveys of different types.
 */
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
  providers: [
    {
      provide: DateAdapter,
      useClass: MomentDateAdapter,
      deps: [MAT_DATE_LOCALE],
    },
    { provide: MAT_DATE_FORMATS, useValue: MY_FORMATS },
  ],
})
export class PasoDosComponent implements OnInit, OnDestroy {
  /**
   * Data shared from the parent component.
   */
  @Input() sharedData: any;

  /**
   * Selected image for the survey.
   */
  @Input() selectedImage: any;

  // Survey type codes
  opinionTopicCode = 'VAL';
  ratingTopicCode = 'EMO';
  votingTopicCode = 'EMP';
  imageTextTopicCode = 'VCI';

  // Flag to display or hide certain survey fields
  isOpinionSurvey = false;
  isVotingSurvey = false;
  isRatingSurvey = false;
  isSurveyImageText = false;

  // Variables to store text field values
  textBoxValue: string = '';
  surveyValue1: string = '';
  surveyValue2: string = '';
  surveyValue3: string = '';
  surveyValue4: string = '';
  imageTextValue1: string = '';
  imageTextValue2: string = '';
  imageTextValue3: string = '';
  imageTextValue4: string = '';

  // List of available emojis for rating surveys
  emojis: Emoji[] = [
    { id: 1, icon: '😄', name: 'Emoji 1', selected: false },
    { id: 2, icon: '🙂', name: 'Emoji 2', selected: false },
    { id: 3, icon: '😐', name: 'Emoji 3', selected: false },
    { id: 4, icon: '😔', name: 'Emoji 4', selected: false },
    { id: 5, icon: '😭', name: 'Emoji 5', selected: false },
  ];

  selectedEmoji: Emoji | null = null;

  // Lists for storing selected objects
  objects: any[] = [];
  objectsToBack: string[] = [];

  // Variables to store selections and values
  textToAdd: string = '';
  imageUrl: string = '';
  users: string[] = [];
  selectedGroup: string[] = [];
  closingDate: string = '';
  selectedFiles: { [key: number]: File | null } = {};
  selectedFilesBase64: string[] = [];
  selectedType: string = 'simple';
  usersCandidates: string[] = [];
  selectedGroupCandidates: string[] = [];
  minDate: Date;
  dateFilter = (date: Date | null): boolean => {
    if (date === null) {
      return false;
    }
    const currentDate = new Date();
    return date >= currentDate;
  };

  constructor(private dialog: MatDialog) {
    this.minDate = new Date();
  }

  /**
   * Method invoked upon initializing the component
   * Determine which survey options to display based on the selected image.
   */
  ngOnInit(): void {
    this.optionsToDisplay();
  }

  /**
   * Configure the options for displaying survey fields based on the selected image.
   */
  optionsToDisplay() {
    if (this.selectedImage.code === this.opinionTopicCode) {
      this.isOpinionSurvey = true;
    } else if (this.selectedImage.code === this.ratingTopicCode) {
      this.isRatingSurvey = true;
    } else if (this.selectedImage.code === this.votingTopicCode) {
      this.isVotingSurvey = true;
    } else if (this.selectedImage.code === this.imageTextTopicCode) {
      this.isSurveyImageText = true;
    }
  }

  /**
   * Clear the options for displaying survey fields.
   */
  removeOptionValues() {
    this.isOpinionSurvey = false;
    this.isRatingSurvey = false;
    this.isVotingSurvey = false;
    this.isSurveyImageText = false;
  }

  /**
   * Open a dialog to add participants to the survey.
   */
  openAddParticipants() {
    const dialogRef = this.dialog.open(AnyadirGruposTopicComponent, {
      width: '400px',
    });

    dialogRef.afterClosed().subscribe((result) => {
      this.selectedGroup = result.selectedGroup;
      this.users = result.selectedUsers;
    });
  }

  /**
   * Select a closing date for the survey.
   * @param event Date selection event.
   */
  onDateSelected(event: any) {
    this.closingDate = this.formatDate(event.value);
    const isValidDate = this.isDateGreaterThanCurrent(this.closingDate);
    if (!isValidDate) {
      this.closingDate = '';
      event.target.value = '';
    }
  }

  /**
   * Format a date in 'DD/MM/YYYY' format.
   * @param dateString Date in string format.
   * @returns Formatted date.
   */
  formatDate(dateString: string): string {
    const date = new Date(dateString);
    const day = date.getDate().toString().padStart(2, '0');
    const month = (date.getMonth() + 1).toString().padStart(2, '0');
    const year = date.getFullYear();
    const formattedDate = `${day}/${month}/${year}`;
    return formattedDate;
  }

  /**
   * Compare if a date is greater than the current date.
   * @param dateString Date in 'DD/MM/YYYY' string format.
   * @returns 'true' if the date is greater than the current date, otherwise 'false'.
   */
  isDateGreaterThanCurrent(dateString: string): boolean {
    const currentDate = new Date();
    const dateParts = dateString.split('/');
    const day = parseInt(dateParts[0]);
    const month = parseInt(dateParts[1]) - 1;
    const year = parseInt(dateParts[2]);
    const enteredDate = new Date(year, month, day);
    return enteredDate > currentDate;
  }

  /**
   * Handles file selection.
   * @param event File selection event.
   * @param imageNumber Number of the selected image.
   */
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

  /**
   * Sends data in base64 format to the web service.
   * @param base64data Data in base64 format.
   * @param imageNumber Number of the selected image.
   */
  sendBase64ToWebService(base64data: string, imageNumber: number) {
    this.selectedFilesBase64[imageNumber] =
      'data:image/png;base64,' + base64data;
  }

  /**
   * Open a dialog to add candidates to the survey.
   */
  openAddCandidates() {
    const dialogRef = this.dialog.open(AnyadirCandidatosTopicComponent, {
      width: '500px',
      data: {},
    });

    dialogRef.afterClosed().subscribe((result) => {
      this.selectedGroupCandidates = result.selectedGroup;
      this.usersCandidates = result.selectedUsers;
      this.objectsToBack = this.usersCandidates;
    });
  }

  /**
   * Method invoked when destroying the component.
   * Clears the display options.
   */
  ngOnDestroy(): void {
    this.removeOptionValues();
  }
}
