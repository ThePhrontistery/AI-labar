import { Component, OnInit, ViewChild } from '@angular/core';
import { PasoDosComponent } from './pasos/paso-dos/paso-dos.component';
import { CookieService } from 'ngx-cookie-service';
import { TopicsCreateService } from './topics-create.service';
import { Router } from '@angular/router';
import { MatDialog } from '@angular/material/dialog';
import { TranslateService } from '@ngx-translate/core';

/**
 * Component for creating surveys of different types.
 */
@Component({
  selector: 'app-topics-create',
  templateUrl: './topics-create.component.html',
  styleUrls: ['./topics-create.component.scss'],
})
export class TopicsCreateComponent implements OnInit {
  @ViewChild(PasoDosComponent)
  childComponent: PasoDosComponent = new PasoDosComponent(this.dialog);

  currentStep = 1;
  sharedData: any = {};
  selectedImageStepOne: any = {};
  surveyTitle: string = '';
  surveyOptions: any[] = [];
  typeTM = 'TEXT_MULTIPLE';
  typeTS = 'TEXT_SINGLE';
  typeIM = 'IMAGE_MULTIPLE';
  typeIS = 'IMAGE_SINGLE';
  typeRating = 'RATING';
  typeAs = 'AS';
  selectedType: string = '';

  members: string[] = [];
  groupSelectedParticipants: string[] = [];

  constructor(
    private cookie: CookieService,
    private topicsCreateService: TopicsCreateService,
    private router: Router,
    private dialog: MatDialog,
    private translate: TranslateService
  ) {}

  ngOnInit(): void {}

  /**
   * Move to the next step in survey creation.
   */
  goToNextStep() {
    this.currentStep++;
  }

  /**
   * Go back to the previous step in survey creation.
   */
  goToPreviousStep() {
    this.currentStep--;
  }

  /**
   * Method invoked by the child component when an image is selected in step one.
   * @param object Object with details of the selected image.
   */
  parentMethod(object: any) {
    this.selectedImageStepOne = object;
    this.goToNextStep();
  }

  /**
   * Cancel survey creation and redirect to the list of topics.
   */
  cancelSave() {
    this.router.navigate(['/topics/topics-list']);
  }

  /**
   * Save the created survey with the provided values.
   */
  saveSurvey() {
    this.surveyTitle = this.childComponent.textBoxValue;
    if (this.childComponent.isOpinionSurvey) {
      this.surveyOptions = [];
      if (this.childComponent.selectedType == 'simple') {
        this.selectedType = this.typeTS;
      } else {
        this.selectedType = this.typeTM;
      }
      this.surveyOptions.push({ option: this.childComponent.surveyValue1 });
      this.surveyOptions.push({ option: this.childComponent.surveyValue2 });
      if (
        this.childComponent.surveyValue3 != null &&
        this.childComponent.surveyValue3 != ''
      ) {
        this.surveyOptions.push({ option: this.childComponent.surveyValue3 });
      }
      if (
        this.childComponent.surveyValue4 != null &&
        this.childComponent.surveyValue4 != ''
      ) {
        this.surveyOptions.push({ option: this.childComponent.surveyValue4 });
      }
    } else if (this.childComponent.isRatingSurvey) {
      this.selectedType = this.typeRating;
      this.surveyOptions = [];
      for (const emoji of this.childComponent.emojis) {
        const option = { option: emoji.id.toString() };
        this.surveyOptions.push(option);
      }
    } else if (this.childComponent.isVotingSurvey) {
      this.selectedType = this.typeAs;
      this.surveyOptions = [];
      for (const object of this.childComponent.objectsToBack) {
        const option = { option: object };
        this.surveyOptions.push(option);
      }
    } else if (this.childComponent.isSurveyImageText) {
      if (this.childComponent.selectedType == 'simple') {
        this.selectedType = this.typeIS;
      } else {
        this.selectedType = this.typeIM;
      }
      this.surveyOptions = [];
      this.surveyOptions.push({
        image: this.childComponent.selectedFilesBase64[1],
        option: this.childComponent.imageTextValue1,
      });
      this.surveyOptions.push({
        image: this.childComponent.selectedFilesBase64[2],
        option: this.childComponent.imageTextValue2,
      });
      if (
        this.childComponent.imageTextValue3 != null &&
        this.childComponent.imageTextValue3 != ''
      ) {
        this.surveyOptions.push({
          image: this.childComponent.selectedFilesBase64[3],
          option: this.childComponent.imageTextValue3,
        });
      }
      if (
        this.childComponent.imageTextValue4 != null &&
        this.childComponent.imageTextValue4 != ''
      ) {
        this.surveyOptions.push({
          image: this.childComponent.selectedFilesBase64[4],
          option: this.childComponent.imageTextValue4,
        });
      }
    }

    if (this.childComponent.users.length > 0) {
      this.members = this.childComponent.users;
      this.groupSelectedParticipants = this.childComponent.selectedGroup;
    }
    this.createTopics();
  }

  /**
   * Validate that the required values are present before creating the survey.
   * @returns 'true' if the values are valid, otherwise 'false'.
   */
  valuesValidation(): boolean {
    const isValid = true;
    if (!this.childComponent.closingDate) {
      alert(this.translate.instant('ERROR_MESSAGES.EMPTY_CLOSING_DATE'));
      return false;
    }
    if (!this.surveyTitle) {
      alert(this.translate.instant('ERROR_MESSAGES.EMPTY_TITLE'));
      return false;
    }
    return isValid;
  }

  /**
   * Create the topic using the corresponding service.
   */
  createTopics() {
    if (this.valuesValidation()) {
      const createTopicsBody = {
        title: this.surveyTitle,
        type: this.selectedType,
        question: 'test',
        options: this.surveyOptions,
        user: this.cookie.get('user'),
        groupName: this.groupSelectedParticipants,
        closeDate: this.childComponent.closingDate,
        token: this.cookie.get('token'),
      };
      this.topicsCreateService.createTopics(createTopicsBody).subscribe(
        (response) => {
          if (response) {
            this.router.navigate(['/topics/topics-list']);
          }
        },
        (error) => {
          let textError = error.error.message;
          if (error.error.message === undefined) textError = error.error.error;
          alert(this.translate.instant('ERROR_MESSAGES.TOPIC_CREATE_ERROR') +'\n'+ error.error.message);
        }
      );
    }
  }
}
