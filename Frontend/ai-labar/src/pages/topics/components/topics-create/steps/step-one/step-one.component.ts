import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

/**
 * Component for selecting a survey image in the first stage.
 */
@Component({
  selector: 'app-step-one',
  templateUrl: './step-one.component.html',
  styleUrls: ['./step-one.component.scss'],
})
export class StepOneComponent implements OnInit {
  /**
   * Data shared from the parent component.
   */
  @Input() sharedData: any;

  values_name: string = '';
  values_img_name: string = '';
  emojis_name: string = '';
  employer_name: string = '';
  /**
   * List of available survey images.
   */
  images = [
    {
      name: this.values_name,
      route: 'assets/images/imageOne.png',
      code: 'VAL',
    },
    {
      name: this.values_img_name,
      route: 'assets/images/imageFour.png',
      code: 'VCI',
    },
    {
      name: this.emojis_name,
      route: 'assets/images/imageTwo.jpg',
      code: 'EMO',
    },
    {
      name: this.employer_name,
      route: 'assets/images/imageThree.png',
      code: 'EMP',
    },
  ];

  /**
   * Selected image for the survey.
   */
  selectedImage = { route: null, name: null, code: null };

  /**
   * Event emitted when an image is selected.
   */
  @Output() parentMethodInvoked = new EventEmitter<any>();

  constructor(
    private translate: TranslateService
  ) { }

  /**
   * Method invoked upon initializing the component.
   */
  ngOnInit(): void {

    this.translate
      .get('STEP_ONE.VALUES_SURVEY_NAME')
      .subscribe((translation: string) => {
        this.values_name = translation;
        this.images[0].name = translation;
      });

    this.translate
      .get('STEP_ONE.VALUES_IMAGE_SURVEY_NAME')
      .subscribe((translation: string) => {
        this.values_img_name = translation;
        this.images[1].name = translation;
      });

    this.translate
      .get('STEP_ONE.EMOJIS_SURVEY_NAME')
      .subscribe((translation: string) => {
        this.emojis_name = translation;
        this.images[2].name = translation;
      });

    this.translate
      .get('STEP_ONE.EMPLOYEE_SURVEY_NAME')
      .subscribe((translation: string) => {
        this.employer_name = translation;
        this.images[3].name = translation;
      });
  }

  /**
   * Select an image from the list and emit the event to the parent component.
   * @param imagen Selected image.
   */
  selectImage(imagen: any) {
    this.selectedImage.route = imagen.route;
    this.selectedImage.name = imagen.name;
    this.selectedImage.code = imagen.code;
    this.callParentMethod();
  }

  /**
   * Emit the event to the parent component with the selected image.
   */
  callParentMethod() {
    this.parentMethodInvoked.emit(this.selectedImage);
  }
}
