import { Component, EventEmitter, Input, OnInit, Output } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

/**
 * Component for selecting a survey image in the first stage.
 */
@Component({
  selector: 'app-paso-uno',
  templateUrl: './paso-uno.component.html',
  styleUrls: ['./paso-uno.component.scss'],
})
export class PasoUnoComponent implements OnInit {
  /**
   * Data shared from the parent component.
   */
  @Input() sharedData: any;

  /**
   * List of available survey images.
   */
  images = [
    {
      name: this.translate.instant('STEP_ONE.VALUES_SURVEY_NAME'),
      route: 'assets/images/imageOne.png',
      code: 'VAL',
    },
    {
      name: this.translate.instant('STEP_ONE.VALUES_IMAGE_SURVEY_NAME'),
      route: 'assets/images/imageFour.png',
      code: 'VCI',
    },
    {
      name: this.translate.instant('STEP_ONE.EMOJIS_SURVEY_NAME'),
      route: 'assets/images/imageTwo.jpg',
      code: 'EMO',
    },
    {
      name: this.translate.instant('STEP_ONE.EMPLOYEE_SURVEY_NAME'),
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
  ) {}

  /**
   * Method invoked upon initializing the component.
   */
  ngOnInit(): void {}

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
