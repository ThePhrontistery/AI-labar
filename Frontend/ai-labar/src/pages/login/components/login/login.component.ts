/**
 * This component represents the login page of the application.
 * It allows users to log in and register, handling both the
 * authentication process and the registration of new users.
 */
import { Component, OnInit, ViewChild } from '@angular/core';
import { Subject, Subscription, takeUntil } from 'rxjs';
import { LoginService } from '../../login.service';
import { Router } from '@angular/router';
import * as CryptoJS from 'crypto-js';
import * as JSEncryptModule from 'jsencrypt';
import { CookieService } from 'ngx-cookie-service';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { LangChangeEvent, TranslateService } from '@ngx-translate/core';
import { MessageService } from 'src/pages/topics/services/message.service';
import { LanguageService } from 'src/pages/language.service';
import { environment } from 'src/environments/environment';
import packageJson from '../../../../../package.json';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss'],
})
export class LoginComponent implements OnInit {
  // Reference to file input element for uploading images
  @ViewChild('f_input') f_input!: any;

  // Login form properties
  username: string = '';
  password: string = '';
  usernameError: string = '';
  passwordError: string = '';

  //
  passwordConfirmation: string = '';
  showPasswordModal: boolean = false;

  // Properties related to user registration
  fileName: string = '';
  showRegistroFom: boolean = false;
  form: FormGroup = this.fb.group({
    user: ['', Validators.required],
    password: ['', Validators.required],
    email: ['', [Validators.required, Validators.email]],
    imageVisible: [''],
    imageFunctional: [''],
  });

   // Login form
   loginAdminForm: FormGroup = this.fb.group({
    passwordAdmin: ['', Validators.required],
  });

  // Login form
  loginForm: FormGroup = this.fb.group({
    user: ['', Validators.required],
    password: ['', Validators.required],
  });

  // Storage of subscriptions for later cleaning
  private mySubscription: Subscription[] = [];

  private ngUnsubscribe = new Subject();

  // Variables to handle image loading
  selectedFile!: any;
  base64String!: string;

  currentLanguage!: string;
  textButtonLanguage!: string;

  publicKey: any;

  loginCap!: boolean;

  appVersion: string = packageJson.version;

  /**
   * Component builder.
   * @param loginService Login service
   * @param router Angular router
   * @param cookie Cookie management service
   * @param fb Reactive form builder
   */
  constructor(
    private loginService: LoginService,
    private router: Router,
    private cookie: CookieService,
    private fb: FormBuilder,
    private translate: TranslateService,
    private messageService: MessageService,
    private languageService: LanguageService
  ) {
    this.translate.onLangChange.subscribe((event: LangChangeEvent) => {
      this.translate.get('LANGUAGE.CHANGE').subscribe((translation: string) => {
        this.textButtonLanguage = translation;
      });
    });
  }

  /**
   * Method that is executed when the component is initialized.
   */
  ngOnInit(): void {
    this.translate.addLangs(['en', 'es']);
    this.translate.setDefaultLang('en');

    this.currentLanguage = this.languageService.getLanguage();
    if (this.languageService.getDefaultLanguage() != this.currentLanguage) {
      if (this.currentLanguage === 'ES') {
        this.languageService.setLanguage('ES');
        this.translate.use('es');
      } else {
        this.languageService.setLanguage('EN');
        this.translate.use('en');
      }
    }

    this.translate.get('LANGUAGE.CHANGE').subscribe((translation: string) => {
      this.textButtonLanguage = translation;
    });

    this.loginCap = environment.loginCap;
  }

  /**
   * Method that is executed when the component is destroyed.
   * Clean all active subscriptions.
   */
  ngOnDestroy() {
    this.mySubscription.forEach((item) => item.unsubscribe());
    this.ngUnsubscribe.complete();
  }

  async getPublicKey(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      this.loginService.getPublicKey().subscribe({
        next: (response) => {
          if (response && response.body && response.body.entity) {
            this.publicKey = response.body.entity;
            resolve();
          } else {
            reject(new Error('Respuesta inválida desde el servidor.'));
          }
        },
        error: (error) => {
          this.messageService.showErrorMessage(error.error.message);
          reject(error);
        },
      });
    });
  }

  encryptText(textToEncrypt: string): string | null {
    if (!this.publicKey) {
      console.error('Clave pública no disponible.');
      return null;
    }

    const encrypt = new JSEncryptModule.JSEncrypt();
    encrypt.setPublicKey(this.publicKey);

    const encryptedText = encrypt.encrypt(textToEncrypt);

    if (!encryptedText) {
      console.error('Error al cifrar el texto.');
      return null;
    }

    return encryptedText;
  }

  /**
   * Handles the click event on the login button.
   * Perform user authentication and redirect if successful.
   */
  async loginClick(): Promise<void> {
    let body;

    if (environment.loginCap) {
      await this.getPublicKey();

      const encryptedPassword = this.encryptText(this.loginForm.value.password);

      body = {
        user: this.loginForm.value.user,
        password: encryptedPassword,
        language: this.currentLanguage,
      };
    } else {
      body = {
        user: this.loginForm.value.user,
        password: CryptoJS.SHA256(this.loginForm.value.password).toString(),
        language: this.currentLanguage,
      };
    }

    // Store username
    this.username = this.loginForm.value.user;
    // Making the login request and handling the response
    this.mySubscription.push(
      this.loginService.login(body).subscribe({
        next: (response) => {
          if (
            response &&
            response.body.entity &&
            response.body.entity.length > 1
          ) {
            // Set cookies and redirect user
            this.cookie.set('user', this.username);
            this.cookie.set('token', response.body.entity[0]);
            this.cookie.set('visualization', response.body.entity[1]);
            if (response.body.entity[2]) {
              this.cookie.set('language', response.body.entity[2]);
            } else {
              this.cookie.set(
                'language',
                this.languageService.getDefaultLanguage()
              );
            }
            if (response.body.entity[3]) {
              this.createThumbnailImage(response.body.entity[3], 32, 32);
            } else {
              this.cookie.set('photo', '');
              this.showContent();
            }
          }
        },
        error: (error) => {
          this.messageService.showErrorMessage(error.error.message);
        },
      })
    );
  }

  /**
   * Shows the registration form.
   */
  registration(): void {
    this.showRegistroFom = true;
  }

  /**
   * Send the registration form data to the server to create a new user.
   * Performs password encryption and handles responses.
   */
  sendRegistration() {
    if (this.form.valid) {
      // Encrypt the password before sending it
      this.form.value.password = CryptoJS.SHA256(
        this.form.value.password
      ).toString();

      // Prepare data for registration
      const data = this.form.value;
      data['gender'] = 'M';

      // Add photo if present
      if (this.base64String) {
        data['photo'] = this.base64String !== '' ? this.base64String : '';
      }

      data['language'] = this.currentLanguage;

      // Remove unnecessary properties before sending
      delete data.imageFunctional;
      delete data.imageVisible;

      // Make the user creation request and handle the response
      this.mySubscription.push(
        this.loginService
          .createUser(data)
          .pipe(takeUntil(this.ngUnsubscribe))
          .subscribe({
            next: (response) => {
              this.showRegistroFom = false;
              this.limpiarForm();
            },
            error: (error) => {
              this.messageService.showErrorMessage(
                this.translate.instant('ERROR_MESSAGES.CREATE_USER_ERROR') +
                '\n' +
                error.error.message
              );
            },
          })
      );
    }
  }

  /**
   * Handles the file upload event.
   * Opens the file selection dialog when a hidden button is clicked.
   * @param ev File upload event
   */
  upload(ev: Event) {
    this.f_input.nativeElement.click(ev);
  }

  /**
   * Check if a file is a valid image.
   * @param file File to verify
   * @returns true if the file is a valid image, false otherwise
   */
  private isImageFile(file: File): boolean {
    const fileType = file.type;
    this.fileName = file.name ? file.name : '';
    return (
      fileType === 'image/jpeg' ||
      fileType === 'image/gif' ||
      fileType === 'image/png'
    );
  }

  /**
   * Handles the file selection event.
   * Converts the selected file to base64 format if it is a valid image.
   * @param ev File selection event
   */
  onFileSelected(ev: any) {
    const file: File = ev.target.files[0];
    if (file && this.isImageFile(file)) {
      this.selectedFile = file;

      if (this.selectedFile) {
        const fileSize = this.selectedFile.size;
        if (fileSize > 5 * 1024 * 1024) {
          this.messageService.showErrorMessage(
            this.translate.instant('ERROR_MESSAGES.ERROR_USER_IMAGE_SIZE')
          );

          this.selectedFile = null;
          this.fileName = '';
        } else {
          this.convertToBase64();
        }
      }
    } else {
      this.selectedFile = null;
      this.base64String = '';
    }
  }

  /**
   * Clean the registration form.
   */
  limpiarForm() {
    this.form.reset();
  }

  toggleLanguage() {
    this.languageService.toggleLanguage();
    this.currentLanguage = this.languageService.getLanguage();
    this.textButtonLanguage = this.translate.instant('LANGUAGE.CHANGE');
  }

  /**
   * Converts the selected file to base64 format.
   */
  private convertToBase64(): void {
    if (this.selectedFile) {
      const reader = new FileReader();
      reader.onload = () => {
        this.base64String = reader.result as string;
      };
      reader.readAsDataURL(this.selectedFile);
    }
  }

  /**
   * Create a thumbnail image from a base64 representation of an image.
   *
   * @param {string} imageBase64 - The base64 representation of the image.
   * @param {number} width - Desired width of the thumbnail image.
   * @param {number} height - Desired height of the thumbnail image.
   */
  private createThumbnailImage(
    imageBase64: string,
    width: number,
    height: number
  ) {
    // Create a canvas to draw the thumbnail image on.
    const canvas = document.createElement('canvas');
    canvas.width = width;
    canvas.height = height;
    const context = canvas.getContext('2d');

    // Create a new instance of Image and set the source as the base64 image.
    const image = new Image();
    image.src = imageBase64;

    // Check if the canvas context has been successfully created.
    if (context === null) return;

    // When the image is successfully loaded, it draws the image on the canvas and obtains the base64 representation of the thumbnail image.
    image.onload = () => {
      context.drawImage(image, 0, 0, width, height);

      // Converts the thumbnail image to base64 and stores it in a cookie named 'photo'.
      this.cookie.set('photo', canvas.toDataURL());

      // Display the content after creating the thumbnail image.
      this.showContent();
    };
  }

  /**
   * Navigate to the topic list view after displaying the content.
   */
  private showContent() {
    this.router.navigate(['/topics/topics-list']);
  }






// Método para mostrar el modal de contraseña cuando se hace clic en "Registrar"
showRegistrationModal() {
  if (this.loginCap) {
    this.showPasswordModal = true;
  } else {
    this.showRegistroFom = true; // Si loginCap es falso, mostrar el formulario de registro directamente
  }
}

// Método para cancelar el modal de contraseña
cancelPasswordModal() {
  this.showPasswordModal = false;
  this.password = ''; // Limpiar la contraseña ingresada
}

// Método para verificar la contraseña ingresada y mostrar el formulario de registro si es válida
checkPassword() {
  // Hash de la contraseña ingresada en SHA256
 // const hashedPassword = CryptoJS.SHA256(this.password).toString();

  let body;
  body = {
          password: CryptoJS.SHA256(this.loginAdminForm.value.passwordAdmin).toString()
        };
  // Llamar al servicio web para verificar la contraseña
  this.loginService.checkPassword(body).subscribe({
    next: (response) => {
      if (response === 'ok') {
        // Contraseña válida, mostrar el formulario de registro
        this.showRegistroFom = true;
        this.showPasswordModal = false; // Cerrar el modal de contraseña
      } else {
        // Contraseña no válida, mostrar un mensaje de error
        // Puedes mostrar el mensaje de error en un elemento HTML o utilizar una biblioteca de notificaciones
        // Por ejemplo: this.notificationService.showError('No tiene permiso para crear usuarios');
        console.error('No tiene permiso para crear usuarios');
      }
      this.showRegistroFom = true;
      this.showPasswordModal = false;
    },
    error: (error) => {
      // Manejar errores de la llamada al servicio web, si es necesario
      console.error('Error al verificar la contraseña', error);
      this.messageService.showErrorMessage(
        this.translate.instant('ERROR_MESSAGES.ERROR_ADMIN_PASS') +
        '\n' +
        error.error.message
      );
    },
  });
}

}
