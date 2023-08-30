/**
 * This component represents the login page of the application.
 * It allows users to log in and register, handling both the
 * authentication process and the registration of new users.
 */
import { Component, OnInit, ViewChild } from '@angular/core';
import { Subscription } from 'rxjs';
import { LoginService } from '../../login.service';
import { Router } from '@angular/router';
import * as CryptoJS from 'crypto-js';
import { CookieService } from 'ngx-cookie-service';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { TranslateService } from '@ngx-translate/core';

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
  loginForm: FormGroup = this.fb.group({
    user: ['', Validators.required],
    password: ['', Validators.required],
  });

  // Storage of subscriptions for later cleaning
  private mySubscription: Subscription[] = [];

  // Variables to handle image loading
  selectedFile!: any;
  base64String!: string;

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
    private translate: TranslateService
  ) {}

  /**
   * Method that is executed when the component is initialized.
   */
  ngOnInit(): void {
    this.translate.setDefaultLang('en');
  }

  /**
   * Method that is executed when the component is destroyed.
   * Clean all active subscriptions.
   */
  ngOnDestroy() {
    this.mySubscription.forEach((item) => item.unsubscribe());
  }

  /**
   * Handles the click event on the login button.
   * Perform user authentication and redirect if successful.
   */
  loginClick(): void {
    // Create the login request body
    const body = {
      user: this.loginForm.value.user,
      password: CryptoJS.SHA256(this.loginForm.value.password).toString(),
    };

    // Store username
    this.username = this.loginForm.value.user;

    // Making the login request and handling the response
    this.mySubscription.push(
      this.loginService.login(body).subscribe((response) => {
        if (
          response &&
          response.body.entity &&
          response.body.entity.length > 1
        ) {
          // Set cookies and redirect user
          this.cookie.set('user', this.username);
          this.cookie.set('token', response.body.entity[0]);
          this.cookie.set('visualization', response.body.entity[1]);
          this.router.navigate(['/topics/topics-list']);
        }
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

      // Remove unnecessary properties before sending
      delete data.imageFunctional;
      delete data.imageVisible;

      // Make the user creation request and handle the response
      this.mySubscription.push(
        this.loginService.createUser(data).subscribe(
          (response) => {
            this.showRegistroFom = false;
            this.limpiarForm();
          },
          (error) => {
            alert('Error al crear al usuario: ' + error.error.message);
          }
        )
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
      this.convertToBase64();
    } else {
      this.selectedFile = null;
      this.base64String = '';

      console.log('Por favor, selecciona un archivo de imagen jpg, png o gif.');
    }
  }

  /**
   * Clean the registration form.
   */
  limpiarForm() {
    this.form.reset();
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
}
