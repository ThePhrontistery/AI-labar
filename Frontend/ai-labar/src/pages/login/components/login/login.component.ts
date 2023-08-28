import { Component, OnInit, ViewChild } from '@angular/core';
import { Subscription } from 'rxjs';
import { LoginService } from '../../login.service';
import { Router } from '@angular/router';
import * as CryptoJS from 'crypto-js';
import { CookieService } from 'ngx-cookie-service';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { HasElementRef } from '@angular/material/core/common-behaviors/color';
import { MatInput } from '@angular/material/input';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss']
})
export class LoginComponent implements OnInit {
  @ViewChild('f_input') f_input!: any; // Importante: agregar ViewChild para el MatSort
  username: string = '';
  password: string = '';
  usernameError: string = '';
  passwordError: string = '';
  fileName: string = '';
  showRegistroFom: boolean = false;
  form: FormGroup =  this.fb.group({
    user: ['', Validators.required],
    password: ['', Validators.required],
    email: ['', [Validators.required, Validators.email]],
    imageVisible: [''],
    imageFunctional: ['']

  });

  loginForm: FormGroup =  this.fb.group({
    user: ['', Validators.required],
    password: ['', Validators.required],
  });

  private mySubscription: Subscription[] = [];
  selectedFile!: any;
  base64String!: string;

  constructor(private loginService: LoginService,
              private router: Router,
              private cookie: CookieService,
              private fb: FormBuilder) {}

  ngOnInit(): void {}
  ngOnDestroy() {
    this.mySubscription.forEach(item => item.unsubscribe());
  }

  loginClick(): void {
     const body = { user: this.loginForm.value.user, password: CryptoJS.SHA256(this.loginForm.value.password).toString() };
     this.username = this.loginForm.value.user;
     this.mySubscription.push(this.loginService.login(body).subscribe(
      response => {
        if (response && response.body.entity && response.body.entity.length>1)
          { 
            this.cookie.set('user', this.username);
            this.cookie.set('token', response.body.entity[0]);
            this.cookie.set('visualization', response.body.entity[1]);
            this.router.navigate(['/topics/topics-list']);
            }}))
  }


  registro(): void {
    this.showRegistroFom = true;
 }
  sendRegistro(){
    if (this.form.valid) {
      this.form.value.password = CryptoJS.SHA256(this.form.value.password).toString()
      const data = this.form.value;
      data['gender'] = 'M'
      if(this.base64String) data['photo'] = this.base64String !== '' ? this.base64String : '';
      delete data.imageFunctional;
      delete data.imageVisible;
      this.mySubscription.push(this.loginService.createUser(data).subscribe(
        response => {
          this.showRegistroFom = false;
        },
        error => {
          alert('Error al crear al usuario: ' + error.error.message);
        }
      ));
    }
  }
  upload(ev: Event){
    this.f_input.nativeElement.click(ev);
//    this.handleFileInputChange(ev);
  }
  private isImageFile(file: File): boolean {
    const fileType = file.type;
    this.fileName = file.name ? file.name : '';
    return fileType === 'image/jpeg' || fileType === 'image/gif' || fileType === 'image/png';
  }
 
  onFileSelected(ev: any){
    const file: File =  ev.target.files[0];
   if (file && this.isImageFile(file)) {
     this.selectedFile = file;
     this.convertToBase64();
   } else {
    this.selectedFile = null;
    this.base64String = ''; // También limpiamos el valor Base64 si se deselecciona la imagen.

     // Si el archivo no es una imagen jpg o gif, puedes mostrar un mensaje o realizar alguna acción adicional.
     console.log('Por favor, selecciona un archivo de imagen jpg, png o gif.');
   }
  }
 
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




