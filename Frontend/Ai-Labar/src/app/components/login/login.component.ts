import { Component, ElementRef, ViewChild } from '@angular/core';
import { Router } from '@angular/router';
import * as CryptoJS from 'crypto-js';
import { CookieService } from 'ngx-cookie-service';

@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.css', './logo.background.component.css'],
})

export class LoginComponent {
  @ViewChild('usernameInput') usernameInputRef!: ElementRef;
  @ViewChild('passwordInput') passwordInputRef!: ElementRef;
  username: string = '';
  password: string = '';
  usernameError: string = '';
  passwordError: string = '';

  constructor(private router: Router, private cookie: CookieService) {}

  ngAfterViewInit() {
    this.usernameInputRef.nativeElement.addEventListener('input', this.onUsernameInputChange.bind(this));
    this.passwordInputRef.nativeElement.addEventListener('input', this.onPasswordInputChange.bind(this));
  }

  onUsernameInputChange() {
    const usernameInput = document.getElementById('username') as HTMLInputElement;
    if (!this.username || this.username.trim() === '') {
      usernameInput.classList.add('inputError');
      this.usernameError = 'El nombre de usuario es requerido';
    } else {
      usernameInput.classList.add('inputOk');
      usernameInput.classList.remove('inputError');
    }
  }

  onPasswordInputChange() {
    const passwordInput = document.getElementById('password') as HTMLInputElement;
    if (!this.password || this.password.trim() === '') {
      passwordInput.classList.add('inputError');
      this.passwordError = 'La contraseña es requerida';
    } else {
      passwordInput.classList.add('inputOk');
      passwordInput.classList.remove('inputError');
    }
  }

  login(event: Event) {
    event.preventDefault();

    this.usernameError = '';
    this.passwordError = '';

    const data = {
      user: this.username,
      password: CryptoJS.SHA256(this.password).toString(),
    };

    fetch('http://localhost:8080/topics/login', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(data),
    })
      .then((response) => {
        if (response.ok) {
          return response.json();
        } else {
          throw response.json(); // Lanzar el objeto de respuesta JSON en caso de error
        }
      })
      .then((responseJson) => {
        this.cookie.set('user', this.username, 3, '/');
        this.cookie.set('token', responseJson.message, 3, '/');
        this.router.navigate(['/topics']);
      })
      .catch((error: any) => {
        // Mostrar el mensaje de error enviado desde el backend, si está disponible
        error.then((json: any) => {
          if (json && json.message) {
            this.passwordError = json.message;
          } else {
            this.passwordError = 'Error en la solicitud';
          }
        });
      });
  }
}
