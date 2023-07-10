import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { CookieService } from 'ngx-cookie-service';

@Injectable({
  providedIn: 'root',
})
export class AuthService {
  constructor(private cookie: CookieService, private router: Router) {}

  login(): boolean {
    const userCookie = this.cookie.check('user');
    const tokenCookie = this.cookie.check('token');
    if(userCookie && tokenCookie) {
      this.toTopics();
    }
    return true;
  }

  logout(): boolean {
    const userCookie = this.cookie.check('user');
    const tokenCookie = this.cookie.check('token');
    if(!userCookie || !tokenCookie) {
      this.toLogin();
    }
    return true;
  }

  toLogin() {
    this.router.navigate(['/', 'login']);
  }

  toTopics() {
    this.router.navigate(['/', 'topics']);
  }

}
