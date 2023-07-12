import { Injectable } from '@angular/core';
import { Router } from '@angular/router';
import { CookieService } from 'ngx-cookie-service';

@Injectable({
  providedIn: 'root',
})
export class AuthService {
  constructor(private cookie: CookieService, private router: Router) {
    setInterval(this.checkCookies.bind(this), 1000);
  }

  checkUserCookie(): boolean {
    return this.cookie.check('user');
  }

  checTokenCookie(): boolean {
    return this.cookie.check('token');
  }

  checkCookies() {
    const userCookie = this.checkUserCookie();
    const tokenCookie = this.checTokenCookie();

    if (!userCookie || !tokenCookie) {
      this.cookie.delete("user");
      this.cookie.delete("token");
      this.toLogin();
    }
  }

  login(): boolean {
    if(this.checkUserCookie() && this.checTokenCookie()) {
      this.toTopics();
    }
    return true;
  }

  logout(): boolean {
    if(!this.checkUserCookie() || !this.checTokenCookie()) {
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
