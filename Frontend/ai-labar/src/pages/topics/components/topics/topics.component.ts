import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { CookieService } from 'ngx-cookie-service';
import { MatDialog } from '@angular/material/dialog';
import { GroupsComponent } from '../groups/groups.component';
import { LangChangeEvent, TranslateService } from '@ngx-translate/core';
import { LanguageService } from 'src/pages/language.service';
import { Subject, takeUntil } from 'rxjs';
import { MessageService } from '../../services/message.service';
import { LoginService } from 'src/pages/login/login.service';

/**
 * Component for the management of topics and groups.
 */
@Component({
  selector: 'app-topics',
  templateUrl: './topics.component.html',
  styleUrls: ['./topics.component.scss'],
})
export class TopicsComponent implements OnInit {
  /**
   * Current user obtained from cookies.
   */
  user = this.cookie.get('user');

  currentLanguage!: string;
  textButtonLanguage!: string;

  userImage = this.cookie.get('photo');
  userImageIsNull: boolean = true;

  private ngUnsubscribe = new Subject();

  /**
   * Constructor of the component.
   * @param cookie Service for working with cookies.
   * @param router Service for navigation between routes.
   * @param dialog Service for displaying modal dialogs.
   * @param translate Service for text internationalization.
   */
  constructor(
    private cookie: CookieService,
    private router: Router,
    private dialog: MatDialog,
    private translate: TranslateService,
    private languageService: LanguageService,
    private messageService: MessageService,
    private loginService: LoginService
  ) {
    this.translate.onLangChange.subscribe((event: LangChangeEvent) => {
      this.translate
      .get('LANGUAGE.CHANGE')
      .subscribe((translation: string) => {
        this.textButtonLanguage = translation;
      });
    });
  }

  /**
   * Method invoked upon initializing the component.
   * Verify if the user is authenticated and set the default language.
   */
  ngOnInit(): void {
    if (this.cookie.get('user') === '' || this.cookie.get('token') === '') {
      this.router.navigate(['login']);
    }
    this.translate.addLangs(['en', 'es']);
    this.translate.setDefaultLang('en');

    this.currentLanguage = this.languageService.getLanguage();
    if (this.cookie.get('language') != this.currentLanguage) {
      this.languageService.toggleLanguage();
      this.currentLanguage = this.languageService.getLanguage();
    }

    this.translate
      .get('LANGUAGE.CHANGE')
      .subscribe((translation: string) => {
        this.textButtonLanguage = translation;
      });

    if (this.userImage != "null"){
      this.userImageIsNull = false;
    }
  }

  /**
   * Log out the user.
   * Delete the cookies and redirect to the login page.
   */
  logOut() {
    this.cookie.delete('user');
    this.cookie.delete('token');
    this.cookie.delete('language');
    this.router.navigate(['login']);
  }

  /**
   * Navigate back to the list of topics.
   */
  back() {
    this.router.navigate(['/topics/topics-list']);
  }

  toggleLanguage() {
    this.languageService.toggleLanguage();
    this.currentLanguage = this.languageService.getLanguage();
    this.textButtonLanguage = this.translate.instant('LANGUAGE.CHANGE');

    const saveLanguageBody = {
      language: this.currentLanguage,
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
    };
    this.languageService
      .saveLanguage(saveLanguageBody)
      .pipe(takeUntil(this.ngUnsubscribe))
      .subscribe({
        next: (response) => {},
        error: (error) => {
          this.messageService.showErrorMessage(
            this.translate.instant('ERROR_MESSAGES.ERROR_SEND_LANGUAGE') +
              '\n' +
              error.error.message
          );
        },
      });
  }
}
