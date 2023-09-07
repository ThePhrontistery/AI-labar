import { Injectable } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';

@Injectable({
  providedIn: 'root',
})
export class LanguageService {
  constructor(private translate: TranslateService) {}

  private currentLanguage: string = 'EN';

  private defaultLanguage: string = 'EN';

  getLanguage(): string {
    return this.currentLanguage;
  }

  setLanguage(language: string) {
    this.currentLanguage = language;
  }

  toggleLanguage() {
    if (this.getLanguage() === 'EN') {
      this.setLanguage('ES');
      this.translate.use('es');
    } else {
      this.setLanguage('EN');
      this.translate.use('en');
    }
  }

  getDefaultLanguage(){
    return this.defaultLanguage;
  }
}
