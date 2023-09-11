import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import { Observable } from 'rxjs';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root',
})
export class LanguageService {
  constructor(private translate: TranslateService, private http: HttpClient) {}

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

  public saveLanguage(body: any): Observable<any> {
    const url = `${environment.apiUrl}/users/editLanguage`;
    return this.http.put<any>(url, body, { observe: 'response' });
  }
}
