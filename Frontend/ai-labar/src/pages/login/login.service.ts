import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root',
})
export class LoginService {
  constructor(private http: HttpClient) {}

  public login(body: any): Observable<any> {
    const url = `${environment.apiUrl}/users/login`;
    return this.http.post<any>(url, body, { observe: 'response' });
  }
  public createUser(body: any): Observable<any> {
    const url = `${environment.apiUrl}/users/createUser`;
    return this.http.post<any>(url, body, { observe: 'response' });
  }
}
