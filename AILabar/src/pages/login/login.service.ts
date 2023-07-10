import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';

@Injectable({
  providedIn: 'root'
})
export class LoginService {
  

  constructor(private http: HttpClient) { }

  public login(body: any): Observable<any> {
    const url = 'http://localhost:8080/topics/login';
    return this.http.post<any>(url, body);
  }
  
}
