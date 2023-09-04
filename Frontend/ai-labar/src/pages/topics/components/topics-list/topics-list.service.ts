import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
@Injectable({
  providedIn: 'root',
})
export class TopicsListService {
  constructor(private http: HttpClient) {}
  public post(body: any, url: string): Observable<any> {
    return this.http.post<any>(url, body);
  }
  public postResponse(body: any, url: string): Observable<any> {
    return this.http.post<any>(url, body, { observe: 'response' });
  }
  public delete(body: any, url: string): Observable<any> {
    return this.http.delete<any>(url, { body });
  }
  public put(body: any, url: string): Observable<any> {
    return this.http.put<any>(url, body);
  }
  public get(body: any, url: string): Observable<any> {
    return this.http.get<any>(url, body);
  }
}
