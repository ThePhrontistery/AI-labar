import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root'
})
export class TopicsCreateService {

  constructor(private http: HttpClient) { }

  public createTopics(body: any): Observable<any> {
    const url = `${environment.apiUrl}/topics/createTopic`;
    return this.http.post<any>(url, body);
  }

  public getGroupsByUser(body: any): Observable<any> {
    const url = `${environment.apiUrl}/groups/getGroupsByUser`;
    return this.http.post<any>(url, body);
  }

  public getGroup(body: any): Observable<any> {
    const url = `${environment.apiUrl}/groups/getGroup`;
    return this.http.post<any>(url, body);
  }
}
