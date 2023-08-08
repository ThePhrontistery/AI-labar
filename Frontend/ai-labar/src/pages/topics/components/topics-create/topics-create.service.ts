import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class TopicsCreateService {

  constructor(private http: HttpClient) { }

  public createTopics(body: any): Observable<any> {
    const url = 'http://localhost:8080/topics/createTopic';
    return this.http.post<any>(url, body);
  }

  public getGroupsByUser(body: any): Observable<any> {
    const url = 'http://localhost:8080/groups/getGroupsByUser';
    return this.http.post<any>(url, body);
  }

  public getGroup(body: any): Observable<any> {
    const url = 'http://localhost:8080/groups/getGroup';
    return this.http.post<any>(url, body);
  }
}
