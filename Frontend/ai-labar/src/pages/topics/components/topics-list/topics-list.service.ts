import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { HttpClient } from '@angular/common/http';
import { environment } from 'src/environments/environment';
@Injectable({
  providedIn: 'root',
})
export class TopicsListService {
  constructor(private http: HttpClient) {}
  public loadTopics(body: any): Observable<any> {
    const url = `${environment.apiUrl}/topics/loadTopics`;
    return this.http.post<any>(url, body);
  }
  public postResponse(body: any, url: string): Observable<any> {
    return this.http.post<any>(url, body, { observe: 'response' });
  }
  public deleteTopic(body: any): Observable<any> {
    const url = `${environment.apiUrl}/topics/deleteTopic`;
    return this.http.delete<any>(url, { body });
  }
  public reOpenTopic(body: any): Observable<any> {
    const url = `${environment.apiUrl}/topics/reOpenTopic`;
    return this.http.put<any>(url, body);
  }
  public closeTopic(body: any): Observable<any> {
    const url = `${environment.apiUrl}/topics/closeTopic`;
    return this.http.put<any>(url, body);
  }
  public get(body: any, url: string): Observable<any> {
    return this.http.get<any>(url, body);
  }
  public votingResults(body: any): Observable<any> {
    const url = `${environment.apiUrl}/topics/votingResults`;
    return this.http.post<any>(url, body);
  }
  public createGroup(body: any): Observable<any> {
    const url = `${environment.apiUrl}/groups/createGroup`;
    return this.http.post<any>(url, body);
  }
}
