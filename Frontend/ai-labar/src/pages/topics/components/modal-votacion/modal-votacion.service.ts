import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';

@Injectable({
  providedIn: 'root'
})
export class ModalVotacionService {

  constructor(private http: HttpClient) { }

  public voteTopics(body: any): Observable<any> {
    const url = 'http://localhost:8080/topics/vote';
    return this.http.put<any>(url, body);
  }
}
