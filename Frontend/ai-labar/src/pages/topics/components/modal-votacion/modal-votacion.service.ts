import { HttpClient } from '@angular/common/http';
import { Injectable } from '@angular/core';
import { Observable } from 'rxjs';
import { environment } from 'src/environments/environment';

@Injectable({
  providedIn: 'root'
})
export class ModalVotacionService {

  constructor(private http: HttpClient) { }

  public voteTopics(body: any): Observable<any> {
    const url = `${environment.apiUrl}/topics/vote`;
    return this.http.put<any>(url, body);
  }
}
