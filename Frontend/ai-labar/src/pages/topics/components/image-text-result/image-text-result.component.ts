import { Component, EventEmitter, Inject, Input, OnInit, Output } from '@angular/core';
import { IResult } from '../interfaces/emoji.model';
import { MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { TopicsListService } from '../topics-list/topics-list.service';
import { ResultadosVotacionService } from '../resultados-votacion/resultados-votacion.service';
import { environment } from 'src/environments/environment';

@Component({
  selector: 'app-image-text-result',
  templateUrl: './image-text-result.component.html',
  styleUrls: ['./image-text-result.component.scss']
})
export class ImageTextResultComponent implements OnInit {

  result: IResult[] = [];
  constructor(@Inject(MAT_DIALOG_DATA) public data: any,
    private cookie: CookieService,
    private topicListService: TopicsListService,
    private resultsService: ResultadosVotacionService
  ) { }

  ngOnInit(): void {
    this.loadResults();
  }
  loadResults(){
    const url = `${environment.apiUrl}/topics/votingResults`;
    const resultData = {
      "id": this.data.votacion.id,
      "user": this.cookie.get("user"),
      "token": this.cookie.get("token")
  }
  this.topicListService.post(resultData, url).subscribe(
    response => {
      if (response){
        this.result = response.entity;
      }
  });
  }

}
