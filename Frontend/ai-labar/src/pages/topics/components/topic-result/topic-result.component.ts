import { Component, OnInit, Inject } from '@angular/core';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { IResult } from '../interfaces/emoji.model';
import { ResultadosVotacionService } from '../resultados-votacion/resultados-votacion.service';
import { TopicsListService } from '../topics-list/topics-list.service';


@Component({
  selector: 'app-topic-result',
  templateUrl: './topic-result.component.html',
  styleUrls: ['./topic-result.component.scss']
})
export class TopicResultComponent implements OnInit {
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
    const url = 'http://localhost:8080/topics/votingResults';
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
