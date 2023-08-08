import { Component, OnInit, Inject } from '@angular/core';
import { MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { IResult, IResultImage } from '../interfaces/emoji.model';
import { ResultadosVotacionService } from '../resultados-votacion/resultados-votacion.service';
import { TopicsListService } from '../topics-list/topics-list.service';

@Component({
  selector: 'app-as-results',
  templateUrl: './as-results.component.html',
  styleUrls: ['./as-results.component.scss']
})
export class AsResultsComponent implements OnInit {

  result: IResultImage[] = [];
  comprobacion: IResultImage[] = [];
  constructor(@Inject(MAT_DIALOG_DATA) public data: any,
  private cookie: CookieService,
  private topicListService: TopicsListService,
  private resultsService: ResultadosVotacionService,
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
      if (response && response.entity){
        response.entity = this.resultsService.getWinnerOption(response.entity)
        response.entity.map((item: IResultImage) => {
        item.option = item.option})
        for (let i = 0; i < response.entity.length; i++) {
          if(response.entity[i].image == undefined){
            response.entity[i].image = "assets/images/interrogante.png";
          }
      }
        this.result = response.entity;
      }
  });
  }
}
