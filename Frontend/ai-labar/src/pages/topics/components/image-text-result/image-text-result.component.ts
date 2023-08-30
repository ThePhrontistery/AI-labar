/**
 * This component displays the result of a voting topic in an image and text format.
 * It fetches and displays the voting results for a specific topic.
 */
import { Component, Inject, OnInit } from '@angular/core';
import { IResult } from '../interfaces/emoji.model';
import { MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { TopicsListService } from '../topics-list/topics-list.service';
import { environment } from 'src/environments/environment';

@Component({
  selector: 'app-image-text-result',
  templateUrl: './image-text-result.component.html',
  styleUrls: ['./image-text-result.component.scss']
})
export class ImageTextResultComponent implements OnInit {

  // Array to store voting results
  result: IResult[] = [];

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: any,
    private cookie: CookieService,
    private topicListService: TopicsListService
  ) { }

  /**
   * Initializes the component by loading the voting results.
   */
  ngOnInit(): void {
    this.loadResults();
  }

  /**
   * Fetches and loads the voting results for the displayed topic.
   */
  loadResults(){
    const url = `${environment.apiUrl}/topics/votingResults`;
    const resultData = {
      "id": this.data.votacion.id,
      "user": this.cookie.get("user"),
      "token": this.cookie.get("token")
    };

    this.topicListService.post(resultData, url).subscribe(
      response => {
        if (response){
          this.result = response.entity;
        }
      }
    );
  }

}
