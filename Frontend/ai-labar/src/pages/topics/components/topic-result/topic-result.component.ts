import { Component, OnInit, Inject } from '@angular/core';
import { MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { IResult } from '../interfaces/emoji.model';
import { TopicsListService } from '../topics-list/topics-list.service';
import { environment } from 'src/environments/environment';

/**
 * Component to display the voting results on a specific topic.
 */
@Component({
  selector: 'app-topic-result',
  templateUrl: './topic-result.component.html',
  styleUrls: ['./topic-result.component.scss'],
})
export class TopicResultComponent implements OnInit {
  /**
   * List of voting results for the specific topic.
   */
  result: IResult[] = [];

  /**
   * Component builder.
   * @param data Data injected from the dialog that invoked the component.
   * @param cookie Service for working with cookies.
   * @param topicListService Service for obtaining the voting results.
   */
  constructor(
    @Inject(MAT_DIALOG_DATA) public data: any,
    private cookie: CookieService,
    private topicListService: TopicsListService
  ) {}

  /**
   * Method invoked upon initializing the component.
   * Load the voting results for the specific topic.
   */
  ngOnInit(): void {
    this.loadResults();
  }

  /**
   * Load the voting results for the specific topic.
   * Make an HTTP request to the server to fetch the results.
   */
  loadResults() {
    const url = `${environment.apiUrl}/topics/votingResults`;
    const resultData = {
      id: this.data.votacion.id,
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
    };

    this.topicListService.post(resultData, url).subscribe((response) => {
      if (response) {
        this.result = response.entity;
      }
    });
  }
}
