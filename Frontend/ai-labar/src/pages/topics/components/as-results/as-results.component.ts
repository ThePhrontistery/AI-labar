/**
 * Angular component to display the results of a vote.
 */
import { Component, OnInit, Inject } from '@angular/core';
import { MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { IResultImage } from '../interfaces/emoji.model';
import { ResultadosVotacionService } from '../resultados-votacion/resultados-votacion.service';
import { TopicsListService } from '../topics-list/topics-list.service';
import { environment } from 'src/environments/environment';

@Component({
  selector: 'app-as-results',
  templateUrl: './as-results.component.html',
  styleUrls: ['./as-results.component.scss'],
})
export class AsResultsComponent implements OnInit {
  // Fix to store voting results.
  result: IResultImage[] = [];

  // Fix to store results after certain check.
  verification: IResultImage[] = [];

  /**
   * Component builder.
   * @param data Data passed to the component from the dialog.
   * @param cookie Service to manage cookies.
   * @param topicListService Service to manage the list of topics.
   * @param resultsService Service to manage voting results.
   */
  constructor(
    @Inject(MAT_DIALOG_DATA) public data: any,
    private cookie: CookieService,
    private topicListService: TopicsListService,
    private resultsService: ResultadosVotacionService
  ) {}

  /**
   * Method that is executed when the component is initialized.
   */
  ngOnInit(): void {
    this.loadResults();
  }

  /**
   * Load the voting results.
   */
  loadResults() {
    const url = `${environment.apiUrl}/topics/votingResults`;

    // Data necessary to request results.
    const resultData = {
      id: this.data.votacion.id,
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
    };

    // Make a POST request to get the results.
    this.topicListService.post(resultData, url).subscribe((response) => {
      if (response && response.entity) {
        // You get the winning option using the results service.
        response.entity = this.resultsService.getWinnerOption(response.entity);

        // Assign the 'option' property of each element.
        response.entity.forEach((item: IResultImage) => {
          item.option = item.option;
        });

        // Assigns a default image to non-image options.
        for (let i = 0; i < response.entity.length; i++) {
          if (response.entity[i].image === undefined) {
            response.entity[i].image = 'assets/images/questionMark.png';
          }
        }

        // Assigns the results to the results array.
        this.result = response.entity;
      }
    });
  }
}
