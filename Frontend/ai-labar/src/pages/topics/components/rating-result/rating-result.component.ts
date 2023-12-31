import { Component, Inject, OnInit } from '@angular/core';
import { MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { Emoji, IEmojiResult } from '../interfaces/emoji.model';
import { TopicsListService } from '../topics-list/topics-list.service';
import { environment } from 'src/environments/environment';

/**
 * Component that displays the results of a rating survey with emojis.
 */
@Component({
  selector: 'app-rating-result',
  templateUrl: './rating-result.component.html',
  styleUrls: ['./rating-result.component.scss'],
})
export class RatingResultComponent implements OnInit {
  // Variable and property declarations.
  message = '';
  emojis: Emoji[] = [
    { id: 1, icon: '😄', name: 'Emoji 1', selected: false },
    { id: 2, icon: '🙂', name: 'Emoji 2', selected: false },
    { id: 3, icon: '😐', name: 'Emoji 3', selected: false },
    { id: 4, icon: '😔', name: 'Emoji 4', selected: false },
    { id: 5, icon: '😭', name: 'Emoji 5', selected: false },
  ];
  emojisVotation: Emoji[] = [];
  optionsVoted: IEmojiResult[] = [];
  results: IEmojiResult[] = [];

  constructor(
    @Inject(MAT_DIALOG_DATA) public data: any,
    private cookie: CookieService,
    private topicListService: TopicsListService
  ) {}

  // Load the survey results upon initializing the component.
  ngOnInit(): void {
    this.loadResults();
  }

  // Method to load the survey results.
  loadResults() {
    if (this.data && this.data.votation) {
      const resultData = {
        id: this.data.votation.id,
        user: this.cookie.get('user'),
        token: this.cookie.get('token'),
      };
      this.topicListService.votingResults(resultData).subscribe((response) => {
        if (response) {
          this.emojisVotation = [];
          response.entity.sort((a: any, b: any) => {
            const optionA = parseInt(a.option);
            const optionB = parseInt(b.option);
            return optionA - optionB;
          });
          for (let i = 0; i < response.entity.length; i++) {
            response.entity[i]['emoji'] = this.emojis[i];
            this.optionsVoted = response.entity;
          }
          this.results = this.getWinnerOption(response.entity);
        }
      });
    }
  }

  // Method to determine the winning option based on votes.
  getWinnerOption(voteOptions: any[]): any[] {
    const maxVotes = Math.max(...voteOptions.map((option) => option.votes));

    const winnerOptions = voteOptions.filter(
      (option) => option.votes === maxVotes
    );

    if (winnerOptions.length > 1) {
      return [
        winnerOptions.reduce((prev, current) =>
          prev.option < current.option ? prev : current
        ),
      ];
    }

    return winnerOptions;
  }
}
