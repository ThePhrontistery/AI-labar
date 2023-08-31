import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root',
})
export class ResultadosVotacionService {
  constructor() {}
  getWinnerOption(voteOptions: any[]): any[] {
    const maxVotes = Math.max(...voteOptions.map((option) => option.votes));

    const winnerOptions = voteOptions.filter(
      (option) => option.votes === maxVotes
    );

    return winnerOptions;
  }
}
