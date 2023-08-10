import { Injectable } from '@angular/core';

@Injectable({
  providedIn: 'root'
})
export class ResultadosVotacionService {

  constructor() { }
  getWinnerOption(voteOptions: any[]): any[] {
    // Obtener el número máximo de votos
    const maxVotes = Math.max(...voteOptions.map(option => option.votes));
  
    // Filtrar las opciones que tengan el número máximo de votos
    const winnerOptions = voteOptions.filter(option => option.votes === maxVotes);
  
    return winnerOptions;
  }
}
