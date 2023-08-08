import { HttpClientModule } from '@angular/common/http';
import { TestBed } from '@angular/core/testing';

import { ResultadosVotacionService } from './resultados-votacion.service';

describe('ResultadosVotacionService', () => {
  let service: ResultadosVotacionService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientModule]
    });
    service = TestBed.inject(ResultadosVotacionService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
