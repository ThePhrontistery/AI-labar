import { HttpClientModule } from '@angular/common/http';
import { TestBed } from '@angular/core/testing';

import { ModalVotacionService } from './modal-votation.service';

describe('ModalVotacionService', () => {
  let service: ModalVotacionService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientModule],
    });
    service = TestBed.inject(ModalVotacionService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
