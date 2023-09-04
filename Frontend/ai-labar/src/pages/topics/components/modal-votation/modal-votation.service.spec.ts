import { HttpClientModule } from '@angular/common/http';
import { TestBed } from '@angular/core/testing';

import { ModalVotationService } from './modal-votation.service';

describe('ModalVotationService', () => {
  let service: ModalVotationService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientModule],
    });
    service = TestBed.inject(ModalVotationService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
