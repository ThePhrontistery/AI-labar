import { HttpClientModule } from '@angular/common/http';
import { TestBed } from '@angular/core/testing';

import { TopicsCreateService } from './topics-create.service';

describe('TopicsCreateService', () => {
  let service: TopicsCreateService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [ HttpClientModule ]
    });
    service = TestBed.inject(TopicsCreateService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
