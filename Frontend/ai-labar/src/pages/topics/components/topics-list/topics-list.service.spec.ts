import { HttpClientModule } from '@angular/common/http';
import { TestBed } from '@angular/core/testing';

import { TopicsListService } from './topics-list.service';

describe('TopicsListService', () => {
  let service: TopicsListService;

  beforeEach(() => {
    TestBed.configureTestingModule({
      imports: [HttpClientModule],
    });
    service = TestBed.inject(TopicsListService);
  });

  it('should be created', () => {
    expect(service).toBeTruthy();
  });
});
