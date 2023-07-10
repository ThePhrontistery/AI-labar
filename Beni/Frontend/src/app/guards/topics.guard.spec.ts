import { TestBed } from '@angular/core/testing';
import { CanActivateFn } from '@angular/router';

import { topicsGuard } from './topics.guard';

describe('topicsGuard', () => {
  const executeGuard: CanActivateFn = (...guardParameters) => 
      TestBed.runInInjectionContext(() => topicsGuard(...guardParameters));

  beforeEach(() => {
    TestBed.configureTestingModule({});
  });

  it('should be created', () => {
    expect(executeGuard).toBeTruthy();
  });
});
