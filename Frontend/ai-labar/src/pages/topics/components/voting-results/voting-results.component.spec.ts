import { ComponentFixture, TestBed } from '@angular/core/testing';

import { VotingResultsComponent } from './voting-results.component';

describe('RestultadosVotacionComponent', () => {
  let component: VotingResultsComponent;
  let fixture: ComponentFixture<VotingResultsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [VotingResultsComponent],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(VotingResultsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
