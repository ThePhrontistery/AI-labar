import { ComponentFixture, TestBed } from '@angular/core/testing';
import { VotingResultsComponent } from './voting-results.component';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';

describe('VotingResultsComponent', () => {
  let component: VotingResultsComponent;
  let fixture: ComponentFixture<VotingResultsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [VotingResultsComponent],
      imports: [HttpClientTestingModule, TranslateModule.forRoot()],
      providers: [TranslateService]
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

  it('should display correct title', () => {
    const titleElement: HTMLElement = fixture.nativeElement.querySelector('h1');
    component.titleSurvey = 'Sample Title';
    fixture.detectChanges();
    expect(titleElement.textContent).toContain('Sample Title');
  });

  it('should display items', () => {
    const items = [
      { votes: 5, option: 'Option A' },
      { votes: 8, option: 'Option B' },
      // Add more sample items as needed
    ];
    component.items = items;
    fixture.detectChanges();

    const liElements: NodeListOf<Element> =
      fixture.nativeElement.querySelectorAll('li');

    expect(liElements.length).toBe(items.length);

    for (let i = 0; i < items.length; i++) {
      const liElement = liElements[i];
      const item = items[i];
      expect(liElement.textContent).toContain(`(${item.votes}) ${item.option}`);
    }
  });

  it('should emit event when close button is clicked', () => {
    spyOn(component.closeModal, 'emit');
    const closeButton: HTMLButtonElement =
      fixture.nativeElement.querySelector('.rightButton');

    closeButton.click();

    expect(component.closeModal.emit).toHaveBeenCalled();
  });
});
