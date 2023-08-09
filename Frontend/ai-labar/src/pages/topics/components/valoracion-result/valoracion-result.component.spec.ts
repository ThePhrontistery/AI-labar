import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatDialogModule, MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { of } from 'rxjs';
import { ValoracionResultComponent } from './valoracion-result.component';
import { TopicsListService } from '../topics-list/topics-list.service';

describe('ValoracionResultComponent', () => {
  let component: ValoracionResultComponent;
  let fixture: ComponentFixture<ValoracionResultComponent>;
  let mockDialogRef: jasmine.SpyObj<MatDialogRef<ValoracionResultComponent>>;
  let mockCookieService: jasmine.SpyObj<CookieService>;
  let mockTopicListService: jasmine.SpyObj<TopicsListService>;

  const testData = {
    votacion: {
      id: 1,
      title: 'Test Votacion'
    }
  };

  beforeEach(() => {
    mockDialogRef = jasmine.createSpyObj(['close']);
    mockCookieService = jasmine.createSpyObj(['get']);
    mockTopicListService = jasmine.createSpyObj(['post']);

    TestBed.configureTestingModule({
      declarations: [ValoracionResultComponent],
      imports: [MatDialogModule, HttpClientTestingModule],
      providers: [
        { provide: MatDialogRef, useValue: mockDialogRef },
        { provide: MAT_DIALOG_DATA, useValue: testData },
        { provide: CookieService, useValue: mockCookieService },
        { provide: TopicsListService, useValue: mockTopicListService }
      ]
    });

    fixture = TestBed.createComponent(ValoracionResultComponent);
    component = fixture.componentInstance;

    mockCookieService.get.and.returnValue('testUser');
    mockTopicListService.post.and.returnValue(of({ entity: [] }));
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load results on initialization', () => {
    const fakeResponse = {
      entity: [
        { emoji: { id: 1, icon: '😄', name: 'Emoji 1', selected: false }, votes: 5 },
        { emoji: { id: 2, icon: '🙂', name: 'Emoji 2', selected: false }, votes: 3 }
      ]
    };

    mockTopicListService.post.and.returnValue(of(fakeResponse));

    component.ngOnInit();

    expect(component.optionsVoted.length).toBe(2);
    expect(component.results.length).toBe(1);
  });

  // Add more test cases as needed
});
