import { ComponentFixture, TestBed } from '@angular/core/testing';
import {
  MatDialogModule,
  MatDialogRef,
  MAT_DIALOG_DATA,
} from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { of } from 'rxjs';

import { TopicResultComponent } from './topic-result.component';
import { TopicsListService } from '../topics-list/topics-list.service';
import { ResultadosVotacionService } from '../voting-results/voting-results.service';
import { IResult } from '../interfaces/emoji.model';

describe('TopicResultComponent', () => {
  let component: TopicResultComponent;
  let fixture: ComponentFixture<TopicResultComponent>;
  let mockTopicsListService: jasmine.SpyObj<TopicsListService>;
  let mockResultsService: jasmine.SpyObj<ResultadosVotacionService>;

  const mockDialogData = {
    votacion: {
      id: 1,
      title: 'Test Voting',
    },
  };

  const mockResultData: IResult[] = [
    {
      votes: 5,
      option: 'Option 1',
      image: 'image.jpg',
    },
    {
      votes: 10,
      option: 'Option 2',
      image: 'image.jpg',
    },
  ];

  beforeEach(() => {
    mockTopicsListService = jasmine.createSpyObj(['post']);
    mockResultsService = jasmine.createSpyObj(['getVotingResults']);

    TestBed.configureTestingModule({
      declarations: [TopicResultComponent],
      imports: [MatDialogModule, HttpClientTestingModule],
      providers: [
        CookieService,
        { provide: MAT_DIALOG_DATA, useValue: mockDialogData },
        { provide: MatDialogRef, useValue: {} },
        { provide: TopicsListService, useValue: mockTopicsListService },
        { provide: ResultadosVotacionService, useValue: mockResultsService },
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(TopicResultComponent);
    component = fixture.componentInstance;

    mockTopicsListService.post.and.returnValue(of({ entity: mockResultData }));
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load results', () => {
    component.ngOnInit();
    expect(mockTopicsListService.post).toHaveBeenCalledWith(
      jasmine.objectContaining({
        id: mockDialogData.votacion.id,
        user: jasmine.any(String),
        token: jasmine.any(String),
      }),
      jasmine.any(String)
    );
    expect(component.result).toEqual(mockResultData);
  });
});
