import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatDialog } from '@angular/material/dialog';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { TopicsCreateComponent } from './topics-create.component';
import { TopicsCreateService } from './topics-create.service';
import { CookieService } from 'ngx-cookie-service';
import { of } from 'rxjs';
import { StepTwoComponent } from './steps/step-two/step-two.component';
import { RouterTestingModule } from '@angular/router/testing';
import { TopicsListComponent } from '../topics-list/topics-list.component';
import { Routes } from '@angular/router';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MatSnackBarModule } from '@angular/material/snack-bar';
import { MessageService } from '../../services/message.service';

const routes: Routes = [
  { path: 'topics/topics-list', component: TopicsListComponent },
];

describe('TopicsCreateComponent', () => {
  let component: TopicsCreateComponent;
  let fixture: ComponentFixture<TopicsCreateComponent>;
  let mockTopicsCreateService: jasmine.SpyObj<TopicsCreateService>;
  let mockCookieService: jasmine.SpyObj<CookieService>;
  let mockDialog: jasmine.SpyObj<MatDialog>;
  let topicsCreateServiceMock: jasmine.SpyObj<TopicsCreateService>;
  let translateService: jasmine.SpyObj<TranslateService>;
  let messageService: jasmine.SpyObj<MessageService>;

  beforeEach(() => {
    const mockServiceSpy = jasmine.createSpyObj('TopicsCreateService', [
      'createTopics',
      'saveSurvey',
    ]);
    const mockCookieServiceSpy = jasmine.createSpyObj('CookieService', ['get']);
    const mockDialogSpy = jasmine.createSpyObj('MatDialog', ['open']);
    const createTopicsResponse = of(true);
    topicsCreateServiceMock = mockServiceSpy;
    topicsCreateServiceMock.createTopics.and.returnValue(createTopicsResponse);

    TestBed.configureTestingModule({
      declarations: [TopicsCreateComponent],

      providers: [TranslateService, MessageService,
        { provide: TopicsCreateService, useValue: mockServiceSpy },
        { provide: CookieService, useValue: mockCookieServiceSpy },
        { provide: MatDialog, useValue: mockDialogSpy },
      ],
      imports: [
        BrowserAnimationsModule,MatSnackBarModule,
        RouterTestingModule.withRoutes(routes),
        HttpClientTestingModule, TranslateModule.forRoot()
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(TopicsCreateComponent);
    component = fixture.componentInstance;
    mockTopicsCreateService = TestBed.inject(
      TopicsCreateService
    ) as jasmine.SpyObj<TopicsCreateService>;
    mockCookieService = TestBed.inject(
      CookieService
    ) as jasmine.SpyObj<CookieService>;
    mockDialog = TestBed.inject(MatDialog) as jasmine.SpyObj<MatDialog>;

    mockCookieService.get.and.returnValue('testUser');

    translateService = TestBed.inject(
      TranslateService
    ) as jasmine.SpyObj<TranslateService>;

    messageService = TestBed.inject(
      MessageService
    ) as jasmine.SpyObj<MessageService>;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should not call createTopics when valuesValidation is false', () => {
    spyOn(component, 'valuesValidation').and.returnValue(false);

    component.createTopics();

    expect(mockTopicsCreateService.createTopics).not.toHaveBeenCalled();
  });
});
