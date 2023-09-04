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

      providers: [
        { provide: TopicsCreateService, useValue: mockServiceSpy },
        { provide: CookieService, useValue: mockCookieServiceSpy },
        { provide: MatDialog, useValue: mockDialogSpy },
      ],
      imports: [
        BrowserAnimationsModule,
        RouterTestingModule.withRoutes(routes),
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
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should call createTopics and navigate when valuesValidation is true', () => {
    component.childComponent = new StepTwoComponent(mockDialog);
    component.childComponent.textBoxValue = 'Test Title';
    component.childComponent.isOpinionSurvey = true;
    component.childComponent.selectedType = 'simple';
    component.childComponent.surveyValue1 = 'Option 1';
    component.childComponent.surveyValue2 = 'Option 2';
    component.childComponent.closingDate = '01/01/2023';
    component.groupSelectedParticipants.push('testUser');
    spyOn(component, 'valuesValidation').and.returnValue(true);

    const createTopicsBody = {
      title: 'Test Title',
      type: component.typeTS,
      question: 'test',
      options: [{ option: 'Option 1' }, { option: 'Option 2' }],
      user: 'testUser',
      groupName: ['testUser'],
      closeDate: '01/01/2023',
      token: 'testUser',
    };

    mockDialog.open.and.callThrough();
    component.saveSurvey();

    expect(topicsCreateServiceMock.createTopics).toHaveBeenCalledWith(
      createTopicsBody
    );
  });

  it('should not call createTopics when valuesValidation is false', () => {
    spyOn(component, 'valuesValidation').and.returnValue(false);

    component.createTopics();

    expect(mockTopicsCreateService.createTopics).not.toHaveBeenCalled();
  });
});
