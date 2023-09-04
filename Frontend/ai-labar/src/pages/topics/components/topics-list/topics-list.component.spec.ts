import {
  ComponentFixture,
  TestBed,
  fakeAsync,
  tick,
} from '@angular/core/testing';
import { MatDialog } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { FormsModule } from '@angular/forms';
import { TopicsListComponent } from './topics-list.component';
import { TopicsListService } from './topics-list.service';
import { TopicsListServiceMock } from './topics-list.service.mock';
import { MatSortModule } from '@angular/material/sort';
import { MatTableDataSource, MatTableModule } from '@angular/material/table';
import { MatDialogModule } from '@angular/material/dialog';
import { MatPaginatorModule } from '@angular/material/paginator';
import { ModalVotationComponent } from '../modal-votation/modal-votation.component';
import { HttpClientModule } from '@angular/common/http';
import { MatButtonModule } from '@angular/material/button';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { environment } from 'src/environments/environment';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';

import { of } from 'rxjs';

describe('TopicsListComponent', () => {
  let component: TopicsListComponent;
  let fixture: ComponentFixture<TopicsListComponent>;
  let mockTopicListService: jasmine.SpyObj<TopicsListServiceMock>;
  let mockDialog: jasmine.SpyObj<MatDialog>;
  let mockCookieService: jasmine.SpyObj<CookieService>;
  let cookieServiceMock: Partial<CookieService>;

  beforeEach(() => {
    environment.mockup = true;

    const mockServiceSpy = jasmine.createSpyObj('TopicsListServiceMock', [
      'loadTopics_post',
      'reopenTopic',
      'deleteTopic',
      'closeTopic',
    ]);
    const mockDialogSpy = jasmine.createSpyObj('MatDialog', ['open']);
    const mockCookieServiceSpy = jasmine.createSpyObj('CookieService', [
      'get',
      'set',
    ]);
    cookieServiceMock = {
      get: (key: string) => 'mocked-value',
    };
    TestBed.configureTestingModule({
      declarations: [TopicsListComponent, ModalVotationComponent],
      providers: [
        { provide: TopicsListService, useValue: mockServiceSpy },
        { provide: TopicsListServiceMock, useValue: mockServiceSpy },
        { provide: MatDialog, useValue: mockDialogSpy },
        { provide: CookieService, useValue: mockCookieServiceSpy },
      ],
      imports: [
        MatSortModule,
        MatTableModule,
        BrowserAnimationsModule,
        HttpClientModule,
        MatPaginatorModule,
        MatSlideToggleModule,
        MatButtonModule,
        FormsModule,
        MatDialogModule,
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(TopicsListComponent);
    component = fixture.componentInstance;
    mockTopicListService = TestBed.inject(
      TopicsListServiceMock
    ) as jasmine.SpyObj<TopicsListServiceMock>;
    mockDialog = TestBed.inject(MatDialog) as jasmine.SpyObj<MatDialog>;
    mockCookieService = TestBed.inject(
      CookieService
    ) as jasmine.SpyObj<CookieService>;
    const mockResponse = { entity: [] };
    mockTopicListService.loadTopics_post.and.returnValue(of(mockResponse));

    mockCookieService.get.and.returnValue('testUser');
  });
  afterEach(() => {
    environment.mockup = false;
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should call getTopicList on init', () => {
    const mockResponse = { entity: [] };

    fixture.detectChanges();

    expect(mockTopicListService.loadTopics_post).toHaveBeenCalled();
    expect(component.dataSource.data).toEqual(mockResponse.entity);
  });

  it('should add option to selectedOptions when onOptionChange is called', () => {
    const option = 'Option 1';

    component.onOptionChange(option);

    expect(component.selectedOptions).toContain(option);
  });

  it('should remove option from selectedOptions when onOptionChange is called with existing option', () => {
    const option = 'Option 1';
    component.selectedOptions = [option];

    component.onOptionChange(option);

    expect(component.selectedOptions).not.toContain(option);
  });

  it('should reOpen a votation', () => {
    const votation = { id: 12 };

    const mockResponse = {};
    mockTopicListService.reopenTopic.and.returnValue(of(mockResponse));

    component.reOpen(votation);

    expect(mockTopicListService.reopenTopic).toHaveBeenCalledWith({
      id: votation.id,
      user: 'testUser',
      token: 'testUser',
    });

    expect(mockTopicListService.loadTopics_post).toHaveBeenCalled();
  });
  it('should close a votation', () => {
    const votation = { id: 1 };

    const mockResponse = { entity: [] };
    mockTopicListService.closeTopic.and.returnValue(of(mockResponse));

    component.close(votation);

    expect(mockTopicListService.closeTopic).toHaveBeenCalledWith({
      id: votation.id,
      user: 'testUser',
      token: 'testUser',
    });

    expect(mockTopicListService.loadTopics_post).toHaveBeenCalled();
  });

  it('should change visualization Scroll', fakeAsync(() => {
    spyOn(component, 'defaultVisualization').and.callThrough();

    const visualization = 'Scroll';
    component.changeDisplay(visualization);
    tick(250);
    expect(component.defaultVisualization).toHaveBeenCalledWith(visualization);
    expect(mockTopicListService.loadTopics_post).toHaveBeenCalled();
  }));
  it('should change visualization pagination', fakeAsync(() => {
    spyOn(component, 'defaultVisualization').and.callThrough();

    const visualization = 'Pagination';
    component.changeDisplay(visualization);
    tick(250);
    expect(component.defaultVisualization).toHaveBeenCalledWith(visualization);
    expect(mockTopicListService.loadTopics_post).toHaveBeenCalled();
  }));
  it('should change visualization Cards', fakeAsync(() => {
    spyOn(component, 'defaultVisualization').and.callThrough();

    const visualization = 'Cards';
    component.changeDisplay(visualization);
    tick(250);
    expect(component.defaultVisualization).toHaveBeenCalledWith(visualization);
    expect(mockTopicListService.loadTopics_post).toHaveBeenCalled();
  }));
  it('should reset filters and fetch topic list when filters are changed', fakeAsync(() => {
    spyOn(component, 'getTopicList');
    component.pageIndex = 2;
    component.dataSource = new MatTableDataSource<any>([
      /* your mock data */
    ]);

    component.onToggleChange({ source: { id: 'minesToggle' }, checked: true });

    tick();

    expect(component.pageIndex).toBe(1);
    expect(component.getTopicList).toHaveBeenCalled();
  }));
  it('should change filters when toggles are changed', fakeAsync(() => {
    component.onToggleChange({ source: { id: 'minesToggle' }, checked: true });
    tick(250);
    component.onToggleChange({ source: { id: 'openedToggle' }, checked: true });
    tick(250);
    component.onToggleChange({
      source: { id: 'closedToggle' },
      checked: false,
    });
    tick(250);
    component.onToggleChange({
      source: { id: 'votePendingFilter' },
      checked: true,
    });
    tick(250);

    expect(component.minesFilter).toBeTrue();
    expect(component.openedFilter).toBeTrue();
    expect(component.closedFilter).toBeFalse();
    expect(component.votePendingFilter).toBeTrue();
  }));
});
