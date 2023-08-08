import { ComponentFixture, TestBed, tick, fakeAsync } from '@angular/core/testing';
import { CookieService } from 'ngx-cookie-service';

import { TopicsListComponent } from './topics-list.component';
import { TopicsListService } from './topics-list.service';
import { HttpClientTestingModule, HttpTestingController } from '@angular/common/http/testing';
import { MatDialogModule } from '@angular/material/dialog';
xdescribe('TopicsListComponent', () => {
  let component: TopicsListComponent;
  let fixture: ComponentFixture<TopicsListComponent>;



  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ TopicsListComponent ],
      imports: [MatDialogModule]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(TopicsListComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});


xdescribe('TopicsListComponent', () => {
  let component: TopicsListComponent;
  let fixture: ComponentFixture<TopicsListComponent>;
  let httpMock: HttpTestingController;
  let topicListService: TopicsListService;
  let cookieService: CookieService;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ TopicsListComponent ],
      imports: [ HttpClientTestingModule ],
      providers: [ CookieService, topicListService ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(TopicsListComponent);
    component = fixture.componentInstance;
    httpMock = TestBed.inject(HttpTestingController);
    topicListService = TestBed.inject(TopicsListService);
    cookieService = TestBed.inject(CookieService);

    spyOn(cookieService, 'get').and.returnValue('example-token');
  });

  afterEach(() => {
    httpMock.verify();
  });

  it('should load topics and set the data source', fakeAsync(() => {
    const url = 'http://localhost:8080/topics/loadTopics';
    const loadTopicsBody = {
      user: 'example-user',
      token: 'example-token'
    };
    const response = {
      entity: [
        { id: 1, name: 'Topic 1' },
        { id: 2, name: 'Topic 2' },
        { id: 3, name: 'Topic 3' }
      ]
    };

    component.getTopicList();

    const req = httpMock.expectOne(url);
    expect(req.request.method).toEqual('POST');
    expect(req.request.body).toEqual(loadTopicsBody);

    req.flush(response);

    tick();

  }));
});
