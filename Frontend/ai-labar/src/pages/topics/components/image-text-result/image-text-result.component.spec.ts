import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatDialogModule, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { HttpClientModule } from '@angular/common/http';
import { ImageTextResultComponent } from './image-text-result.component';
import { TopicsListService } from '../topics-list/topics-list.service';
import { ResultadosVotacionService } from '../voting-results/voting-results.service';
import { of } from 'rxjs';

describe('ImageTextResultComponent', () => {
  let component: ImageTextResultComponent;
  let fixture: ComponentFixture<ImageTextResultComponent>;

  const mockDialogData = {
    votacion: {
      id: 1,
      title: 'Test Voting',
    },
  };

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ImageTextResultComponent],
      imports: [MatDialogModule, HttpClientModule],
      providers: [
        CookieService,
        TopicsListService,
        ResultadosVotacionService,
        { provide: MAT_DIALOG_DATA, useValue: mockDialogData },
      ],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ImageTextResultComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load results', () => {
    // Mock the response from your service
    const mockResponse = {
      entity: [
        {
          votes: 5,
          image: 'mock-image-url',
          option: 'Option 1',
        },
        {
          votes: 3,
          image: 'mock-image-url',
          option: 'Option 2',
        },
      ],
    };

    // Spy on the service method
    const topicListService = TestBed.inject(TopicsListService);
    const postSpy = spyOn(topicListService, 'post').and.returnValue(
      // Return a mock observable with the response using 'of'
      of(mockResponse)
    );

    component.ngOnInit();

    expect(postSpy).toHaveBeenCalled();
    expect(component.result).toEqual(mockResponse.entity);
  });
});
