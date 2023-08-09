import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatDialog, MatDialogRef } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';

import { TopicsListComponent } from './topics-list.component';
import { TopicsListService } from './topics-list.service';
import { TopicsListServiceMock } from './topics-list.service.mock';
import { MatSortModule } from '@angular/material/sort';
import { MatTableModule } from '@angular/material/table';
import { MatDialogModule } from '@angular/material/dialog';
// Importa el componente app-modal-votacion
import { ModalVotacionComponent } from '../modal-votacion/modal-votacion.component'; 
import { HttpClientModule } from '@angular/common/http'; // Agrega esta línea

import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { environment } from 'src/environments/environment'; // Asegúrate de importar el environment

import { of } from 'rxjs';
import { Console } from 'console';

describe('TopicsListComponent', () => {
  let component: TopicsListComponent;
  let fixture: ComponentFixture<TopicsListComponent>;
  let mockTopicListService: jasmine.SpyObj<TopicsListServiceMock>;
  let mockDialog: jasmine.SpyObj<MatDialog>;
  let mockCookieService: jasmine.SpyObj<CookieService>;

  beforeEach(() => {
    environment.mockup = true; // Configura el environment.mockup a true para las pruebas

    const mockServiceSpy = jasmine.createSpyObj('TopicsListServiceMock', ['loadTopics_post', 'reopenTopic', 'deleteTopic', 'closeTopic']);
    //const mockServiceSpy = jasmine.createSpyObj('TopicsListService', ['post', 'put', 'delete']);
    const mockDialogSpy = jasmine.createSpyObj('MatDialog', ['open']);
    const mockCookieServiceSpy = jasmine.createSpyObj('CookieService', ['get']);

    TestBed.configureTestingModule({
      declarations: [TopicsListComponent,ModalVotacionComponent],
      providers: [
        { provide: TopicsListService, useValue: mockServiceSpy },
        { provide: TopicsListServiceMock, useValue: mockServiceSpy },
        { provide: MatDialog, useValue: mockDialogSpy },
        { provide: CookieService, useValue: mockCookieServiceSpy }
      ],
      imports: [MatSortModule, MatTableModule,
        BrowserAnimationsModule,HttpClientModule,
        MatDialogModule]
    }).compileComponents();

    fixture = TestBed.createComponent(TopicsListComponent);
    component = fixture.componentInstance;
    mockTopicListService = TestBed.inject(TopicsListServiceMock) as jasmine.SpyObj<TopicsListServiceMock>;
    mockDialog = TestBed.inject(MatDialog) as jasmine.SpyObj<MatDialog>;
    mockCookieService = TestBed.inject(CookieService) as jasmine.SpyObj<CookieService>;
    const mockResponse = { entity: [] };
    mockTopicListService.loadTopics_post.and.returnValue(of(mockResponse));

    mockCookieService.get.and.returnValue('testUser');
  });
  afterEach(() => {
    environment.mockup = false; // Restablece el environment.mockup después de las pruebas
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });


  // Add more tests for other methods and scenarios as needed
  it('should call getTopicList on init', () => {
    // Arrange
   // mockTopicListService.post.and.returnValue(of({ entity: [] }));
    const mockResponse = { entity: [] };
    // Act
    fixture.detectChanges();

    // Assert
    //expect(mockTopicListService.post).toHaveBeenCalled();
    expect(mockTopicListService.loadTopics_post).toHaveBeenCalled();
    expect(component.dataSource.data).toEqual(mockResponse.entity);
  });



  it('should add option to selectedOptions when onOptionChange is called', () => {
    // Arrange
    const option = 'Option 1';

    // Act
    component.onOptionChange(option);

    // Assert
    expect(component.selectedOptions).toContain(option);
  });

  it('should remove option from selectedOptions when onOptionChange is called with existing option', () => {
    // Arrange
    const option = 'Option 1';
    component.selectedOptions = [option];

    // Act
    component.onOptionChange(option);

    // Assert
    expect(component.selectedOptions).not.toContain(option);
  });


  it('should reOpen a votation', () => {
    // Arrange
    const votation = { id: 12 };

    // Mock the service response
    //mockTopicListService.put.and.returnValue(of({}));
    const mockResponse = {};
    mockTopicListService.reopenTopic.and.returnValue(of(mockResponse));
 
    // Act
    component.reOpen(votation);
     

    // Assert
    expect(mockTopicListService.reopenTopic).toHaveBeenCalledWith({
      id: votation.id,
      user: 'testUser',
      token: 'testUser' // Adjust this based on your actual logic
    } ); // Make sure correct URL is used

    
    // Verify that getTopicList is called after reopening
    expect(mockTopicListService.loadTopics_post).toHaveBeenCalled();
    
    
    /*expect(mockTopicListService.put).toHaveBeenCalledWith({
      id: votation.id,
      user: 'testUser',
      token: 'testUser' // Adjust this based on your actual logic
    }, 'http://localhost:8080/topics/reOpenTopic'); // Make sure correct URL is used

    // Verify that getTopicList is called after reopening
    expect(mockTopicListService.post).toHaveBeenCalled();*/
  });
  it('should close a votation', () => {
    // Arrange
    const votation = { id: 1 };

    // Mock the service response
    //mockTopicListService.put.and.returnValue(of({}));
    const mockResponse = { entity: [] };
    mockTopicListService.closeTopic.and.returnValue(of(mockResponse));

    // Act
    component.close(votation);

    // Assert
    expect(mockTopicListService.closeTopic).toHaveBeenCalledWith({
      id: votation.id,
      user: 'testUser',
      token: 'testUser' // Adjust this based on your actual logic
    } ); // Make sure correct URL is used

    // Verify that getTopicList is called after closing
    expect(mockTopicListService.loadTopics_post).toHaveBeenCalled();
    /*expect(mockTopicListService.put).toHaveBeenCalledWith({
      id: votation.id,
      user: 'testUser',
      token: 'testUser' // Adjust this based on your actual logic
    }, 'http://localhost:8080/topics/closeTopic'); // Make sure correct URL is used

    // Verify that getTopicList is called after closing
    expect(mockTopicListService.post).toHaveBeenCalled();*/
  });


  /** */
});
