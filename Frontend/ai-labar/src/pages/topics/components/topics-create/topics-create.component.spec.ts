import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatDialog } from '@angular/material/dialog';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { TopicsCreateComponent } from './topics-create.component';
import { TopicsCreateService } from './topics-create.service';
import { CookieService } from 'ngx-cookie-service';
import { of } from 'rxjs';
import { PasoDosComponent } from './pasos/paso-dos/paso-dos.component';
import { RouterTestingModule } from '@angular/router/testing';
import { RouterModule, Routes } from '@angular/router';
import { TopicsListComponent } from '../topics-list/topics-list.component';


const routes: Routes = [
  // Otras rutas si las tienes
  { path: 'topics/topics-list', component: TopicsListComponent }, // Asegúrate de que el componente sea un componente de prueba
];

describe('TopicsCreateComponent', () => {
  let component: TopicsCreateComponent;
  let fixture: ComponentFixture<TopicsCreateComponent>;
  let mockTopicsCreateService: jasmine.SpyObj<TopicsCreateService>;
  let mockCookieService: jasmine.SpyObj<CookieService>;
  let mockDialog: jasmine.SpyObj<MatDialog>;
  let topicsCreateServiceMock: jasmine.SpyObj<TopicsCreateService>;

  beforeEach(() => {
    const mockServiceSpy = jasmine.createSpyObj('TopicsCreateService', ['createTopics','guardarEncuesta']);
    const mockCookieServiceSpy = jasmine.createSpyObj('CookieService', ['get']);
    const mockDialogSpy = jasmine.createSpyObj('MatDialog', ['open']);
    const createTopicsResponse = of(true); 
    //topicsCreateServiceMock = jasmine.createSpyObj('TopicsCreateService', ['createTopics']).and.returnValue(createTopicsResponse);
    topicsCreateServiceMock = mockServiceSpy;
    topicsCreateServiceMock.createTopics.and.returnValue(createTopicsResponse);
  

    TestBed.configureTestingModule({
      declarations: [TopicsCreateComponent],

      providers: [
        { provide: TopicsCreateService, useValue: mockServiceSpy },
        { provide: CookieService, useValue: mockCookieServiceSpy },
        { provide: MatDialog, useValue: mockDialogSpy },
      ],
      imports: [BrowserAnimationsModule,
        RouterTestingModule.withRoutes(routes)],
    }).compileComponents();

    fixture = TestBed.createComponent(TopicsCreateComponent);
    component = fixture.componentInstance;
    mockTopicsCreateService = TestBed.inject(TopicsCreateService) as jasmine.SpyObj<TopicsCreateService>;
    mockCookieService = TestBed.inject(CookieService) as jasmine.SpyObj<CookieService>;
    mockDialog = TestBed.inject(MatDialog) as jasmine.SpyObj<MatDialog>;

    mockCookieService.get.and.returnValue('testUser');
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should call createTopics and navigate when validaciónValores is true', () => {
    // Arrange
    component.componenteHijo = new PasoDosComponent(mockDialog);
    component.componenteHijo.valorTextbox = 'Test Title';
    component.componenteHijo.isEncuestaOpinion = true;
    component.componenteHijo.selectedType = 'simple';
    component.componenteHijo.valorEncuesta1 = 'Option 1';
    component.componenteHijo.valorEncuesta2 = 'Option 2';
    component.componenteHijo.fechaCierre = '01/01/2023';
    component.members.push('testUser');
    spyOn(component, 'validaciónValores').and.returnValue(true);

    const createTopicsBody = {
      title: 'Test Title',
      type: component.typeTS,
      question: 'prueba',
      options: [
        { option: 'Option 1' },
        { option: 'Option 2' },
      ],
      user: 'testUser',
      members: ['testUser'],
      closeDate: '01/01/2023',
      token: 'testUser',
    };
   // mockTopicsCreateService.createTopics.and.returnValue(of(true)); 

    mockDialog.open.and.callThrough();
    // Act
    component.guardarEncuesta();
    //component.createTopics();

    // Assert
    expect(topicsCreateServiceMock.createTopics).toHaveBeenCalledWith(createTopicsBody);
    //expect(mockDialog.open).toHaveBeenCalled(); // Adjust this as needed
  });

  it('should not call createTopics when validaciónValores is false', () => {
    // Arrange
    spyOn(component, 'validaciónValores').and.returnValue(false);

    // Act
    component.createTopics();

    // Assert
    expect(mockTopicsCreateService.createTopics).not.toHaveBeenCalled();
  });

  
  // Add more tests for other methods as needed

});
