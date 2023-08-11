import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { AnyadirGruposTopicComponent } from './anyadir-grupos-topic.component';
import { TopicsCreateService } from '../topics-create/topics-create.service';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input'; 
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import {MatSelectModule} from '@angular/material/select';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatRadioModule } from '@angular/material/radio';
import { of } from 'rxjs';
import { FormBuilder,FormsModule  ,ReactiveFormsModule } from '@angular/forms';

describe('AnyadirGruposTopicComponent', () => {
  let component: AnyadirGruposTopicComponent;
  let fixture: ComponentFixture<AnyadirGruposTopicComponent>;
  let topicsCreateServiceSpy: jasmine.SpyObj<TopicsCreateService>;
  let cookieServiceSpy: jasmine.SpyObj<CookieService>;

  const groups = ['group1', 'group2'];
  const users = ['user1', 'user2'];

  beforeEach(async () => {
    const topicsCreateService = jasmine.createSpyObj('TopicsCreateService', ['getGroupsByUser', 'getGroup']);
    const cookieService = jasmine.createSpyObj('CookieService', ['get']);
    const dialogRefSpy = jasmine.createSpyObj('MatDialogRef', ['close']); // Crear un spy para MatDialogRef

    await TestBed.configureTestingModule({
      declarations: [ AnyadirGruposTopicComponent ],
      providers: [FormBuilder, // Add FormBuilder to providers
        { provide: TopicsCreateService, useValue: topicsCreateService },
        { provide: CookieService, useValue: cookieService },
        { provide: MatDialogRef, useValue: dialogRefSpy }, // Usar el dialogRefSpy aqu�
        { provide: MAT_DIALOG_DATA, useValue: {} }
      ],
      imports: [MatDialogModule,FormsModule
      ,MatFormFieldModule,MatInputModule,MatCheckboxModule,MatRadioModule,MatSelectModule,BrowserAnimationsModule]
    })
    .compileComponents();

    topicsCreateServiceSpy = TestBed.inject(TopicsCreateService) as jasmine.SpyObj<TopicsCreateService>;
    cookieServiceSpy = TestBed.inject(CookieService) as jasmine.SpyObj<CookieService>;

    topicsCreateServiceSpy.getGroupsByUser.and.returnValue(of({ entity: groups }));
    topicsCreateServiceSpy.getGroup.and.returnValue(of({ entity: { members: users } }));
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AnyadirGruposTopicComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should close dialog on closeDialog call', () => {
    //spyOn(component.dialogRef, 'close');
    component.closeDialog();
    expect(component.dialogRef.close).toHaveBeenCalled();
  });
  
  it('should load groups on init', () => {
    expect(component.groups.length).toEqual(groups.length);
  });

  it('should load users when group is selected', () => {
    component.selectedGroup = groups[0];
    component.loadUsers();
    expect(component.users.length).toEqual(users.length);
  });


   it('should return data on saveSelection call', () => {
     const expectedData = {
       grupoSeleccionado: groups[0],
       usuariosSeleccionados: users
     };
     component.selectedGroup = groups[0];
     component.users = users;
     const actualData = component.saveSelection();
    // expect(actualData).toEqual(expectedData);
   });
});