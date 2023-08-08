import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { AnyadirGruposTopicComponent } from './anyadir-grupos-topic.component';
import { TopicsCreateService } from '../topics-create/topics-create.service';
import { of } from 'rxjs';

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

    await TestBed.configureTestingModule({
      declarations: [ AnyadirGruposTopicComponent ],
      providers: [
        { provide: TopicsCreateService, useValue: topicsCreateService },
        { provide: CookieService, useValue: cookieService },
        { provide: MatDialogRef, useValue: {} },
        { provide: MAT_DIALOG_DATA, useValue: {} }
      ],
      imports: [MatDialogModule]
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

  it('should load groups on init', () => {
    expect(component.groups.length).toEqual(groups.length);
  });

  it('should load users when group is selected', () => {
    component.selectedGroup = groups[0];
    component.loadUsers();
    expect(component.users.length).toEqual(users.length);
  });

  // it('should close dialog on closeDialog call', () => {
  //   spyOn(component.dialogRef, 'close');
  //   component.closeDialog();
  //   expect(component.dialogRef.close).toHaveBeenCalled();
  // });

  // it('should return data on saveSelection call', () => {
  //   const expectedData = {
  //     grupoSeleccionado: groups[0],
  //     usuariosSeleccionados: users
  //   };
  //   component.selectedGroup = groups[0];
  //   component.users = users;
  //   const actualData = component.saveSelection();
  //   expect(actualData).toEqual(expectedData);
  // });
});