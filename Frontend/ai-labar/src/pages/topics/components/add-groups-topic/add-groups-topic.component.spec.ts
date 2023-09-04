import { ComponentFixture, TestBed } from '@angular/core/testing';
import {
  MatDialogModule,
  MatDialogRef,
  MAT_DIALOG_DATA,
} from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { AddGroupsTopicComponent } from './add-groups-topic.component';
import { TopicsCreateService } from '../topics-create/topics-create.service';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { MatSelectModule } from '@angular/material/select';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatRadioModule } from '@angular/material/radio';
import { of } from 'rxjs';
import { FormBuilder, FormsModule } from '@angular/forms';

describe('AddGroupsTopicComponent', () => {
  let component: AddGroupsTopicComponent;
  let fixture: ComponentFixture<AddGroupsTopicComponent>;
  let topicsCreateServiceSpy: jasmine.SpyObj<TopicsCreateService>;
  let cookieServiceSpy: jasmine.SpyObj<CookieService>;

  const groups = ['group1', 'group2'];
  const users = ['user1', 'user2'];

  beforeEach(async () => {
    const topicsCreateService = jasmine.createSpyObj('TopicsCreateService', [
      'getGroupsByUser',
      'getGroup',
    ]);
    const cookieService = jasmine.createSpyObj('CookieService', ['get']);
    const dialogRefSpy = jasmine.createSpyObj('MatDialogRef', ['close']);

    await TestBed.configureTestingModule({
      declarations: [AddGroupsTopicComponent],
      providers: [
        FormBuilder,
        { provide: TopicsCreateService, useValue: topicsCreateService },
        { provide: CookieService, useValue: cookieService },
        { provide: MatDialogRef, useValue: dialogRefSpy },
        { provide: MAT_DIALOG_DATA, useValue: {} },
      ],
      imports: [
        MatDialogModule,
        FormsModule,
        MatFormFieldModule,
        MatInputModule,
        MatCheckboxModule,
        MatRadioModule,
        MatSelectModule,
        BrowserAnimationsModule,
      ],
    }).compileComponents();

    topicsCreateServiceSpy = TestBed.inject(
      TopicsCreateService
    ) as jasmine.SpyObj<TopicsCreateService>;
    cookieServiceSpy = TestBed.inject(
      CookieService
    ) as jasmine.SpyObj<CookieService>;

    topicsCreateServiceSpy.getGroupsByUser.and.returnValue(
      of({ entity: groups })
    );
    topicsCreateServiceSpy.getGroup.and.returnValue(
      of({ entity: { members: users } })
    );
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AddGroupsTopicComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should close dialog on closeDialog call', () => {
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

  it('should call dialogRef.close with the correct data', () => {
    component.selectedGroup = groups[0];
    component.users = users;

    component.saveSelection();

    const expectedData = {
      grupoSeleccionado: groups[0],
      usuariosSeleccionados: users,
    };

    expect(component.dialogRef.close).toHaveBeenCalledWith(expectedData);
  });
});
