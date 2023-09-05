import { ComponentFixture, TestBed } from '@angular/core/testing';
import {
  MatDialogModule,
  MatDialogRef,
  MAT_DIALOG_DATA,
} from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { AddCandidatesTopicComponent } from './add-candidates-topic.component';
import { TopicsCreateService } from '../topics-create/topics-create.service';
import { TopicsListService } from '../topics-list/topics-list.service';
import { FormBuilder, ReactiveFormsModule } from '@angular/forms';
import { MatRadioModule } from '@angular/material/radio';
import { IUser } from '../interfaces/emoji.model';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatInputModule } from '@angular/material/input';
import { MatSelectModule } from '@angular/material/select';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { of } from 'rxjs';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';

describe('AddCandidatesTopicComponent', () => {
  let component: AddCandidatesTopicComponent;
  let fixture: ComponentFixture<AddCandidatesTopicComponent>;
  let topicsCreateServiceSpy: jasmine.SpyObj<TopicsCreateService>;
  let cookieServiceSpy: jasmine.SpyObj<CookieService>;
  let topicsListServiceSpy: jasmine.SpyObj<TopicsListService>;

  const groups = ['group1', 'group2'];
  const users = ['user1', 'user2'];

  beforeEach(async () => {
    const topicsCreateService = jasmine.createSpyObj('TopicsCreateService', [
      'getGroupsByUser',
      'getGroup',
    ]);
    const cookieService = jasmine.createSpyObj('CookieService', ['get']);
    const topicsListService = jasmine.createSpyObj('TopicsListService', [
      'postResponse',
    ]);
    const dialogRefSpy = jasmine.createSpyObj('MatDialogRef', ['close']);

    await TestBed.configureTestingModule({
      declarations: [AddCandidatesTopicComponent],
      providers: [TranslateService,
        { provide: TopicsCreateService, useValue: topicsCreateService },
        { provide: CookieService, useValue: cookieService },
        { provide: TopicsListService, useValue: topicsListService },
        FormBuilder,
        { provide: MatDialogRef, useValue: dialogRefSpy },
        { provide: MAT_DIALOG_DATA, useValue: {} },
      ],
      imports: [
        MatDialogModule,
        ReactiveFormsModule,
        MatFormFieldModule,
        MatInputModule,
        MatRadioModule,
        BrowserAnimationsModule,
        MatSelectModule,
        MatCheckboxModule,HttpClientTestingModule, TranslateModule.forRoot()
      ],
    }).compileComponents();

    topicsCreateServiceSpy = TestBed.inject(
      TopicsCreateService
    ) as jasmine.SpyObj<TopicsCreateService>;
    cookieServiceSpy = TestBed.inject(
      CookieService
    ) as jasmine.SpyObj<CookieService>;
    topicsListServiceSpy = TestBed.inject(
      TopicsListService
    ) as jasmine.SpyObj<TopicsListService>;

    topicsCreateServiceSpy.getGroupsByUser.and.returnValue(
      of({ entity: groups })
    );
    topicsCreateServiceSpy.getGroup.and.returnValue(
      of({ entity: { members: users } })
    );
    topicsListServiceSpy.postResponse.and.returnValue(
      of({ body: { entity: users } })
    );
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AddCandidatesTopicComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load groups on init', () => {
    expect(component.groups.length).toEqual(groups.length);
  });

  it('should load users on init', () => {
    component.ngOnInit();
    expect(component.usersNames.length).toEqual(users.length);
  });
  it('should load users for selected group', () => {
    const selectedGroup = groups[0];
    component.selectedGroup = selectedGroup;
    component.loadUsers();
    expect(component.usersGroups.length).toEqual(users.length);
  });
  it('should select and clear users', () => {
    const user1: IUser = { name: 'user1', checked: false, hidden: false };
    const user2: IUser = { name: 'user2', checked: false, hidden: false };

    component.selectUser(user1);
    expect(user1.checked).toBe(true);
    expect(component.selectedUsers).toContain(user1.name);

    component.selectUser(user2);
    expect(user2.checked).toBe(true);
    expect(component.selectedUsers).toContain(user2.name);

    component.selectUser(user1);
    expect(user1.checked).toBe(false);
    expect(component.selectedUsers).not.toContain(user1.name);

    component.clearSelection();
    expect(component.selectedUsers.length).toEqual(0);
  });

  it('should save group and close dialog', () => {
    const expectedData = {
      selectedGroup: null,
      selectedUsers: ['user1', 'user2'],
    };

    component.selectedUsers = ['user1', 'user2'];
    component.saveGroup();

    expect(component.dialogRef.close).toHaveBeenCalledWith(expectedData);
  });
});
