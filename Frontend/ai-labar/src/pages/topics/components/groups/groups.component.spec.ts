import { ComponentFixture, TestBed } from '@angular/core/testing';
import {
  MatDialogModule,
  MatDialogRef,
  MAT_DIALOG_DATA,
} from '@angular/material/dialog';
import { FormBuilder, ReactiveFormsModule } from '@angular/forms';
import { CookieService } from 'ngx-cookie-service';
import { TopicsListService } from '../topics-list/topics-list.service';
import { GroupsComponent } from './groups.component';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { MatInputModule } from '@angular/material/input';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { ElementRef } from '@angular/core';
import { environment } from 'src/environments/environment';
import { of } from 'rxjs';
import { IUser } from '../interfaces/emoji.model';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('GroupsComponent', () => {
  let component: GroupsComponent;
  let fixture: ComponentFixture<GroupsComponent>;
  let mockDialogRef: MatDialogRef<GroupsComponent>;
  let mockTopicsListService: jasmine.SpyObj<TopicsListService>;
  const mockUser: IUser = { name: 'TestUser', checked: false, hidden: false };

  beforeEach(() => {
    mockDialogRef = jasmine.createSpyObj('MatDialogRef', ['close']);
    mockTopicsListService = jasmine.createSpyObj('TopicsListService', [
      'post',
      'postResponse',
    ]);

    TestBed.configureTestingModule({
      declarations: [GroupsComponent],
      imports: [
        ReactiveFormsModule,
        MatDialogModule,
        BrowserAnimationsModule,
        MatCheckboxModule,
        MatFormFieldModule,
        MatInputModule,HttpClientTestingModule, TranslateModule.forRoot()
      ],
      providers: [TranslateService,
        FormBuilder,
        CookieService,
        { provide: TopicsListService, useValue: mockTopicsListService },
        { provide: MatDialogRef, useValue: mockDialogRef },
        { provide: MAT_DIALOG_DATA, useValue: {} },
        { provide: ElementRef, useValue: {} },
      ],
    }).compileComponents();

    fixture = TestBed.createComponent(GroupsComponent);
    component = fixture.componentInstance;

    const cookieService = TestBed.inject(CookieService);
    component.setCookie();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should load users and initialize form', () => {
    const mockResponse = { body: { entity: [mockUser.name] } };
    mockTopicsListService.postResponse.and.returnValue(of(mockResponse));

    component.getUsers();

    expect(mockTopicsListService.postResponse).toHaveBeenCalled();
    expect(component.usersNames).toEqual([mockUser.name]);
    expect(component.users.length).toBe(1);
    expect(component.groupsForm.controls[mockUser.name]).toBeDefined();
  });

  it('should filter and display users', () => {
    const mockResponse = { body: { entity: [mockUser.name] } };
    mockTopicsListService.postResponse.and.returnValue(of(mockResponse));

    component.groupsForm.get('searcher')!.setValue('test');

    component.filterUsers();

    expect(component.filtering).toBe(true);
    expect(component.matcher).toBe('test');
    expect(mockTopicsListService.postResponse).toHaveBeenCalled();
    expect(component.showUsers).toBe(true);
    expect(component.usersNames).toEqual([mockUser.name]);
  });

  it('should select and deselect users', () => {
    const user: any = { name: 'user1', checked: false };
    component.selectUser(user);
    expect(user.checked).toBe(true);
    expect(component.selectedUsers).toContain('user1');

    component.selectUser(user);
    expect(user.checked).toBe(false);
    expect(component.selectedUsers).not.toContain('user1');
  });

  it('should save group', () => {
    const formValues = {
      groupName: 'Test Group',
      currentSelection: ['user1', 'user2'],
      searcher: 'initialValue',
    };

    component.groupsForm.setValue(formValues);
    component.selectedUsers = ['user1', 'user2'];
    mockTopicsListService.post.and.returnValue(of(true));

    component.saveGroup();

    expect(mockTopicsListService.post).toHaveBeenCalledWith(
      {
        groupName: 'Test Group',
        members: ['user1', 'user2'],
        user: 'testUser',
        token: 'testToken',
      },
      `${environment.apiUrl}/groups/createGroup`
    );
    expect(mockDialogRef.close).toHaveBeenCalled();
  });
});
