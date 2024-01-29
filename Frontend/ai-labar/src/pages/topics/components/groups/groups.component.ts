/**
 * This component represents the group creation dialog in the application.
 * It allows users to select and create groups with selected members.
 */
import {
  Component,
  OnInit,
  Inject,
  ViewChild,
  ElementRef,
  OnDestroy,
  ChangeDetectorRef,
} from '@angular/core';
import {
  FormBuilder,
  FormControl,
  FormGroup,
  Validators,
} from '@angular/forms';
import { IUser } from '../interfaces/emoji.model';
import { CookieService } from 'ngx-cookie-service';
import { TopicsListService } from '../topics-list/topics-list.service';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { environment } from 'src/environments/environment';
import { TranslateService } from '@ngx-translate/core';
import { Subject, takeUntil } from 'rxjs';
import { MessageService } from '../../services/message.service';
import { TopicsCreateService } from '../topics-create/topics-create.service';
import { MatCheckboxChange } from '@angular/material/checkbox';

@Component({
  selector: 'app-groups',
  templateUrl: './groups.component.html',
  styleUrls: ['./groups.component.scss'],
})
export class GroupsComponent implements OnInit, OnDestroy {
  // Whether the user list is being filtered
  filtering = true;

  // Array to store the names of all users
  usersNames: Array<string> = [];

  // Array to store all user objects
  users: Array<IUser> = [];

  // Array to store filtered user objects
  filteredUsers: IUser[] = [];

  // Array to store selected user names
  selectedUsers: string[] = [];

  // Array to store selected user names
  selectedUsersGroup: string[] = [];

  // FormGroup for the group creation form
  groupsForm: FormGroup = this.fb.group({
    groupName: ['', Validators.required],
    searcher: [''],
    searcherGroup: [''],
    currentSelection: [''],
    selectedOption: ['create'], // Initial value for selectedOption
    selectedGroup: '',
  });

  // Whether to show the list of users
  showUsers: boolean = false;

  // Search text matcher for user filtering
  matcher: string = '';

  // Whether to show the selected users
  showSelected: boolean = false;

  showSelectedGroup: boolean = false;
  private searchTimer: any;
  // Reference to the checkbox container element
  @ViewChild('checkboxContainer')
  checkboxContainer!: ElementRef;

  // Selected option: 'create' or 'edit'
  selectedOption: string = 'create';

  // Previously selected option
  oldSelectedOption: string | undefined;

  // Currently selected group
  selectedGroup: string | undefined;

  // List of groups the users belong to
  usersGroups: Array<IUser> = [];

  usersGroupsNames: Array<string> = [];

  // Maximum number of candidates allowed
  limitCandidates: number = 8;

  // List of available groups
  groups: string[] = [];

  isFilterUsersGroups: boolean = false;

  // Flag to show user list
  showUsersGroups: boolean = false;

  idGroup!: number;

  actualGroupName!: string;

  private ngUnsubscribe = new Subject();

  isChecked: boolean = true;


  constructor(
    private fb: FormBuilder,
    private cookie: CookieService,
    private topicListService: TopicsListService,
    public dialogRef: MatDialogRef<GroupsComponent>,
    private translate: TranslateService,
    private messageService: MessageService,
    private topicsCreateService: TopicsCreateService,
    private changeDetectorRef: ChangeDetectorRef,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnDestroy(): void {
    this.ngUnsubscribe.complete();
  }

  ngOnInit(): void {
    // Initialize the component
    //this.getUsers();
  }

  /**
   * Fetches the list of users from the server and loads them into the form.
   */
  getUsers() {
    const url = `${environment.apiUrl}/users/getAllUsers`;
    const loadTopicsBody = {
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
    };
    this.topicListService
      .postResponse(loadTopicsBody, url)
      .subscribe((response) => {
        if (response && response.body) {
          this.usersNames = response.body.entity;
          this.loadForm();
        }
      });
  }

  /**
   * Loads the user list into the form and sets up FormControl listeners.
   */
  loadFormGroup() {
    this.usersGroups = [];
    if(!this.isFilterUsersGroups){
      this.selectedUsersGroup = [];
    }
    this.usersGroupsNames.map((item) => {
      //const username = item;
      const username = item.split('(')[0].trim();
      let user = {
        name: item,
        checked: this.selectedUsersGroup.includes(username),
        hidden: false,
        modal: this.selectedUsersGroup.includes(username)
      };
      this.usersGroups.push(user);
      if (!this.isFilterUsersGroups) {
        this.selectUserGroup(user);
      }
    });
    this.usersGroups.forEach((user) => {
      this.groupsForm.addControl(user.name, new FormControl());
      this.groupsForm.patchValue(user);
    });
    this.showSelectedGroup = true;
    this.changeDetectorRef.detectChanges();
  }

  /**
   * Loads the user list into the form and sets up FormControl listeners.
   */
  loadForm() {
    this.users = [];
    this.usersNames.map((item) => {
      //const username = item;
      const username = item.split('(')[0].trim();
      let user = {
        name: item,
        checked: this.selectedUsers.includes(username),
        hidden: false,
        modal: this.selectedUsers.includes(username)
      };
      this.users.push(user);
    });
    this.users.forEach((user) => {
      this.groupsForm.addControl(user.name, new FormControl());
    });
  }

  /**
   * Filters the list of users based on the search criteria.
   */
  filterUsers(): void {
    this.filtering = true;
    this.users = [];
    let search = this.groupsForm.value.searcher.toUpperCase();
    if (this.searchTimer) {
      clearTimeout(this.searchTimer);
    }
    if (search.length >= 3) {
      this.matcher = search;
      this.searchTimer = setTimeout(() => {
        this.getUsersFilter();
      }, 400);
      this.showUsers = true;
    } else {
      this.showUsers = false;
    }
  }

  /**
   * Filters the list of users based on the search criteria.
   */
  filterUsersGroup(): void {
    this.filtering = true;
    this.usersGroups = [];
    let search = this.groupsForm.value.searcherGroup.toUpperCase();
    if (this.searchTimer) {
      clearTimeout(this.searchTimer);
    }
    if (search.length >= 3) {
      this.matcher = search;
      this.searchTimer = setTimeout(() => {
        this.getUsersFilterGroup();
      }, 400);
      this.showUsersGroups = true;
    } else {
      this.showUsersGroups = false;
    }
  }

  /**
   * Selects or deselects a user based on checkbox status.
   */
  selectUser(user: IUser): void {
    user.checked = !user.checked;
    //const userName = user.name;
    const userName = user.name.split('(')[0].trim();
    if (user.checked) {
      if (!this.selectedUsers.includes(userName)) {
        this.selectedUsers.push(userName);
      }
    } else {
      const index = this.selectedUsers.indexOf(userName);
      if (index !== -1) {
        this.selectedUsers.splice(index, 1);
      }
    }
    this.showSelected = this.selectedUsers.length > 0;
  }

  /**
   * Selects or deselects a user based on checkbox status.
   */
  selectUserGroup(user: IUser): void {
    user.checked = !user.checked;
    //const userName = user.name;
    const userName = user.name.split('(')[0].trim();
    if (user.checked) {
      if (!this.selectedUsersGroup.includes(userName)) {
        this.selectedUsersGroup.push(userName);
      }
    } else {
      const index = this.selectedUsersGroup.indexOf(userName);
      if (index !== -1) {
        this.selectedUsersGroup.splice(index, 1);
      }
    }
    this.showSelectedGroup = this.selectedUsersGroup.length > 0;
  }

  /**
   * Creates a new group with the selected users and saves it to the server.
   */
  saveGroup() {

    const modifiedSelectedUsers: string[] = [];
    for (const selectedUser of this.selectedUsers) {
      const userName = selectedUser.split('(')[0];
      modifiedSelectedUsers.push(userName.trim());
    }

    const groupBody = {
      groupName: this.groupsForm.value.groupName,
      members: modifiedSelectedUsers,
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
    };

    this.topicListService
      .createGroup(groupBody)
      .pipe(takeUntil(this.ngUnsubscribe))
      .subscribe({
        next: (response) => {
          this.dialogRef.close();
          this.messageService.showSuccessMessage(
            this.translate.instant('OK_MESSAGES.OK_CREATE_GROUP')
          );
        },
        error: (error) => {
          this.messageService.showErrorMessage(
            this.translate.instant('ERROR_MESSAGES.CREATE_GROUP_ERROR') +
              '\n' +
              error.error.message
          );
        },
      });
  }

  /**
   * Edit a group with the selected users and saves it to the server.
   */
  editGroup() {
    const modifiedSelectedUsers: string[] = [];
    for (const selectedUser of this.selectedUsersGroup) {
      const userName = selectedUser.split('(')[0];
      modifiedSelectedUsers.push(userName.trim());
    }

    const groupBody = {
      groupName: this.actualGroupName,
      members: modifiedSelectedUsers,
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
      id: this.idGroup,
    };

    this.topicListService
      .editGroup(groupBody)
      .pipe(takeUntil(this.ngUnsubscribe))
      .subscribe({
        next: (response) => {
          this.dialogRef.close();
          this.messageService.showSuccessMessage(
            this.translate.instant('OK_MESSAGES.OK_EDIT_GROUP')
          );
        },
        error: (error) => {
          this.messageService.showErrorMessage(
            this.translate.instant('ERROR_MESSAGES.ERROR_EDIT_GROUP') +
              '\n' +
              error.error.message
          );
        },
      });
  }

  /**
   * Clears the user selection and resets the form.
   */
  clearSelection() {
    this.oldSelectedOption = this.selectedOption;
    this.users.forEach((user) => (user.checked = false));
    this.groupsForm.reset();
    this.selectedUsers = [];
    this.showUsers = false;
    this.showSelected = false;
    this.users = [];
    this.groupsForm.patchValue({
      selectedOption: this.oldSelectedOption,
    });
  }

  /**
   * Sets a test user and token in cookies.
   */
  setCookie() {
    this.cookie.set('user', 'testUser');
    this.cookie.set('token', 'testToken');
  }

  /**
   * Fetches filtered user list from the server.
   */
  getUsersFilter() {
    const url = `${environment.apiUrl}/users/getUsersByMatch`;
    const loadTopicsBody = {
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
      matcher: this.matcher,
    };
    this.topicListService
      .postResponse(loadTopicsBody, url)
      .subscribe((response) => {
        if (response && response.body) {
          this.usersNames = response.body.entity;
          this.loadForm();
        }
      });
  }

  getUsersFilterGroup() {
    const url = `${environment.apiUrl}/users/getUsersByMatch`;
    const loadTopicsBody = {
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
      matcher: this.matcher,
    };
    this.topicListService
      .postResponse(loadTopicsBody, url)
      .subscribe((response) => {
        if (response && response.body) {
          this.isFilterUsersGroups = true;
          this.usersGroupsNames = response.body.entity;
          this.loadFormGroup();
        }
      });
  }

  // Update the selected option based on user input
  onOptionChange() {
    const selectedOptionControl = this.groupsForm.get('selectedOption');
    if (selectedOptionControl) {
      this.selectedOption = selectedOptionControl.value;
    }

    if (this.selectedOption == 'edit') {
      this.getGrupos();
    }
  }

  // Load users when called
  loadUsers() {
    this.getGroup();
  }

  // Retrieve users from the selected group
  getGroup() {
    const selectedGroupControl = this.groupsForm.get('selectedGroup');
    if (selectedGroupControl) {
      this.selectedGroup = selectedGroupControl.value;
    }
    const loadGroupBody = {
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
      groupName: this.selectedGroup,
    };
    this.topicsCreateService
      .getGroup(loadGroupBody)
      .pipe(takeUntil(this.ngUnsubscribe))
      .subscribe({
        next: (response) => {
          this.isFilterUsersGroups = false;
          this.usersGroupsNames = response.entity.members;
          this.idGroup = response.entity.id;
          this.actualGroupName = response.entity.groupName;
          this.loadFormGroup();
        },
        error: (error) => {
          this.messageService.showErrorMessage(
            this.translate.instant('ERROR_MESSAGES.ERROR_RETRIEVING_DATA_CB') +
              '\n' +
              error.error.message
          );
        },
      });
  }

  // Retrieve available groups
  getGrupos() {
    const loadGroupsBody = {
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
    };
    this.topicsCreateService
      .getGroupsByUser(loadGroupsBody)
      .pipe(takeUntil(this.ngUnsubscribe))
      .subscribe({
        next: (response) => {
          this.groups = response.entity;
        },
        error: (error) => {
          this.messageService.showErrorMessage(
            this.translate.instant('ERROR_MESSAGES.ERROR_RETRIEVING_DATA_CB') +
              '\n' +
              error.error.message
          );
        },
      });
  }

  deselectUserGroup(user: string): void {
    const index = this.selectedUsersGroup.indexOf(user);
    if (index !== -1) {
      this.selectedUsersGroup.splice(index, 1);
    }
    this.showSelectedGroup = this.selectedUsersGroup.length > 0;
    this.changeDetectorRef.detectChanges();
    this.filterUsersGroup();
  }

  deselectUser(user: string): void {
    const index = this.selectedUsers.indexOf(user);
    if (index !== -1) {
      this.selectedUsers.splice(index, 1);
    }
    this.showSelected = this.selectedUsers.length > 0;
    this.changeDetectorRef.detectChanges();
    this.filterUsers();
  }

  isChipRemovable(user: string): boolean {
    return true;
  }
}
