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

  // FormGroup for the group creation form
  groupsForm: FormGroup = this.fb.group({
    groupName: ['', Validators.required],
    searcher: [''],
    currentSelection: [''],
  });

  // Whether to show the list of users
  showUsers: boolean = false;

  // Search text matcher for user filtering
  matcher: string = '';

  // Whether to show the selected users
  showSelected: boolean = false;

  // Reference to the checkbox container element
  @ViewChild('checkboxContainer')
  checkboxContainer!: ElementRef;

  private ngUnsubscribe = new Subject();

  constructor(
    private fb: FormBuilder,
    private cookie: CookieService,
    private topicListService: TopicsListService,
    public dialogRef: MatDialogRef<GroupsComponent>,
    private translate: TranslateService,
    private messageService: MessageService,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  ngOnDestroy(): void {
    this.ngUnsubscribe.complete();
  }

  ngOnInit(): void {
    // Initialize the component
    this.getUsers();
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
  loadForm() {
    this.usersNames.map((item) => {
      let user = {
        name: item,
        checked: this.selectedUsers.includes(item),
        hidden: false,
      };
      this.users.push(user);
    });
    this.users.forEach((user) => {
      this.groupsForm.addControl(user.name, new FormControl());
    });
    this.users.forEach((user) => {
      this.groupsForm.controls[user.name].statusChanges.subscribe(() => {
        if (user) this.selectUser(user);
      });
    });
  }

  /**
   * Filters the list of users based on the search criteria.
   */
  filterUsers(): void {
    this.filtering = true;
    this.users = [];
    let search = this.groupsForm.value.searcher.toLowerCase();
    if (search.length >= 3) {
      this.matcher = search;
      this.getUsersFilter();
      this.showUsers = true;
    } else {
      this.showUsers = false;
    }
  }

  /**
   * Selects or deselects a user based on checkbox status.
   */
  selectUser(user: IUser): void {
    user.checked = !user.checked;
    if (user.checked) {
      if (!this.selectedUsers.includes(user.name)) {
        this.selectedUsers.push(user.name);
      }
    } else {
      const index = this.selectedUsers.indexOf(user.name);
      if (index !== -1) {
        this.selectedUsers.splice(index, 1);
      }
    }
    this.showSelected = this.selectedUsers.length > 0;
  }

  /**
   * Creates a new group with the selected users and saves it to the server.
   */
  saveGroup() {
    const url = `${environment.apiUrl}/groups/createGroup`;
    const groupBody = {
      groupName: this.groupsForm.value.groupName,
      members: this.selectedUsers,
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
    };

    this.topicListService
      .post(groupBody, url)
      .pipe(takeUntil(this.ngUnsubscribe))
      .subscribe({
        next: (response) => {
          if (response) {
            console.log(response);
          }
          this.dialogRef.close();
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
   * Clears the user selection and resets the form.
   */
  clearSelection() {
    this.users.forEach((user) => (user.checked = false));
    this.groupsForm.reset();
    this.selectedUsers = [];
    this.showUsers = false;
    this.showSelected = false;
    this.users = [];
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
}
