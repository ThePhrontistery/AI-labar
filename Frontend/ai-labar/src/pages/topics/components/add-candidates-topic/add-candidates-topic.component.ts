/**
 * This component allows users to add candidates to a topic. It provides functionality to select candidates from groups or individual users.
 */
import {
  Component,
  OnInit,
  Inject,
  ViewChild,
  ElementRef,
  OnDestroy,
} from '@angular/core';
import { TopicsCreateService } from '../topics-create/topics-create.service';
import { CookieService } from 'ngx-cookie-service';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { AddGroupsTopicComponent } from '../add-groups-topic/add-groups-topic.component';
import { IUser } from '../interfaces/emoji.model';
import { FormBuilder, FormControl, FormGroup } from '@angular/forms';
import { TopicsListService } from '../topics-list/topics-list.service';
import { environment } from 'src/environments/environment';
import { TranslateService } from '@ngx-translate/core';
import { Subject, takeUntil } from 'rxjs';
import { MessageService } from '../../services/message.service';

@Component({
  selector: 'app-add-candidates-topic',
  templateUrl: './add-candidates-topic.component.html',
  styleUrls: ['./add-candidates-topic.component.scss'],
})
export class AddCandidatesTopicComponent implements OnInit, OnDestroy {
  // Selected option: 'group' or 'individual'
  selectedOption: string = 'group';

  // Currently selected group
  selectedGroup: string | undefined;

  // List of available groups
  groups: string[] = [];

  // List of groups the users belong to
  usersGroups: string[] = [];

  // Flag for filtering users
  filtering = true;

  // Array of user names
  usersNames: Array<string> = [];

  // Array of user objects
  users: Array<IUser> = [];

  // Filtered list of users
  filteredUsers: IUser[] = [];

  // Selected users for the topic
  selectedUsers: string[] = [];

  // Form group for managing users and groups
  groupsForm: FormGroup;

  // Maximum number of candidates allowed
  limitCandidates: number = 8;

  // Flag to show user list
  showUsers: boolean = false;

  // User input for filtering
  matcher: string = '';

  // Flag to show selected users
  showSelected: boolean = false;

  // Reference to the checkbox container
  @ViewChild('checkboxContainer')
  checkboxContainer!: ElementRef;

  // Previously selected option
  oldSelectedOption: string | undefined;

  private ngUnsubscribe = new Subject();
  private searchTimer: any;

  constructor(
    private topicsCreateService: TopicsCreateService,
    private cookie: CookieService,
    private fb: FormBuilder,
    private topicListService: TopicsListService,
    public dialogRef: MatDialogRef<AddGroupsTopicComponent>,
    private translate: TranslateService,
    private messageService: MessageService,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {
    // Initialize the form group and controls
    this.groupsForm = this.fb.group({
      searcher: [''],
      selectedGroup: '',
      selectedOption: ['group'], // Initial value for selectedOption
      currentSelection: [''],
    });
  }
  ngOnDestroy(): void {
    this.ngUnsubscribe.complete();
  }

  ngOnInit(): void {
    // Load groups and users on component initialization
    this.getGrupos();
    //this.getUsers();
  }

  // Load users when called
  loadUsers() {
    this.getGroup();
  }

  // Function to close the dialog
  closeDialog() {
    const datosCancelar = {
      selectedGroup: '',
      selectedUsers: [],
    };
    this.dialogRef.close(datosCancelar);
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
          this.usersGroups = response.entity.members;
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

  // Save the selected group and users to the dialog data and close the dialog
  saveSelection() {
    const data = {
      selectedGroup: this.selectedGroup,
      selectedUsers: this.usersGroups,
    };
    this.dialogRef.close(data);
  }

  // Retrieve all users
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

  // Initialize the user selection form
  loadForm() {
    this.users = [];
    this.usersNames.map((item) => {
      let user = {
        name: item,
        checked: false,
        hidden: false,
        modal: false
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

  // Filter users based on search input
  filterUsers(): void {
    this.filtering = true;
    this.users = [];
    let search = this.groupsForm.value.searcher.toUpperCase();
    if (this.searchTimer) {
      clearTimeout(this.searchTimer);
    }
    if (search.length >= 3) {
      this.matcher = search;
      //this.getUsersFilter();
      this.searchTimer = setTimeout(() => {
        this.getUsersFilter();
      }, 400);
      this.showUsers = true;
    } else {
      this.showUsers = false;
    }
  }

  // Select or deselect a user
  selectUser(user: IUser): void {
    user.checked = !user.checked;
    //const userName = user.name;
    const userName = user.name.split('(')[0];
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

  // Save the selected users to the dialog data and close the dialog
  saveGroup() {
    const data = {
      selectedGroup: null,
      selectedUsers: this.selectedUsers,
    };
    this.dialogRef.close(data);
  }

  // Clear all selections and reset the form
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

  // Update the selected option based on user input
  onOptionChange() {
    const selectedOptionControl = this.groupsForm.get('selectedOption');
    if (selectedOptionControl) {
      this.selectedOption = selectedOptionControl.value;
    }
  }

  // Get users filtered by the matcher
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
