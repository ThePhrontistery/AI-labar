/**
 * Component to add groups to a topic.
 */
import { Component, OnInit, Inject } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialog, MatDialogRef } from '@angular/material/dialog';
import { TopicsCreateService } from '../topics-create/topics-create.service';
import { CookieService } from 'ngx-cookie-service';
import { GroupsComponent } from '../groups/groups.component';

@Component({
  selector: 'app-anyadir-grupos-topic',
  templateUrl: './anyadir-grupos-topic.component.html',
  styleUrls: ['./anyadir-grupos-topic.component.scss'],
})
export class AnyadirGruposTopicComponent implements OnInit {
  selectedGroup: string | undefined;
  groups: string[] = [];
  users: string[] = [];

  /**
   * Component builder.
   * @param topicsCreateService - Service to create topics.
   * @param cookie - Service to manage cookies.
   * @param dialog - Service to open modal dialogues.
   * @param dialogRef - Reference to the current dialogue.
   * @param data - Data injected into the dialog.
   */
  constructor(
    private topicsCreateService: TopicsCreateService,
    private cookie: CookieService,
    private dialog: MatDialog,
    public dialogRef: MatDialogRef<AnyadirGruposTopicComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  /**
   * It is executed when the component is initialized.
   * Gets the list of available groups.
   */
  ngOnInit(): void {
    this.getGroups();
  }

  /**
   * Loads the users of the selected group.
   */
  loadUsers() {
    this.getGroup();
  }

  /**
   * Closes the current dialog and emits an empty object.
   */
  closeDialog() {
    const dataCancel = {
      selectedGroup: '',
      selectedUsers: [],
    };
    this.dialogRef.close(dataCancel);
  }

  /**
   * Gets the list of available groups.
   */
  getGroups() {
    const loadGroupsBody = {
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
    };
    this.topicsCreateService.getGroupsByUser(loadGroupsBody).subscribe(
      (data) => {
        this.groups = data.entity;
      },
      (error) => {
        console.log('Error al obtener los datos del combo box:', error);
      }
    );
  }

  /**
   * Gets the users of a selected group.
   */
  getGroup() {
    const loadGroupBody = {
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
      groupName: this.selectedGroup,
    };
    this.topicsCreateService.getGroup(loadGroupBody).subscribe(
      (data) => {
        this.users = data.entity.members;
      },
      (error) => {
        console.log('Error al obtener los datos del combo box:', error);
      }
    );
  }

  /**
   * Save the current selection and close the dialog.
   */
  saveSelection() {
    const data = {
      selectedGroup: this.selectedGroup,
      selectedUsers: this.users,
    };
    this.dialogRef.close(data);
  }

  /**
   * Opens a dialog to add a new group.
   * Updates the group list after closing the dialog.
   */
  addGroup() {
    const dialogRef = this.dialog.open(GroupsComponent, {
      width: '750px',
      data: {},
    });

    dialogRef.afterClosed().subscribe((result) => {
      this.getGroups();
    });
  }
}
