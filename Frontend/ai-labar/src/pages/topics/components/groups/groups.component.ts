import { Component, OnInit, Inject } from '@angular/core';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { IUser } from '../interfaces/emoji.model';
import { CookieService } from 'ngx-cookie-service';
import { TopicsListService } from '../topics-list/topics-list.service';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { environment } from 'src/environments/environment';

@Component({
  selector: 'app-groups',
  templateUrl: './groups.component.html',
  styleUrls: ['./groups.component.scss']
})
export class GroupsComponent implements OnInit {
  filtering = true;
  usersNames: Array<string> = [];
  users: Array<IUser> = [];
  filteredUsers: IUser[] = [];
  selectedUsers: string[] = [];
  groupsForm: FormGroup =  this.fb.group({
    groupName: ['', Validators.required],
    searcher: [''],
    currentSelection:['']
  });
  constructor(private fb: FormBuilder,
    private cookie: CookieService,
    private topicListService: TopicsListService,
    public dialogRef: MatDialogRef<GroupsComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any) { }

  ngOnInit(): void {
    this.getUsers();
  }
  getUsers(){
    const url = `${environment.apiUrl}/users/getAllUsers`;
    const loadTopicsBody = {
      "user": this.cookie.get("user"),
      "token": this.cookie.get("token")
  }
    this.topicListService.postResponse(loadTopicsBody, url).subscribe(
        response => {
          if (response && response.body){
            this.usersNames = response.body.entity;
            this.loadForm();
          }
        }
    );
  }
  loadForm(){
    this.usersNames.map(item => {
        let user = {
          name: item,
          checked: false,
          hidden: false
        }
        this.users.push(user);
      });
    this.users.forEach(user => {
      this.groupsForm.addControl(user.name, new FormControl());
    });
    this.users.forEach(user => {
      this.groupsForm.controls[user.name].statusChanges.subscribe(() => {
        if (user) this.selectUser(user);
      })
    });
  }

  filterUsers(): void {
    this.filtering = true;
    let search = this.groupsForm.value.searcher.toLowerCase();
    this.filteredUsers = [];
    this.users.map(user => user.hidden = false);
    this.filteredUsers = this.users
      .filter(user => user.name.toLowerCase().includes(search))
      .map(user => ({ name: user.name, checked: false , hidden: false}));
      this.users.forEach(user => {
        if (!this.filteredUsers.find(filteredUser => filteredUser.name === user.name)) {
          user.hidden = true;
        }
      });
      this.filtering = false;
  }

  selectUser(user: IUser): void {
    user.checked = !user.checked;
    if (user.checked) {
      this.selectedUsers.push(user.name);
    } else {
      const index = this.selectedUsers.indexOf(user.name);
      if (index !== -1) {
        this.selectedUsers.splice(index, 1);
      }
    }
  }
  saveGroup(){
    const url = `${environment.apiUrl}/groups/createGroup`;
    const groupBody =  {
      "groupName": this.groupsForm.value.groupName,
      "members": this.selectedUsers,
      "user": this.cookie.get("user"),
      "token": this.cookie.get("token")
    }
    this.topicListService.post(groupBody, url).subscribe(
        response => {
          if (response){
            console.log(response);
          }
        }
    );
      this.dialogRef.close();
  }
  clearSelection(){
    this.users.forEach(user => user.checked = false);
    this.selectedUsers = [];
  }

  //solo para los test
  setCookie(){
    this.cookie.set('user', 'testUser');
    this.cookie.set('token', 'testToken');
  }
}
