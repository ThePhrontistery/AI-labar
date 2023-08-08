import { Component, OnInit, Inject, ChangeDetectorRef } from '@angular/core';
import { TopicsCreateService } from '../topics-create/topics-create.service';
import { CookieService } from 'ngx-cookie-service';
import { MAT_DIALOG_DATA, MatDialog, MatDialogRef } from '@angular/material/dialog';
import { AnyadirGruposTopicComponent } from '../anyadir-grupos-topic/anyadir-grupos-topic.component';
import { IUser } from '../interfaces/emoji.model';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { TopicsListService } from '../topics-list/topics-list.service';

@Component({
  selector: 'app-anyadir-candidatos-topic',
  templateUrl: './anyadir-candidatos-topic.component.html',
  styleUrls: ['./anyadir-candidatos-topic.component.scss']
})
export class AnyadirCandidatosTopicComponent implements OnInit {

  selectedOption: string = "group";
  selectedGroup: string | undefined;
  groups: string[] = [];
  usersGroups: string[] = [];

  filtering = true;
  usersNames: Array<string> = [];
  users: Array<IUser> = [];
  filteredUsers: IUser[] = [];
  selectedUsers: string[] = [];
  groupsForm: FormGroup =  this.fb.group({
    searcher: [''],
    currentSelection:['']
  });

  limiteCandidatos: number = 8;

  constructor(private topicsCreateService: TopicsCreateService,
    private cookie: CookieService,
    private fb: FormBuilder,
    private topicListService: TopicsListService,
    private changeDetectorRef: ChangeDetectorRef,
    public dialogRef: MatDialogRef<AnyadirGruposTopicComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any) { }

  ngOnInit(): void {
    this.getGrupos();
    this.getUsers();
  }

  loadUsers() {
    this.getGroup();
  }

  // Función para cerrar el diálogo emergente
  closeDialog() {
    const datosCancelar = {
      grupoSeleccionado: "",
      usuariosSeleccionados: []
    };
    this.dialogRef.close(datosCancelar);
  }

  getGrupos() {
    const loadGroupsBody = {
      "user": this.cookie.get("user"),
      "token": this.cookie.get("token")
  }
    this.topicsCreateService.getGroupsByUser(loadGroupsBody).subscribe(
      data => {
        this.groups = data.entity;
      },
      error => {
        console.log('Error al obtener los datos del combo box:', error);
      }
    );
  }

  getGroup(){
    const loadGroupBody = {
      "user": this.cookie.get("user"),
      "token": this.cookie.get("token"),
      "groupName": this.selectedGroup
  }
    this.topicsCreateService.getGroup(loadGroupBody).subscribe(
      data => {
        this.usersGroups = data.entity.members;
      },
      error => {
        console.log('Error al obtener los datos del combo box:', error);
      }
    );
  }

  saveSelection(){
    const datos = {
      grupoSeleccionado: this.selectedGroup,
      usuariosSeleccionados: this.usersGroups
    };
    this.dialogRef.close(datos);
  }

  getUsers(){
    const url = 'http://localhost:8080/users/getAllUsers';
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
      const datos = {
        grupoSeleccionado: null,
        usuariosSeleccionados: this.selectedUsers
      };
      this.dialogRef.close(datos);
  }

  clearSelection(){
    this.users.forEach(user => user.checked = false);
    this.selectedUsers = [];
  }

}
