import { Component, OnInit, Inject, ChangeDetectorRef, ViewChild, ElementRef } from '@angular/core';
import { TopicsCreateService } from '../topics-create/topics-create.service';
import { CookieService } from 'ngx-cookie-service';
import { MAT_DIALOG_DATA, MatDialog, MatDialogRef } from '@angular/material/dialog';
import { AnyadirGruposTopicComponent } from '../anyadir-grupos-topic/anyadir-grupos-topic.component';
import { IUser } from '../interfaces/emoji.model';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { TopicsListService } from '../topics-list/topics-list.service';
import { environment } from 'src/environments/environment';

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
  groupsForm: FormGroup;

  limiteCandidatos: number = 8;

  mostrarUsuarios: boolean = false;
  matcher: string = "";
  mostrarSelected: boolean = false;

  @ViewChild('checkboxContainer')
  checkboxContainer!: ElementRef;

  oldSelectedOption: string | undefined;

  constructor(private topicsCreateService: TopicsCreateService,
    private cookie: CookieService,
    private fb: FormBuilder,
    private topicListService: TopicsListService,
    private changeDetectorRef: ChangeDetectorRef,
    public dialogRef: MatDialogRef<AnyadirGruposTopicComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any) {

    // Initialize the form group and controls
    this.groupsForm = this.fb.group({
     searcher: [''],
     selectedGroup: '',
     selectedOption: ['group'], // Valor inicial para selectedOption
     currentSelection:['']
    });
    }

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

  getGroup() {
    const selectedGroupControl = this.groupsForm.get("selectedGroup");
    if (selectedGroupControl) {
      this.selectedGroup = selectedGroupControl.value;
    }
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
    this.users = [];
    let search = this.groupsForm.value.searcher.toLowerCase();
    if(search.length >= 3){
      this.matcher = search;
      this.getUsersFilter();
      this.mostrarUsuarios = true;
    } else {
      this.mostrarUsuarios = false;
    }
  }

  selectUser(user: IUser): void {
    user.checked = !user.checked;
    if (user.checked) {
      if(!this.selectedUsers.includes(user.name)){
        this.selectedUsers.push(user.name);
      }
    } else {
      const index = this.selectedUsers.indexOf(user.name);
      if (index !== -1) {
        this.selectedUsers.splice(index, 1);
      }
    }
    if(this.selectedUsers.length > 0){
      this.mostrarSelected = true;
    } else {
      this.mostrarSelected = false;
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
    this.oldSelectedOption = this.selectedOption;
    this.users.forEach(user => user.checked = false);
    this.groupsForm.reset();
    this.selectedUsers = [];
    this.mostrarUsuarios = false;
    this.mostrarSelected = false;
    this.users = [];
this.groupsForm.patchValue({
  selectedOption: this.oldSelectedOption
});
  }

  onOptionChange() {
    const selectedOptionControl = this.groupsForm.get("selectedOption");
    if (selectedOptionControl) {
      this.selectedOption = selectedOptionControl.value;
    }
  }

  getUsersFilter(){
    const url = `${environment.apiUrl}/users/getUsersByMatch`;
    const loadTopicsBody = {
      "user": this.cookie.get("user"),
      "token": this.cookie.get("token"),
      "matcher": this.matcher
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

}
