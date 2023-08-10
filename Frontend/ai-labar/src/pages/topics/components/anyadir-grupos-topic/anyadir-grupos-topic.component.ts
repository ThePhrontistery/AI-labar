import { Component, OnInit, Inject, Input } from '@angular/core';
import { MAT_DIALOG_DATA, MatDialog, MatDialogRef } from '@angular/material/dialog';
import { TopicsCreateService } from '../topics-create/topics-create.service';
import { CookieService } from 'ngx-cookie-service';
import { GroupsComponent } from '../groups/groups.component';

@Component({
  selector: 'app-anyadir-grupos-topic',
  templateUrl: './anyadir-grupos-topic.component.html',
  styleUrls: ['./anyadir-grupos-topic.component.scss']
})
export class AnyadirGruposTopicComponent implements OnInit {

  selectedGroup: string | undefined;
  groups: string[] = [];
  users: string[] = [];

  constructor(
    private topicsCreateService: TopicsCreateService,
    private cookie: CookieService,
    private dialog: MatDialog,
    public dialogRef: MatDialogRef<AnyadirGruposTopicComponent>,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) { }

  ngOnInit(): void {
    this.getGrupos();
  }

  // Función para cargar los usuarios del grupo seleccionado
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
        this.users = data.entity.members;
      },
      error => {
        console.log('Error al obtener los datos del combo box:', error);
      }
    );
  }

  saveSelection(){
    const datos = {
      grupoSeleccionado: this.selectedGroup,
      usuariosSeleccionados: this.users
    };
    this.dialogRef.close(datos);
  }

  anadirGrupo(){
    const dialogRef = this.dialog.open(GroupsComponent, {
      width: '750px',
      data: {},
    });

    dialogRef.afterClosed().subscribe(result => {
      this.getGrupos();
    });
  }

}
