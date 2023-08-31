import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { CookieService } from 'ngx-cookie-service';
import { MatDialog } from '@angular/material/dialog';
import { GroupsComponent } from '../groups/groups.component';
import { TranslateService } from '@ngx-translate/core';
@Component({
  selector: 'app-topics',
  templateUrl: './topics.component.html',
  styleUrls: ['./topics.component.scss']
})
export class TopicsComponent implements OnInit {
  user = this.cookie.get('user');

  constructor(private cookie: CookieService,
              private router: Router,
              private dialog: MatDialog,
              private translate: TranslateService) { }

  ngOnInit(): void {
    if (this.cookie.get('user') === '' || this.cookie.get('token') === '') this.router.navigate(['login'])
    this.translate.setDefaultLang('en');
  }
  logOut(){
    this.cookie.delete('user');
    this.cookie.delete('token');
    this.router.navigate(['login']);
  }

  anyadirTopic() {
    this.router.navigate(['/topics/topics-create']);
  }
  back(){
    this.router.navigate(['/topics/topics-list']);
  }
  anadirGrupo(){
    const dialogRef = this.dialog.open(GroupsComponent, {
      width: '750px',
      data: {},
    });
  }

}
