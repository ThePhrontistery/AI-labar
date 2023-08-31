import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { CookieService } from 'ngx-cookie-service';
import { MatDialog } from '@angular/material/dialog';
import { GroupsComponent } from '../groups/groups.component';
import { TranslateService } from '@ngx-translate/core';

/**
 * Component for the management of topics and groups.
 */
@Component({
  selector: 'app-topics',
  templateUrl: './topics.component.html',
  styleUrls: ['./topics.component.scss'],
})
export class TopicsComponent implements OnInit {
  /**
   * Current user obtained from cookies.
   */
  user = this.cookie.get('user');

  /**
   * Constructor of the component.
   * @param cookie Service for working with cookies.
   * @param router Service for navigation between routes.
   * @param dialog Service for displaying modal dialogs.
   * @param translate Service for text internationalization.
   */
  constructor(
    private cookie: CookieService,
    private router: Router,
    private dialog: MatDialog,
    private translate: TranslateService
  ) {}

  /**
   * Method invoked upon initializing the component.
   * Verify if the user is authenticated and set the default language.
   */
  ngOnInit(): void {
    if (this.cookie.get('user') === '' || this.cookie.get('token') === '') {
      this.router.navigate(['login']);
    }
    this.translate.setDefaultLang('en');
  }

  /**
   * Log out the user.
   * Delete the cookies and redirect to the login page.
   */
  logOut() {
    this.cookie.delete('user');
    this.cookie.delete('token');
    this.router.navigate(['login']);
  }

  /**
   * Navigate to the page to add a new topic.
   */
  addTopic() {
    this.router.navigate(['/topics/topics-create']);
  }

  /**
   * Navigate back to the list of topics.
   */
  back() {
    this.router.navigate(['/topics/topics-list']);
  }

  /**
   * Open a dialog to add a new group.
   */
  addGroup() {
    const dialogRef = this.dialog.open(GroupsComponent, {
      width: '750px',
      data: {},
    });
  }
}
