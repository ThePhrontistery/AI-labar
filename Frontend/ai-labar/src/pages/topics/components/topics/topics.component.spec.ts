import { ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { Router } from '@angular/router';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatMenuModule } from '@angular/material/menu';
import { CookieService } from 'ngx-cookie-service';
import { TopicsComponent } from './topics.component';
import { MatIconModule } from '@angular/material/icon';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { MatSnackBarModule } from '@angular/material/snack-bar';

describe('TopicsComponent', () => {
  let component: TopicsComponent;
  let fixture: ComponentFixture<TopicsComponent>;
  let dialog: MatDialog;
  let router: Router;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [TopicsComponent],
      imports: [
        RouterTestingModule.withRoutes([]),
        MatDialogModule,
        MatMenuModule,
        MatIconModule,
        HttpClientTestingModule,
        MatSnackBarModule,
        TranslateModule.forRoot(),
      ],
      providers: [TranslateService, CookieService, MatDialog],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(TopicsComponent);
    component = fixture.componentInstance;
    dialog = TestBed.inject(MatDialog);
    router = TestBed.inject(Router);
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should navigate to login if user or token are missing', () => {
    spyOn(router, 'navigateByUrl');
    spyOn(component['cookie'], 'delete');
    component.ngOnInit();
    expect(router.navigateByUrl).toHaveBeenCalled();
  });

  it('should navigate to login on logOut', () => {
    spyOn(router, 'navigateByUrl');
    component.logOut();
    expect(router.navigateByUrl).toHaveBeenCalled();
  });
});
