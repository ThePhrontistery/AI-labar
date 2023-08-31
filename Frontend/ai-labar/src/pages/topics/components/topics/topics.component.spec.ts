import { ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { Router } from '@angular/router';
import { MatDialog, MatDialogModule } from '@angular/material/dialog';
import { MatMenuModule } from '@angular/material/menu';
import { CookieService } from 'ngx-cookie-service';
import { TopicsComponent } from './topics.component';
import { MatIconModule } from '@angular/material/icon';

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
      ],
      providers: [CookieService, MatDialog],
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

  it('should open dialog on addGroup', () => {
    spyOn(dialog, 'open');
    component.addGroup();
    expect(dialog.open).toHaveBeenCalled();
  });
});
