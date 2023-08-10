import { ComponentFixture, TestBed } from '@angular/core/testing';
import { RouterTestingModule } from '@angular/router/testing';
import { Router } from '@angular/router';
import { MatDialog, MatDialogModule, MatDialogRef } from '@angular/material/dialog';
import { MatMenuModule } from '@angular/material/menu';
import { CookieService } from 'ngx-cookie-service';
import { TopicsComponent } from './topics.component';
import { GroupsComponent } from '../groups/groups.component';

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
      ],
      providers: [
        CookieService,
        MatDialog,
      ],
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

  it('should open dialog on anadirGrupo', () => {
    spyOn(dialog, 'open');
    component.anadirGrupo();
    expect(dialog.open).toHaveBeenCalled();
  });

  it('should have navigated to login if user is missing', () => {
    spyOn(router, 'navigateByUrl');
    spyOn(component['cookie'], 'delete');
    component.ngOnInit();
    expect(router.navigateByUrl).toHaveBeenCalled();
  });
});
