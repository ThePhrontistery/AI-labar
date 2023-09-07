import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { FormBuilder } from '@angular/forms';
import { CookieService } from 'ngx-cookie-service';
import { LoginComponent } from './login.component';
import { HttpClientModule } from '@angular/common/http';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';
import { MatSnackBarModule } from '@angular/material/snack-bar';
import { MatDialogModule } from '@angular/material/dialog';
import { LoginService } from '../../login.service';
import { catchError, of, throwError } from 'rxjs';
import { MessageService } from 'src/pages/topics/services/message.service';


describe('LoginComponent', () => {
  let component: LoginComponent;
  let fixture: ComponentFixture<LoginComponent>;
  let loginService: LoginService;
  let router: Router;
  let cookie: CookieService;
  let fb: FormBuilder;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [LoginComponent],
      providers: [
        TranslateService,
        LoginService,
        {
          provide: Router,
          useValue: { navigate: jasmine.createSpy('navigate') },
        },
        CookieService,
        FormBuilder,
        {
          provide: MessageService,
          useValue: {
            showErrorMessage: jasmine.createSpy('showErrorMessage'),
          }
        }
      ],
      imports: [
        HttpClientModule,
        HttpClientTestingModule,
        MatSnackBarModule,
        MatDialogModule,
        TranslateModule.forRoot(),
      ],
    }).compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(LoginComponent);
    component = fixture.componentInstance;
    loginService = TestBed.inject(LoginService);
    router = TestBed.inject(Router);
    cookie = TestBed.inject(CookieService);
    fb = TestBed.inject(FormBuilder);
  });

  it('should create LoginComponent', () => {
    expect(component).toBeTruthy();
  });

  it('should initialize the form groups', () => {
    expect(component.form).toBeDefined();
    expect(component.loginForm).toBeDefined();
  });

  it('should authenticate user on loginClick', () => {
    // Mock the loginService.login method and set up a mock response
    spyOn(loginService, 'login').and.returnValue(
      of({ body: { entity: ['tokenValue', 'visualizationValue'] } }).pipe()
    );

    // Set form values
    component.loginForm.setValue({ user: 'testuser', password: 'testpassword' });

    // Call the loginClick method
    component.loginClick();

    // Expectations
    expect(loginService.login).toHaveBeenCalledWith({
      user: 'testuser',
      password: '9f735e0df9a1ddc702bf0a1a7b83033f9f7153a00c29de82cedadc9957289b05',
    });
    expect(router.navigate).toHaveBeenCalledWith(['/topics/topics-list']);
  });

  it('should show error message on login failure', () => {
    // Mock the loginService.login method and set up a mock error response
    const err = new Error();
    err.message = 'Error message';
    let err2!: any;
    err2 = { 'error': err.message };
    spyOn(loginService, 'login').and.returnValue(
      throwError(() => err2)
    );

    // Set form values
    component.loginForm.setValue({ user: 'testuser', password: 'testpassword' });

    // Call the loginClick method
    component.loginClick();

    // Expectations
    expect(component.usernameError).toBe(''); // Ensure usernameError is cleared
    expect(loginService.login).toHaveBeenCalledWith({
      user: 'testuser',
      password: '9f735e0df9a1ddc702bf0a1a7b83033f9f7153a00c29de82cedadc9957289b05',
    });
    expect(router.navigate).not.toHaveBeenCalled(); // No navigation on error
    expect(TestBed.inject(MessageService).showErrorMessage).toHaveBeenCalled();
  });



  // Add more test cases for other scenarios as needed...
});
