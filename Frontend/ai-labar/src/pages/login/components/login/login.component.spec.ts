import { ComponentFixture, TestBed } from '@angular/core/testing';
import { Router } from '@angular/router';
import { FormBuilder } from '@angular/forms';
import { CookieService } from 'ngx-cookie-service';
import { LoginService } from './../../login.service';
import { LoginComponent } from './login.component';
import { HttpClientModule } from '@angular/common/http';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';

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
      providers: [TranslateService,
        LoginService,
        {
          provide: Router,
          useValue: { navigate: jasmine.createSpy('navigate') },
        },
        CookieService,
        FormBuilder,
      ],
      imports: [HttpClientModule,HttpClientTestingModule, TranslateModule.forRoot()],
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

  it('should create', () => {
    expect(LoginComponent).toBeTruthy();
  });
});
