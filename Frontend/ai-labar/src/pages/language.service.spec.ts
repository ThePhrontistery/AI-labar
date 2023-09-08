import { TestBed, inject } from '@angular/core/testing';

import { LanguageService } from './language.service';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('LanguageService', () => {
  let service: LanguageService;

  let languageService: LanguageService;
  let translateService: TranslateService;
  
  beforeEach(() => {
    TestBed.configureTestingModule({
      declarations: [ ],
      providers: [TranslateService],
      imports: [HttpClientTestingModule, TranslateModule.forRoot()]});
    service = TestBed.inject(LanguageService);
  });

  beforeEach(inject(
    [LanguageService, TranslateService],
    (service: LanguageService, translate: TranslateService) => {
      languageService = service;
      translateService = translate;
    }
  ));

  it('should be created', () => {
    expect(service).toBeTruthy();
  });

  it('should have a default language of "EN"', () => {
    expect(languageService.getLanguage()).toBe('EN');
  });

  it('should be able to set and get the current language', () => {
    languageService.setLanguage('ES');
    expect(languageService.getLanguage()).toBe('ES');
  });

  it('should toggle between "EN" and "ES" languages', () => {
    languageService.toggleLanguage();
    expect(languageService.getLanguage()).toBe('ES');
    languageService.toggleLanguage();
    expect(languageService.getLanguage()).toBe('EN');
  });

  it('should return the default language as "EN"', () => {
    expect(languageService.getDefaultLanguage()).toBe('EN');
  });

  it('should change the TranslateService language when toggling', () => {
    spyOn(translateService, 'use');
    languageService.toggleLanguage();
    expect(translateService.use).toHaveBeenCalledWith('es');
    languageService.toggleLanguage();
    expect(translateService.use).toHaveBeenCalledWith('en');
  });
});
