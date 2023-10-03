import { ComponentFixture, TestBed } from '@angular/core/testing';
import { MatDialogModule, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { CookieService } from 'ngx-cookie-service';
import { AsResultsComponent } from './as-results.component';
import { VotingResultsService } from '../voting-results/voting-results.service';
import { TopicsListService } from '../topics-list/topics-list.service';
import { of } from 'rxjs';
import { TranslateModule, TranslateService } from '@ngx-translate/core';
import { HttpClientTestingModule } from '@angular/common/http/testing';

describe('AsResultsComponent', () => {
  let component: AsResultsComponent;
  let fixture: ComponentFixture<AsResultsComponent>;
  let resultsServiceSpy: jasmine.SpyObj<VotingResultsService>;
  let topicsListServiceSpy: jasmine.SpyObj<TopicsListService>;
  let cookieServiceSpy: jasmine.SpyObj<CookieService>;

  const votation = { id: 1, title: 'Test Votación' };
  const result = [{ image: 'image1.jpg', option: 'Option 1' }, { image: 'image2.jpg', option: 'Option 2' }];

  beforeEach(async () => {
    const resultsService = jasmine.createSpyObj('VotingResultsService', ['getWinnerOption']);
    const topicsListService = jasmine.createSpyObj('TopicsListService', ['votingResults']);
    const cookieService = jasmine.createSpyObj('CookieService', ['get']);

    await TestBed.configureTestingModule({
      declarations: [ AsResultsComponent ],
      providers: [TranslateService,
        { provide: VotingResultsService, useValue: resultsService },
        { provide: TopicsListService, useValue: topicsListService },
        { provide: CookieService, useValue: cookieService },
        { provide: MatDialogRef, useValue: {} },
        { provide: MAT_DIALOG_DATA, useValue: { votation } }
      ],
      imports: [MatDialogModule,HttpClientTestingModule, TranslateModule.forRoot()]
    })
    .compileComponents();

    resultsServiceSpy = TestBed.inject(VotingResultsService) as jasmine.SpyObj<VotingResultsService>;
    topicsListServiceSpy = TestBed.inject(TopicsListService) as jasmine.SpyObj<TopicsListService>;
    cookieServiceSpy = TestBed.inject(CookieService) as jasmine.SpyObj<CookieService>;

    resultsServiceSpy.getWinnerOption.and.returnValue(result);
    topicsListServiceSpy.votingResults.and.returnValue(of({ entity: result }));
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AsResultsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });

  it('should display votation title', () => {
    const title = fixture.nativeElement.querySelector('h1').textContent;
    expect(title).toEqual(votation.title);
  });

  it('should load results on init', () => {
    expect(component.result.length).toEqual(result.length);
  });

  // it('should display result data', () => {
  //   fixture.detectChanges();
  //   const datos = fixture.nativeElement.querySelectorAll('.dato');
  //   expect(datos.length).toEqual(result.length);
  //   datos.forEach((dato, i) => {
  //     expect(dato.querySelector('img').getAttribute('src')).toEqual(result[i].image);
  //     expect(dato.querySelector('.opcion').textContent).toEqual(result[i].option);
  //   });
  // });

  // it('should close dialog on button click', () => {
  //   const closeButton = fixture.nativeElement.querySelector('.rightButton');
  //   closeButton.click();
  //   expect(component.data.close).toHaveBeenCalled();
  // });
});
