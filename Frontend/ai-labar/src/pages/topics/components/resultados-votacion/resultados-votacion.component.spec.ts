import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ResultadosVotacionComponent } from './resultados-votacion.component';

describe('RestultadosVotacionComponent', () => {
  let component: ResultadosVotacionComponent;
  let fixture: ComponentFixture<ResultadosVotacionComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ResultadosVotacionComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ResultadosVotacionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
