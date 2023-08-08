import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ValoracionResultComponent } from './valoracion-result.component';

xdescribe('ValoracionResultComponent', () => {
  let component: ValoracionResultComponent;
  let fixture: ComponentFixture<ValoracionResultComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ValoracionResultComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ValoracionResultComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
