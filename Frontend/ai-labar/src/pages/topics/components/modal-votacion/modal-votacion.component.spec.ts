import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ModalVotacionComponent } from './modal-votacion.component';

xdescribe('ModalVotacionComponent', () => {
  let component: ModalVotacionComponent;
  let fixture: ComponentFixture<ModalVotacionComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ModalVotacionComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ModalVotacionComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
