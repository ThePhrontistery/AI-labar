import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ConfirmarEliminacionTopicComponent } from './confirmar-eliminacion-topic.component';

describe('ConfirmarEliminacionTopicComponent', () => {
  let component: ConfirmarEliminacionTopicComponent;
  let fixture: ComponentFixture<ConfirmarEliminacionTopicComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ConfirmarEliminacionTopicComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ConfirmarEliminacionTopicComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
