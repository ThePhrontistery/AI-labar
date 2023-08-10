import { ComponentFixture, TestBed } from '@angular/core/testing';

import { AnyadirCandidatosTopicComponent } from './anyadir-candidatos-topic.component';

describe('AnyadirCandidatosTopicComponent', () => {
  let component: AnyadirCandidatosTopicComponent;
  let fixture: ComponentFixture<AnyadirCandidatosTopicComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ AnyadirCandidatosTopicComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(AnyadirCandidatosTopicComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
