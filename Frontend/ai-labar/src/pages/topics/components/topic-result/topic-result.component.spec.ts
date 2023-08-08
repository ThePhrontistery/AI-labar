import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TopicResultComponent } from './topic-result.component';

xdescribe('TopicResultComponent', () => {
  let component: TopicResultComponent;
  let fixture: ComponentFixture<TopicResultComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ TopicResultComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(TopicResultComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
