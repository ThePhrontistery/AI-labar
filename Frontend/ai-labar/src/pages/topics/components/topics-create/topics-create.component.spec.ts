import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TopicsCreateComponent } from './topics-create.component';

xdescribe('TopicsCreateComponent', () => {
  let component: TopicsCreateComponent;
  let fixture: ComponentFixture<TopicsCreateComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ TopicsCreateComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(TopicsCreateComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
