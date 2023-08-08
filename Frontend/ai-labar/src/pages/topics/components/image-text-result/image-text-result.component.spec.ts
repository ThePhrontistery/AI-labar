import { ComponentFixture, TestBed } from '@angular/core/testing';

import { ImageTextResultComponent } from './image-text-result.component';

xdescribe('ImageTextResultComponentComponent', () => {
  let component: ImageTextResultComponent;
  let fixture: ComponentFixture<ImageTextResultComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ ImageTextResultComponent ]
    })
    .compileComponents();
  });

  beforeEach(() => {
    fixture = TestBed.createComponent(ImageTextResultComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
