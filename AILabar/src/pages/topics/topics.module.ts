import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TopicsComponent } from './components/topics/topics.component';
import { TopicsRoutingModule } from './topics-routing.module';
import { TopicsCreateComponent } from './components/topics-create/topics-create.component';



@NgModule({
  declarations: [
    TopicsComponent,
    TopicsCreateComponent
  ],
  imports: [
    TopicsRoutingModule,
    CommonModule
  ]
})
export class TopicsModule { }
