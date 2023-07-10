import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { TopicsCreateComponent } from './components/topics-create/topics-create.component';
import { TopicsComponent } from './components/topics/topics.component';

const routes: Routes = [
  {
    path: '',
    component: TopicsComponent,
  },
  {
    path: 'create',
    component: TopicsCreateComponent,
  },
  {
    path: '**',
    redirectTo: '',
  },
];
@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class TopicsRoutingModule { }
