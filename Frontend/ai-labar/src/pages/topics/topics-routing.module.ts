import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { TopicsCreateComponent } from './components/topics-create/topics-create.component';
import { TopicsListComponent } from './components/topics-list/topics-list.component';
import { TopicsComponent } from './components/topics/topics.component';

const routes: Routes = [
  {
    path: '',
    component: TopicsComponent, children: [
      { path: 'create', component: TopicsCreateComponent },
      { path: 'topics-list', component: TopicsListComponent},
      { path: 'topics-create', component: TopicsCreateComponent}]
  },
  {
    path: '**',
    redirectTo: 'topics/topics-list',
  },
];
@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule]
})
export class TopicsRoutingModule { }
