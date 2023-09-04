import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';

const routes: Routes = [
  {
    path: 'login',
    loadChildren: () => import('../pages/login/login.module').then((m) => m.LoginModule),
  },
  {
    path: 'topics',
    loadChildren: () => import('../pages/topics/topics.module').then((m) => m.TopicsModule),
  },
  {
    path: '**',
    redirectTo: 'login',
  },
  
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule],
})
export class AppRoutingModule {}
