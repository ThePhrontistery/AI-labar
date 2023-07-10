import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { LoginComponent } from './components/login/login.component';
import { TopicsComponent } from './components/topics/topics.component';
import { PagenotfoundComponent } from './components/pagenotfound/pagenotfound.component';
import { loginGuard } from './guards/login.guard';
import { topicsGuard } from './guards/topics.guard';

const routes: Routes = [
  { path: '', redirectTo: '/login', pathMatch: 'full' },
  { path: 'login', component: LoginComponent, canActivate: [loginGuard]},
  { path: 'topics', component: TopicsComponent, canActivate: [topicsGuard]},
  { path: '**', component: PagenotfoundComponent}
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule]
})
export class AppRoutingModule { }
