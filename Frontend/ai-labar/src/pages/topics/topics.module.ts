import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TopicsComponent } from './components/topics/topics.component';
import { TopicsRoutingModule } from './topics-routing.module';
import { TopicsCreateComponent } from './components/topics-create/topics-create.component';
import { TopicsListComponent } from './components/topics-list/topics-list.component';
import { TopicsListService } from '../topics/components/topics-list/topics-list.service';
import { TopicsListServiceMock } from '../topics/components/topics-list/topics-list.service.mock';
import { PasoUnoComponent } from './components/topics-create/pasos/paso-uno/paso-uno.component';
import { PasoDosComponent } from './components/topics-create/pasos/paso-dos/paso-dos.component';
import { FormsModule } from '@angular/forms';
import { MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { RouterModule } from '@angular/router';
import { MatSortModule } from '@angular/material/sort';
import { CookieService } from 'ngx-cookie-service';
import { HttpClientModule } from '@angular/common/http';
import { TopicsCreateService } from './components/topics-create/topics-create.service';
import { MatButtonModule } from '@angular/material/button';
import { GroupsComponent } from './components/groups/groups.component';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatExpansionModule } from '@angular/material/expansion';
import { ReactiveFormsModule } from '@angular/forms';
import {MatInputModule} from '@angular/material/input';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { ModalVotacionComponent } from './components/modal-votacion/modal-votacion.component';
import { ModalVotacionService } from './components/modal-votacion/modal-votacion.service';
import { ResultadosVotacionComponent } from './components/resultados-votacion/resultados-votacion.component';
import { TopicResultComponent } from './components/topic-result/topic-result.component';
import { AnyadirGruposTopicComponent } from './components/anyadir-grupos-topic/anyadir-grupos-topic.component';
import { ValoracionResultComponent } from './components/valoracion-result/valoracion-result.component';
import { AsResultsComponent } from './components/as-results/as-results.component';
import { MatNativeDateModule } from '@angular/material/core';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { ImageTextResultComponent } from './components/image-text-result/image-text-result.component';
import { MatSelectModule } from '@angular/material/select';
import { AnyadirCandidatosTopicComponent } from './components/anyadir-candidatos-topic/anyadir-candidatos-topic.component';
import { MatRadioModule } from '@angular/material/radio';
import { MatMenuModule } from '@angular/material/menu';
import { FlexLayoutModule } from '@angular/flex-layout';
import { ConfirmarEliminacionTopicComponent } from './components/confirmar-eliminacion-topic/confirmar-eliminacion-topic.component';
@NgModule({
  declarations: [
    TopicsComponent,
    TopicsCreateComponent,
    TopicsListComponent,
    PasoUnoComponent,
    PasoDosComponent,
    ModalVotacionComponent,
    ResultadosVotacionComponent,
    GroupsComponent,
    TopicResultComponent,
    AnyadirGruposTopicComponent,
    ValoracionResultComponent,
    AsResultsComponent,
    ImageTextResultComponent,
    AnyadirCandidatosTopicComponent,
    ConfirmarEliminacionTopicComponent
  ],
  imports: [
    TopicsRoutingModule,
    CommonModule,
    FormsModule,
    HttpClientModule,
    MatTableModule,
    MatIconModule,
    MatSortModule,
    RouterModule,
    MatButtonModule,
    MatDialogModule,
    MatExpansionModule,
    MatCheckboxModule,
    MatFormFieldModule,
    MatInputModule,
    ReactiveFormsModule,
    MatDatepickerModule,
    MatNativeDateModule,
    MatSelectModule,
    MatRadioModule,
    MatMenuModule,
    FlexLayoutModule
  ],
  providers: [TopicsListService,TopicsListServiceMock, CookieService, TopicsCreateService, ModalVotacionService, HttpClientModule]
})
export class TopicsModule { }
