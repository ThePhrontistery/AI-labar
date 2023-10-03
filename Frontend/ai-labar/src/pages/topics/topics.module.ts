import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { TopicsComponent } from './components/topics/topics.component';
import { TopicsRoutingModule } from './topics-routing.module';
import { TopicsCreateComponent } from './components/topics-create/topics-create.component';
import { TopicsListComponent } from './components/topics-list/topics-list.component';
import { TopicsListService } from '../topics/components/topics-list/topics-list.service';
import { TopicsListServiceMock } from '../topics/components/topics-list/topics-list.service.mock';
import { StepOneComponent } from './components/topics-create/steps/step-one/step-one.component';
import { StepTwoComponent } from './components/topics-create/steps/step-two/step-two.component';
import { FormsModule } from '@angular/forms';
import { MatTableModule } from '@angular/material/table';
import { MatIconModule } from '@angular/material/icon';
import { RouterModule } from '@angular/router';
import { MatSortModule } from '@angular/material/sort';
import { CookieService } from 'ngx-cookie-service';
import { HttpClient, HttpClientModule } from '@angular/common/http';
import { TopicsCreateService } from './components/topics-create/topics-create.service';
import { MatButtonModule } from '@angular/material/button';
import { GroupsComponent } from './components/groups/groups.component';
import { MatDialogModule } from '@angular/material/dialog';
import { MatFormFieldModule } from '@angular/material/form-field';
import { MatExpansionModule } from '@angular/material/expansion';
import { ReactiveFormsModule } from '@angular/forms';
import { MatInputModule } from '@angular/material/input';
import { MatCheckboxModule } from '@angular/material/checkbox';
import { ModalVotationComponent } from './components/modal-votation/modal-votation.component';
import { ModalVotationService } from './components/modal-votation/modal-votation.service';
import { VotingResultsComponent } from './components/voting-results/voting-results.component';
import { TopicResultComponent } from './components/topic-result/topic-result.component';
import { AddGroupsTopicComponent } from './components/add-groups-topic/add-groups-topic.component';
import { RatingResultComponent } from './components/rating-result/rating-result.component';
import { AsResultsComponent } from './components/as-results/as-results.component';
import { MatNativeDateModule } from '@angular/material/core';
import { MatDatepickerModule } from '@angular/material/datepicker';
import { ImageTextResultComponent } from './components/image-text-result/image-text-result.component';
import { MatSelectModule } from '@angular/material/select';
import { AddCandidatesTopicComponent } from './components/add-candidates-topic/add-candidates-topic.component';
import { MatRadioModule } from '@angular/material/radio';
import { MatMenuModule } from '@angular/material/menu';
import { FlexLayoutModule } from '@angular/flex-layout';
import { ConfirmDeletionTopicComponent } from './components/confirm-deletion-topic/confirm-deletion-topic.component';
import { MatPaginatorModule } from '@angular/material/paginator';
import { ScrollingModule } from '@angular/cdk/scrolling';
import { MatCardModule } from '@angular/material/card';
import { MatTooltipModule } from '@angular/material/tooltip';
import { MatSlideToggleModule } from '@angular/material/slide-toggle';
import { TranslateLoader, TranslateModule } from '@ngx-translate/core';
import { TranslateHttpLoader } from '@ngx-translate/http-loader';
import { MatSnackBarModule } from '@angular/material/snack-bar';
import { MessageService } from './services/message.service';
import { ErrorMessageComponent } from './components/error-message/error-message.component';
import { LanguageService } from '../language.service';
import { LoginService } from '../login/login.service';

// Function TranslateModule
export function HttpLoaderFactory(http: HttpClient) {
  return new TranslateHttpLoader(http, './assets/i18n/', '.json');
}

@NgModule({
  declarations: [
    TopicsComponent,
    TopicsCreateComponent,
    TopicsListComponent,
    StepOneComponent,
    StepTwoComponent,
    ModalVotationComponent,
    VotingResultsComponent,
    GroupsComponent,
    TopicResultComponent,
    AddGroupsTopicComponent,
    RatingResultComponent,
    AsResultsComponent,
    ImageTextResultComponent,
    AddCandidatesTopicComponent,
    ConfirmDeletionTopicComponent,
    ErrorMessageComponent,
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
    FlexLayoutModule,
    MatPaginatorModule,
    ScrollingModule,
    MatCardModule,
    MatTooltipModule,
    MatSlideToggleModule,
    TranslateModule.forRoot({
      loader: {
        provide: TranslateLoader,
        useFactory: HttpLoaderFactory,
        deps: [HttpClient],
      },
    }),
    MatSnackBarModule,
  ],
  providers: [
    TopicsListService,
    TopicsListServiceMock,
    CookieService,
    TopicsCreateService,
    ModalVotationService,
    HttpClientModule,
    MessageService,
    LanguageService,
    LoginService
  ],
})
export class TopicsModule {}
