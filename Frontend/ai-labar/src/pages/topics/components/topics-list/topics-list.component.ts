import {
  Component,
  OnDestroy,
  OnInit,
  ViewChild,
  ElementRef,
} from '@angular/core';
import { TopicsListService } from './topics-list.service';
import { MatSort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { CookieService } from 'ngx-cookie-service';
import { MatDialog } from '@angular/material/dialog';
import { TopicResultComponent } from '../topic-result/topic-result.component';
import { RatingResultComponent } from '../rating-result/rating-result.component';
import { AsResultsComponent } from '../as-results/as-results.component';
import { ImageTextResultComponent } from '../image-text-result/image-text-result.component';
import { environment } from 'src/environments/environment';
import { TopicsListServiceMock } from './topics-list.service.mock';
import { ConfirmDeletionTopicComponent } from '../confirm-deletion-topic/confirm-deletion-topic.component';
import {
  MatPaginator,
  MatPaginatorIntl,
  PageEvent,
} from '@angular/material/paginator';
import { Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { LOCALE_ID, Inject } from '@angular/core';
import { TranslateService } from '@ngx-translate/core';
import { MessageService } from '../../services/message.service';

/**
 * Component displaying a list of topics and surveys.
 */
@Component({
  selector: 'app-topics-list',
  templateUrl: './topics-list.component.html',
  styleUrls: ['./topics-list.component.scss'],
})
export class TopicsListComponent implements OnInit, OnDestroy {
  // Variable and property declarations.
  private ngUnsubscribe = new Subject();
  @ViewChild(MatSort) sort!: MatSort;
  @ViewChild('table', { static: true }) table!: ElementRef;
  @ViewChild('scrollContainer') scrollContainer!: ElementRef;
  @ViewChild('topicsTable', { read: ElementRef }) topicsTable!: ElementRef;

  @ViewChild('fill') fill!: ElementRef;

  topicsData: any;
  user: string = this.cookie.get('user');
  displayedColumns: string[] = [
    'title',
    'author',
    'closeDate',
    'status',
    'open',
    'close',
    'delete',
    'vote',
    'result',
    'result2',
  ];

  dataSource: any = new MatTableDataSource<any>([]);

  modalOpen: boolean = false;
  optionsVotation: string[] = [];
  selectedOptions: string[] = [];
  titleVotation: string = '';
  idVotation: any;
  typeVoting: string = '';

  isSurveyVoting: Boolean = false;
  isSurveyRating: Boolean = false;
  isSurveyOpinionSimple: Boolean = false;
  isSurveyOpinionMultiple: Boolean = false;
  isSurveyImageTextSimple: Boolean = false;
  isSurveyImageTextMultiple: Boolean = false;

  minesFilter = false;
  openedFilter = false;
  closedFilter = false;
  votePendingFilter = false;

  isButtonDisabled: boolean = false;

  listItems: any[] = [
    { option: 'Element 1', votes: 1 },
    { option: 'Element 2', votes: 2 },
    { option: 'Element 3', votes: 5 },
    { option: 'Element 4', votes: 0 },
  ];
  showResultsModal = false;
  showPaginatedTable = true;
  showScrollTable = false;
  showCards = false;
  titleSurvey: string = '';

  pageIndex: number = 1;
  pageSize: number = 10;
  totalItems: number | undefined;

  openStatusTranslation: string = '';
  closedStatusTranslation: string = '';

  @ViewChild(MatPaginator) paginator!: MatPaginator;
  loading = false;
  constructor(
    private topicListService: TopicsListService,
    private topicsListServiceMock: TopicsListServiceMock,
    private dialog: MatDialog,
    private matPaginatorIntl: MatPaginatorIntl,
    private translate: TranslateService,
    @Inject(LOCALE_ID) private locale: string,
    private cookie: CookieService,
    private messageService: MessageService
  ) {}

  // Component and value initialization.
  ngOnInit(): void {
    this.defaultVisualization(this.cookie.get('visualization'));
    this.getTopicList();
    this.matPaginatorIntl.itemsPerPageLabel = this.translate.instant(
      'TOPICS_LIST.TOPICS_PAGE'
    );
    this.openStatusTranslation = this.translate.instant(
      'TOPICS_LIST.OPEN_STATUS'
    );
    this.closedStatusTranslation = this.translate.instant(
      'TOPICS_LIST.CLOSED_STATUS'
    );
  }

  ngOnDestroy() {
    this.ngUnsubscribe.complete();
  }

  // Methods for manipulating and filtering the list of topics
  onToggleChange(event: any) {
    if (event.source.id === 'minesToggle' && event.checked) {
      this.minesFilter = event.checked;
    }

    if (event.source.id === 'openedToggle' && event.checked) {
      this.closedFilter = false;
      this.openedFilter = event.checked;
    } else if (event.source.id === 'closedToggle' && event.checked) {
      this.openedFilter = false;
      this.closedFilter = event.checked;
    }

    if (event.source.id === 'votePendingFilter' && event.checked) {
      this.votePendingFilter = event.checked;
    }

    this.dataSource = new MatTableDataSource<any>([]);
    this.pageIndex = 1;
    if (this.paginator) this.paginator.firstPage();
    this.getTopicList();
  }

  filterTopics(): String[] {
    const activeFilters = [];

    if (this.minesFilter) {
      activeFilters.push('mines');
    }
    if (this.openedFilter) {
      activeFilters.push('opened');
    }
    if (this.closedFilter) {
      activeFilters.push('closed');
    }
    if (this.votePendingFilter) {
      activeFilters.push('votePending');
    }
    return activeFilters;
  }

  // Method to reopen a closed topic.
  reOpen(votation: any) {
    const url = `${environment.apiUrl}/topics/reOpenTopic`;
    const closingData = {
      id: votation.id,
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
    };
    let serviceCall;
    if (environment.mockup) {
      serviceCall = this.topicsListServiceMock.reopenTopic(closingData);
    } else {
      serviceCall = this.topicListService.put(closingData, url);
    }

    serviceCall.pipe(takeUntil(this.ngUnsubscribe)).subscribe({
      next: (response) => {
        if (response) {
          if (this.showScrollTable) {
            votation.status = 1;
          } else {
            this.getTopicList();
          }
        }
      },
      error: (error) => {
        let textError = error.error.message;
        if (error.error.message === undefined) textError = error.error.error;
        this.messageService.showErrorMessage(
          this.translate.instant('ERROR_MESSAGES.ERROR_OPEN_TOPIC') +
            '\n' +
            textError
        );
      },
    });
  }

  // Method to close an open topic.
  close(votation: any) {
    const url = `${environment.apiUrl}/topics/closeTopic`;
    const closingData = {
      id: votation.id,
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
    };
    let serviceCall;
    if (environment.mockup) {
      serviceCall = this.topicsListServiceMock.closeTopic(closingData);
    } else {
      serviceCall = this.topicListService.put(closingData, url);
    }

    serviceCall.pipe(takeUntil(this.ngUnsubscribe)).subscribe({
      next: (response) => {
        if (response) {
          if (this.showScrollTable) {
            votation.status = 0;
          } else {
            this.getTopicList();
          }
        }
      },
      error: (error) => {
        let textError = error.error.message;
        if (error.error.message === undefined) textError = error.error.error;
        this.messageService.showErrorMessage(
          this.translate.instant('ERROR_MESSAGES.ERROR_CLOSE_TOPIC') +
            '\n' +
            textError
        );
      },
    });
  }

  // Method to delete a topic.
  delete(votation: any) {
    const dialogRef = this.dialog.open(ConfirmDeletionTopicComponent, {
      width: '250px',
      data: {
        title: this.translate.instant('TOPICS_LIST.CONFIRM_DELETION'),
        message: this.translate.instant('TOPICS_LIST.CONFIRM_DELETION_MESSAGE'),
      },
    });

    dialogRef.afterClosed().subscribe((result) => {
      if (result) {
        const url = `${environment.apiUrl}/topics/deleteTopic`;
        const deletionData = {
          id: votation.id,
          user: this.cookie.get('user'),
          token: this.cookie.get('token'),
        };

        let serviceCall;
        if (environment.mockup) {
          serviceCall = this.topicsListServiceMock.deleteTopic(deletionData);
        } else {
          serviceCall = this.topicListService.delete(deletionData, url);
        }

        serviceCall.pipe(takeUntil(this.ngUnsubscribe)).subscribe({
          next: (response) => {
            if (response) {
              if (this.showScrollTable) {
                const pageSizeBefore = this.pageSize;
                const pageIndexBefore = this.pageIndex - 1;
                this.pageIndex = this.dataSource.data.length;
                this.pageSize = 1;
                this.dataSource.data = this.dataSource.data.filter(
                  (item: { id: any }) => item.id !== votation.id
                );
                this.getTopicList();
                this.pageIndex = pageIndexBefore;
                this.pageSize = pageSizeBefore;
              } else {
                this.getTopicList();
              }
            }
          },
          error: (error) => {
            let textError = error.error.message;
            if (error.error.message === undefined)
              textError = error.error.error;
              this.messageService.showErrorMessage(
              this.translate.instant('ERROR_MESSAGES.ERROR_DELETE_TOPIC') +
                '\n' +
                textError
            );
          },
        });
      }
    });
  }

  // Method to vote in a survey.
  vote(votation: any) {
    this.idVotation = votation.id;
    this.optionsVotation = votation.options;
    this.titleVotation = votation.title;
    this.typeVoting = votation.type;
    this.openModal();
  }

  // Other methods for interacting with surveys and results.
  openModal(): void {
    this.modalOpen = true;
    this.reviewType();
  }

  closeModalVotation(isCancel: any): void {
    this.modalOpen = false;
    this.isSurveyOpinionSimple = false;
    this.isSurveyRating = false;
    this.isSurveyVoting = false;
    this.isSurveyImageTextSimple = false;
    this.isSurveyImageTextMultiple = false;
    this.isSurveyOpinionMultiple = false;
    if (isCancel) {
      if (this.showScrollTable) {
        const registro = this.dataSource.data.find(
          (item: { id: any }) => item.id === this.idVotation
        );

        if (registro) {
          registro.canVote = false;

          this.dataSource.data = [...this.dataSource.data];
        }
      } else {
        this.getTopicList();
      }
    }
  }

  onOptionChange(option: string): void {
    const index = this.selectedOptions.indexOf(option);
    if (index !== -1) {
      this.selectedOptions.splice(index, 1);
    } else {
      this.selectedOptions.push(option);
    }
  }

  reviewType(): void {
    if (this.typeVoting == 'TEXT_MULTIPLE') {
      this.isSurveyOpinionMultiple = true;
    } else if (this.typeVoting == 'RATING') {
      this.isSurveyRating = true;
    } else if (this.typeVoting == 'AS') {
      this.isSurveyVoting = true;
    } else if (this.typeVoting == 'IMAGE_SINGLE') {
      this.isSurveyImageTextSimple = true;
    } else if (this.typeVoting == 'IMAGE_MULTIPLE') {
      this.isSurveyImageTextMultiple = true;
    } else if (this.typeVoting == 'TEXT_SINGLE') {
      this.isSurveyOpinionSimple = true;
    }
  }

  closeResultsModal() {
    this.showResultsModal = false;
  }

  results(votation: any) {
    let component;
    if (votation.type === 'TEXT_MULTIPLE' || votation.type === 'TEXT_SINGLE')
      component = TopicResultComponent;
    if (votation.type === 'AS') component = AsResultsComponent;
    if (votation.type === 'RATING') component = RatingResultComponent;
    if (votation.type === 'IMAGE_SINGLE' || votation.type === 'IMAGE_MULTIPLE')
      component = ImageTextResultComponent;
    if (component) this.openResult(votation, component);
  }
  openResult(votation: any, component: any) {
    const dialogRef = this.dialog.open(component, {
      width: '350px',
      data: { votation },
    });
  }

  // Method to handle page changes.
  onPageChange(event: PageEvent): void {
    this.pageIndex = event.pageIndex + 1;
    this.pageSize = event.pageSize;
    this.getTopicList();
  }

  // Method to check if pagination is possible.
  canPaginate(): boolean {
    return (
      this.totalItems === undefined ||
      (this.totalItems !== undefined &&
        (this.pageIndex - 1) * this.pageSize + 1 <= this.totalItems)
    );
  }

  // Method to get the list of topics.
  getTopicList() {
    if (this.showScrollTable && !this.canPaginate()) return;

    if (this.loading) {
      return;
    }

    this.loading = true;

    const url = `${environment.apiUrl}/topics/loadTopics`;
    const loadTopicsBody = {
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
      page: this.pageIndex,
      elements: this.pageSize,
      filters: this.filterTopics(),
    };
    let serviceCall;
    if (environment.mockup) {
      serviceCall = this.topicsListServiceMock.loadTopics_post(loadTopicsBody);
    } else {
      serviceCall = this.topicListService.post(loadTopicsBody, url);
    }

    serviceCall.pipe(takeUntil(this.ngUnsubscribe)).subscribe({
      next: (response) => {
        if (response) {
          const data = response.entity.entity;

          if (this.dataSource) {
            if (this.showScrollTable) {
              this.dataSource.data = this.dataSource.data.concat(data);
            } else {
              this.dataSource.data = response.entity.entity;
            }
            this.dataSource.sort = this.sort;
          } else {
            this.dataSource = new MatTableDataSource(data);
          }
          if (response.entity.pagination !== undefined) {
            this.totalItems = response.entity.pagination[0].total;
          } else {
            this.totalItems = 0;
          }

          if (this.showScrollTable) {
            this.pageIndex++;
          }
          this.loading = false;
          this.adjustFillHeight();
        }
      },

      error: (error) => {
        let textError = error.error.message;
        if (error.error.message === undefined) textError = error.error.error;
        this.messageService.showErrorMessage(
          this.translate.instant('ERROR_MESSAGES.ERROR_RETRIEVING_TOPICS') +
            '\n' +
            textError
        );
      },
    });
  }

  // Method to handle scrolling and loading more topics.
  onScroll(event: any): void {
    if (!this.showScrollTable) return;
    const element = this.scrollContainer.nativeElement;
    if (element.scrollTop + element.clientHeight >= element.scrollHeight) {
      this.getTopicList();
    }
  }

  // Method to change the default view.
  defaultVisualization(visualization: string) {
    switch (visualization) {
      case 'Pagination': {
        this.cookie.set('visualization', visualization);
        this.showScrollTable = false;
        this.showPaginatedTable = true;
        this.showCards = false;
        break;
      }
      case 'Scroll': {
        this.cookie.set('visualization', visualization);
        this.showScrollTable = true;
        this.showPaginatedTable = false;
        this.showCards = false;
        break;
      }
      case 'Cards': {
        this.cookie.set('visualization', visualization);
        this.showScrollTable = false;
        this.showPaginatedTable = false;
        this.showCards = true;
        break;
      }
    }

    this.dataSource = new MatTableDataSource<any>([]);
    this.pageIndex = 1;
    if (this.paginator) this.paginator.firstPage();
  }

  // Method to change the way topics are displayed.
  changeDisplay(visualization: string) {
    if (this.isButtonDisabled) {
      return;
    }

    this.isButtonDisabled = true;

    const visualizationBody = {
      user: this.cookie.get('user'),
      token: this.cookie.get('token'),
      visualization: visualization,
    };

    this.defaultVisualization(visualization);
    this.editVisualizationFetch(visualizationBody);

    setTimeout(() => {
      this.getTopicList();
      this.isButtonDisabled = false;
    }, 250);
  }

  // Method to edit the display through an HTTP request.
  editVisualizationFetch(visualizationBody: any) {
    fetch(`${environment.apiUrl}/users/editVisualization`, {
      method: 'PUT',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(visualizationBody),
    })
      .then((response) => {})
      .catch((error) => {});
  }

  // Method to adjust the padding height between the table and the container.
  adjustFillHeight() {
    if (!this.scrollContainer || !this.topicsTable || !this.fill) {
      return;
    }
    const div1Height = this.scrollContainer.nativeElement.clientHeight;
    const table1Height = this.topicsTable.nativeElement.clientHeight;
    const fillHeight = div1Height - table1Height + 6;
    this.fill.nativeElement.style.height = fillHeight + 'px';
  }
}
