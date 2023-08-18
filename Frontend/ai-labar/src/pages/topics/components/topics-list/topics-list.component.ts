import { Component, OnDestroy, OnInit, ViewChild } from '@angular/core';
import { TopicsListService } from './topics-list.service';
import { MatSort } from '@angular/material/sort';
import { MatTableDataSource } from '@angular/material/table';
import { CookieService } from 'ngx-cookie-service';
import { MatDialog } from '@angular/material/dialog';
import { TopicResultComponent } from '../topic-result/topic-result.component';
import { ValoracionResultComponent } from '../valoracion-result/valoracion-result.component';
import { AsResultsComponent } from '../as-results/as-results.component';
import { ImageTextResultComponent } from '../image-text-result/image-text-result.component';
import { environment } from 'src/environments/environment';
import { TopicsListServiceMock } from './topics-list.service.mock';
import { ConfirmarEliminacionTopicComponent } from '../confirmar-eliminacion-topic/confirmar-eliminacion-topic.component';
import { MatPaginator, MatPaginatorIntl, PageEvent } from '@angular/material/paginator';
import { Subject } from 'rxjs';
import { takeUntil } from 'rxjs/operators';
import { LOCALE_ID, Inject } from '@angular/core';

@Component({
  selector: 'app-topics-list',
  templateUrl: './topics-list.component.html',
  styleUrls: ['./topics-list.component.scss']
})
export class TopicsListComponent implements OnInit, OnDestroy {

  private ngUnsubscribe = new Subject();
  @ViewChild(MatSort) sort!: MatSort; // Importante: agregar ViewChild para el MatSort

  topicsData: any;
  user: string = this.cookie.get('user');
  displayedColumns: string[] = ['title', 'author', 'closeDate', 'status', 'open', 'close', 'delete', 'vote', 'result', 'result2'];

  dataSource: any = new MatTableDataSource<any>([]);

  modalOpen: boolean = false;
  optionsVotacion: string[] = [];
  selectedOptions: string[] = [];
  titleVotacion: string = '';
  idVotation: any;
  typeVotacion: string = '';

  isEnquestaVotacion: Boolean = false;
  isEnquestaValoracion: Boolean = false;
  isEncuestaOpinionSimple: Boolean = false;
  isEncuestaOpinionMultiple: Boolean = false;
  isEnquestaImagenTextoSimple: Boolean = false;
  isEnquestaImagenTextoMultiple: Boolean = false;

  listItems: any[] = [{ option: 'Elemento 1', votes: 1 }, { option: 'Elemento 2', votes: 2 }, { option: 'Elemento 3', votes: 5 }, { option: 'Elemento 4', votes: 0 }];
  showModalResultados = false;
  titleEncuesta: string = '';

  pageIndex: number = 1;
  pageSize: number = 10; // Cantidad de elementos por página
  totalItems: number | undefined;

  @ViewChild(MatPaginator, { static: true })
  paginator!: MatPaginator;

  constructor(private topicListService: TopicsListService,
    private topicsListServiceMock: TopicsListServiceMock,
    private dialog: MatDialog,
    private matPaginatorIntl: MatPaginatorIntl,
    @Inject(LOCALE_ID) private locale: string,
    private cookie: CookieService) {
  }

  ngOnInit(): void {
    this.getTopicList();
    //this.paginator._intl.itemsPerPageLabel="Topics por página: ";
    this.matPaginatorIntl.itemsPerPageLabel = "Topics por página: ";
    this.matPaginatorIntl.getRangeLabel = this.getRangeLabel.bind(this);
  }
  ngOnDestroy() {
    //this.ngUnsubscribe.next();
    this.ngUnsubscribe.complete();
  }

  getTopicList() {
    const url = `${environment.apiUrl}/topics/loadTopics`;
    const loadTopicsBody = {
      "user": this.cookie.get("user"),
      "token": this.cookie.get("token"),
      "page": this.pageIndex,
      "elements": this.pageSize
    }
    let serviceCall;
    if (environment.mockup) {
      serviceCall = this.topicsListServiceMock.loadTopics_post(loadTopicsBody);
    } else {
      serviceCall = this.topicListService.post(loadTopicsBody, url);
    }

    serviceCall
      .pipe(takeUntil(this.ngUnsubscribe))
      .subscribe({
        next: response => {
          if (response) {
            this.dataSource.data = response.entity.entity;
            this.dataSource.sort = this.sort; 
            if(response.entity.pagination!==undefined){
              this.totalItems = response.entity.pagination[0].total;}
            else{this.totalItems = 0;}
            /*this.dataSource.paginator = this.paginator;
            */
          }
        },

        error: error => {
          alert('Error al obtener los topicos: ' + error.error.message);
        }
      }
      );
  }
  reOpen(votation: any) {
    const url = `${environment.apiUrl}/topics/reOpenTopic`;
    const closingData = {
      "id": votation.id,
      "user": this.cookie.get("user"),
      "token": this.cookie.get("token")
    }
    let serviceCall;
    if (environment.mockup) {
      serviceCall = this.topicsListServiceMock.reopenTopic(closingData);
    } else {
      serviceCall = this.topicListService.put(closingData, url);
    }

    serviceCall
      .pipe(takeUntil(this.ngUnsubscribe))
      .subscribe({
        next: response => {
          if (response) {
            this.getTopicList();
          }
        },
        error: error => {
          alert('Error al abrir el topico: ' + error.error.message);
        }
      }
      );

  }
  close(votation: any) {
    const url = `${environment.apiUrl}/topics/closeTopic`;
    const closingData = {
      "id": votation.id,
      "user": this.cookie.get("user"),
      "token": this.cookie.get("token")
    }
    let serviceCall;
    if (environment.mockup) {
      serviceCall = this.topicsListServiceMock.closeTopic(closingData);
    } else {
      serviceCall = this.topicListService.put(closingData, url);
    }

    serviceCall
      .pipe(takeUntil(this.ngUnsubscribe))
      .subscribe({
        next: response => {
          if (response) {
            this.getTopicList();
          }
        },
        error: error => {
          alert('Error al cerra el topico: ' + error.error.message);
        }
      });


  }
  delete(votation: any) {
    const dialogRef = this.dialog.open(ConfirmarEliminacionTopicComponent, {
      width: '250px',
      data: {
        title: 'Confirmar Eliminación',
        message: '¿Estás seguro de que deseas eliminar esta encuesta?'
      }
    });

    dialogRef.afterClosed().subscribe(result => {
      if (result) {
        const url = `${environment.apiUrl}/topics/deleteTopic`;
        const deletionData = {
          "id": votation.id,
          "user": this.cookie.get("user"),
          "token": this.cookie.get("token")
        }


        let serviceCall;
        if (environment.mockup) {
          serviceCall = this.topicsListServiceMock.deleteTopic(deletionData);
        } else {
          serviceCall = this.topicListService.delete(deletionData, url);
        }

        serviceCall
          .pipe(takeUntil(this.ngUnsubscribe))
          .subscribe({
            next: response => {
              if (response) {
                this.getTopicList();
              }
            },
            error: error => {
              alert('Error al borrar el topico: ' + error.error.message);
            }
          }
          );
      }
    });
  }
  vote(votation: any) {
    //console.log(votation);
    this.idVotation = votation.id;
    //this.optionsVotacion = votation.optionsDataList;
    this.optionsVotacion = votation.options;
    this.titleVotacion = votation.title;
    this.typeVotacion = votation.type;
    this.openModal();
  }

  openModal(): void {
    this.modalOpen = true;
    this.revisarType();
  }

  closeModalVotacion(): void {
    this.modalOpen = false;
    this.isEncuestaOpinionSimple = false;
    this.isEnquestaValoracion = false;
    this.isEnquestaVotacion = false;
    this.isEnquestaImagenTextoSimple = false;
    this.isEnquestaImagenTextoMultiple = false;
    this.isEncuestaOpinionMultiple = false;
    this.getTopicList();
  }

  onOptionChange(option: string): void {
    const index = this.selectedOptions.indexOf(option);
    if (index !== -1) {
      this.selectedOptions.splice(index, 1);
    } else {
      this.selectedOptions.push(option);
    }
  }

  revisarType(): void {
    if (this.typeVotacion == "TEXT_MULTIPLE") {
      this.isEncuestaOpinionMultiple = true;
    } else if (this.typeVotacion == "RATING") {
      this.isEnquestaValoracion = true;
    } else if (this.typeVotacion == "AS") {
      this.isEnquestaVotacion = true;
    } else if (this.typeVotacion == "IMAGE_SINGLE") {
      this.isEnquestaImagenTextoSimple = true;
    } else if (this.typeVotacion == "IMAGE_MULTIPLE") {
      this.isEnquestaImagenTextoMultiple = true;
    } else if (this.typeVotacion == "TEXT_SINGLE") {
      this.isEncuestaOpinionSimple = true;
    }
  }

  openModalResultados(votation: any) {
    this.titleEncuesta = votation.title;
    this.showModalResultados = true;
  }

  closeModalResultados() {
    this.showModalResultados = false;
  }

  results(votacion: any) {
    let component;
    if (votacion.type === 'TEXT_MULTIPLE' || votacion.type === 'TEXT_SINGLE') component = TopicResultComponent;
    if (votacion.type === 'AS') component = AsResultsComponent;
    if (votacion.type === 'RATING') component = ValoracionResultComponent;
    if (votacion.type === 'IMAGE_SINGLE' || votacion.type === 'IMAGE_MULTIPLE') component = ImageTextResultComponent;
    if (component) this.openResult(votacion, component)
  }
  openResult(votacion: any, component: any) {
    const dialogRef = this.dialog.open(component, {
      width: '350px',
      data: { votacion },
    });
  }

  onPageChange(event: PageEvent): void {
    this.pageIndex = event.pageIndex + 1;
    this.pageSize = event.pageSize;
    this.getTopicList();
  }

  private getRangeLabel(page: number, pageSize: number, length: number): string {
    if (length === 0 || pageSize === 0) {
      return `0 de ${length}`;
    }

    const startIndex = page * pageSize + 1;
    const endIndex = Math.min(startIndex + pageSize - 1, length);

    return `${startIndex} – ${endIndex} de ${length}`;
  }

}
