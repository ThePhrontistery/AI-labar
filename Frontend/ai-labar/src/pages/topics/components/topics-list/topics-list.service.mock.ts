import { Observable, of } from 'rxjs';

export class TopicsListServiceMock {
    private topics: any[] = [
        {
            id: 12,
            title: '¿Cuándo preferís las tardes libres?',
            type: 'TEXT_SINGLE',
            question: 'Visítame',
            options: [
              {
                option: 'Sí',
              },
              {
                option: 'No',
              },
            ],
            author: 'Test',
            members: ['Beni', 'Lester'],
            visits: 4,
            status: 'Abierto',
            canVote: false,
          },
          {
            id: 13,
            title: '¿Cuándo preferís ir a la oficina?',
            type: 'TEXT_MULTIPLE',
            question: '¡Ey! ¿Cuándo preferís ir a la oficina?',
            options: [
              {
                option: 'Sí',
              },
              {
                option: 'No',
              },
            ],
            author: 'Beni',
            members: ['Beni', 'Lester'],
            visits: 2,
            status: 'Abierto',
            canVote: true,
          },
    ];
  loadTopics_post(json: any): Observable<any> {
    return of({ entity: this.topics, message: 'OK' });
  }
  reopenTopic(json: any): Observable<any> {
    const topicId = json.id;
    const topicIndex = this.topics.findIndex(topic => topic.id === topicId);

    if (topicIndex !== -1) {
      this.topics[topicIndex].status = 'Abierto';
    }

    return of({ message: 'Topic reopened' });
  }
  closeTopic(json: any): Observable<any> {
    // Mock response for reopening a topic
    const topicId = json.id;
    const topicIndex = this.topics.findIndex(topic => topic.id === topicId);

    if (topicIndex !== -1) {
      this.topics[topicIndex].status = 'Cerrado';
    }

    return of({ message: 'The topic has been closed' });
  }  
  deleteTopic(json: any): Observable<any> {
    const topicId = json.id;
    const topicIndex = this.topics.findIndex(topic => topic.id === topicId);

    if (topicIndex !== -1) {
      this.topics.splice(topicIndex, 1);
    }

    return of({ message: 'The topic has been deleted' });
  }
}
