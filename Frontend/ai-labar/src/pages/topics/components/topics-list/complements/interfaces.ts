
export interface TopicListInterface {
    id: number;
    title: string;
    type: string;
    question: string;
    options: string[];
    votedBy: string;
    author: string;
    members: string[];
    closeDateString: string;
    visits: number;
    status: string;
    canVote: boolean;
  }

