import { Component, OnInit } from '@angular/core';
import { Router } from '@angular/router';
import { CookieService } from 'ngx-cookie-service';

@Component({
  selector: 'app-topics',
  templateUrl: './topics.component.html',
  styleUrls: ['./topics.component.css'],
})
export class TopicsComponent implements OnInit {
  topics: any[] = [];

  constructor(private router: Router, private cookie: CookieService) {}

  ngOnInit() {
    this.loadTopics();
  }

  loadTopics() {
    const userCookie = this.cookie.get('user');
    const tokenCookie = this.cookie.get('token');

    const data = {
      user: userCookie,
      token: tokenCookie,
    };

    fetch('http://localhost:8080/topics/loadTopics', {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
      },
      body: JSON.stringify(data),
    })
      .then((response) => {
        if (response.ok) {
          return response.json();
        } else {
          throw new Error('Error en la solicitud');
        }
      })
      .then((responseJson) => {
        this.topics = responseJson.entity;
      })
      .catch((error) => {
        console.log(error);
      });
  }

  getUsernameFromCookie(): string {
    return this.cookie.get('user');
  }

  logout() {
    this.cookie.delete('user');
    this.cookie.delete('token');
    this.router.navigate(['/', 'login']);
  }
}
