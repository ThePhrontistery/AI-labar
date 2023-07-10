import { Component, OnInit } from '@angular/core';
import { Subscription } from 'rxjs';
import { LoginService } from '../../login.service';
import { Router } from '@angular/router';


@Component({
  selector: 'app-login',
  templateUrl: './login.component.html',
  styleUrls: ['./login.component.scss']
})
export class LoginComponent implements OnInit {
  private mySubscription: Subscription = new Subscription();

  constructor(private loginService: LoginService,
    private router: Router) {}

  ngOnInit(): void {}
  ngOnDestroy() {
    this.mySubscription.unsubscribe();
  }

  loginClick(email: string, password: string): void {
    //  const body = { user: email, password: password };
     this.router.navigate(['topics']);
  //   this.mySubscription = this.http.post('http://localhost:8080/topics/login', body).subscribe(
  //     response => {
  //       console.log(response);
  //     }
  // );
  // const promise = this.http.post('http://localhost:8080/topics/login', body).toPromise();
  //   console.log(promise);  
  //   promise.then((data)=>{
  //     console.log("Promise resolved with: " + JSON.stringify(data));
  //   }).catch((error)=>{
  //     console.log("Promise rejected with " + JSON.stringify(error));
  //   });
  //   this.loginService.login(body).subscribe(
  //     response => {
  //       if (response === 'Login successful') this.router.navigate(['topics']);
  //     }
  // );
  // this.mySubscription = this.loginService.login(body).subscribe({
  //       next: response => console.log(response),
  //       complete: () => console.log('Complete. Cookie time.')
  //     });
  // fetch('http://localhost:8080/topics/login', {
  //     method: 'POST',
  //     headers: {
  //       'Content-Type': 'application/json',
  //     },
  //     body: JSON.stringify(body),
  //   })
  //     .then((response) => {
  //       if (response.ok) {
  //         this.router.navigate(['topics']);
        //   return response.json();
        // } else {
        //   throw response.json(); // Lanzar el objeto de respuesta JSON en caso de error
        // }
      // })
     // .then((responseJson) => {
        // this.cookie.set('user', this.username, 3, '/');
        // this.cookie.set('token', responseJson.message, 3, '/');
      //   this.router.navigate(['/topics']);
      // })
    //  .catch((error: any) => {
        // Mostrar el mensaje de error enviado desde el backend, si está disponible
    //    error.then((json: any) => {
          // if (json && json.message) {
          //   this.passwordError = json.message;
          // } else {
          //   this.passwordError = 'Error en la solicitud';
          // }
        //   console.log('Login error')
        // });
      
  }
  
}

