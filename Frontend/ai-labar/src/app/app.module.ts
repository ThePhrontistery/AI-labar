import { NgModule } from '@angular/core';
import { BrowserModule, DomSanitizer } from '@angular/platform-browser';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { BrowserAnimationsModule } from '@angular/platform-browser/animations';
import { HttpClientModule } from '@angular/common/http';
import { MatIconRegistry } from '@angular/material/icon';

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule,
    AppRoutingModule,
    BrowserAnimationsModule,
    HttpClientModule
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule {
  constructor(private matIconRegistry: MatIconRegistry, private domSanitizer: DomSanitizer) {
    matIconRegistry.addSvgIcon('lock_open', domSanitizer.bypassSecurityTrustResourceUrl('assets/icons/lock_open.svg'));
    matIconRegistry.addSvgIcon('lock', domSanitizer.bypassSecurityTrustResourceUrl('assets/icons/lock.svg'));
    matIconRegistry.addSvgIcon('delete', domSanitizer.bypassSecurityTrustResourceUrl('assets/icons/delete.svg'));
    matIconRegistry.addSvgIcon('how_to_vote', domSanitizer.bypassSecurityTrustResourceUrl('assets/icons/how_to_vote.svg'));
    matIconRegistry.addSvgIcon('visibility', domSanitizer.bypassSecurityTrustResourceUrl('assets/icons/visibility.svg'));
    matIconRegistry.addSvgIcon('more_vert', domSanitizer.bypassSecurityTrustResourceUrl('assets/icons/more_vert.svg'));
    matIconRegistry.addSvgIcon('arrow_drop_down', domSanitizer.bypassSecurityTrustResourceUrl('assets/icons/arrow_drop_down.svg'));
    matIconRegistry.addSvgIcon('account_circle', domSanitizer.bypassSecurityTrustResourceUrl('assets/icons/account_circle.svg'));
  }
}
