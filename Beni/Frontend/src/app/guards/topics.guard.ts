import { inject } from '@angular/core';
import { CanActivateFn } from '@angular/router';
import { AuthService } from '../service/auth.service';

export const topicsGuard: CanActivateFn = (route, state) => {
  const auth = inject(AuthService);
  return(auth.logout());
};
