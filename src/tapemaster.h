/* Copyright 2016-2017
   Daniel Seagraves <dseagrav@lunar-tokyo.net>

   This file is part of LambdaDelta.

   LambdaDelta is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 2 of the License, or
   (at your option) any later version.

   LambdaDelta is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with LambdaDelta.  If not, see <http://www.gnu.org/licenses/>.
*/

// Guard
#ifndef TAPEMASTER_H
#define TAPEMASTER_H

void tapemaster_init();
int tapemaster_open_next();
void tapemaster_clock_pulse();
void tapemaster_reset();
void tapemaster_attn();
int tapemaster_open_next();

#endif // TAPEMASTER_H
