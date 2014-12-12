/*
 * Extra bits for CUFFT bindings
 */

#ifndef C_WRAP_H
#define C_WRAP_H

/*
 * We need to work around some shortcomings in the C parser of c2hs by disabling advanced attributes etc on Apple platforms.
 */
#ifdef __APPLE__
#define _ANSI_SOURCE
#define __AVAILABILITY__
#define __OSX_AVAILABLE_STARTING(_mac, _iphone)
#define __OSX_AVAILABLE_BUT_DEPRECATED(_macIntro, _macDep, _iphoneIntro, _iphoneDep)
#define __OSX_AVAILABLE_BUT_DEPRECATED_MSG(_osxIntro, _osxDep, _iosIntro, _iosDep, _msg)
#endif

#include <cufft.h>

#endif

