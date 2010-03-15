#ifndef MSWD6_32_H
#define MSWD6_32_H

typedef unsigned long (pascal *PFN_RTF_CALLBACK)(int, int);

extern "C" int pascal InitConverter32(HANDLE, char *);
extern "C" HANDLE pascal RegisterApp32(unsigned long, void *);
extern "C" int pascal IsFormatCorrect32(HANDLE, HANDLE);
extern "C" int pascal ForeignToRtf32(HANDLE, void *, HANDLE, HANDLE, HANDLE, PFN_RTF_CALLBACK);
extern "C" int pascal RtfToForeign32(HANDLE, LPSTORAGE, HANDLE, HANDLE, PFN_RTF_CALLBACK);

#endif
