#ifndef __RWEPERSIST_H__
#define __RWEPERSIST_H__

/***************************************************************************
 *
 * epersist.h - declarations for external template persistence.
 *
 * $Id: epersist.h,v 1.1 1996/04/09 18:44:08 rohde Exp $
 *
 * (c) Copyright 1989 - 1996 Rogue Wave Software, Inc.
 * ALL RIGHTS RESERVED
 *
 * The software and information contained herein are proprietary to, and
 * comprise valuable trade secrets of, Rogue Wave Software, Inc., which
 * intends to preserve as trade secrets such software and information.
 * This software is furnished pursuant to a written license agreement and
 * may be used, copied, transmitted, and stored only in accordance with
 * the terms of such license and with the inclusion of the above copyright
 * notice.  This software and information or any other copies thereof may
 * not be provided or otherwise made available to any other person.
 *
 * Notwithstanding any other lease or license that may pertain to, or
 * accompany the delivery of, this computer software and information, the
 * rights of the Government regarding its use, reproduction and disclosure
 * are as set forth in Section 52.227-19 of the FARS Computer
 * Software-Restricted Rights clause.
 * 
 * Use, duplication, or disclosure by the Government is subject to
 * restrictions as set forth in subparagraph (c)(1)(ii) of the Rights in
 * Technical Data and Computer Software clause at DFARS 252.227-7013.
 * Contractor/Manufacturer is Rogue Wave Software, Inc.,
 * P.O. Box 2328, Corvallis, Oregon 97339.
 *
 * This computer software and information is distributed with "restricted
 * rights."  Use, duplication or disclosure is subject to restrictions as
 * set forth in NASA FAR SUP 18-52.227-79 (April 1985) "Commercial
 * Computer Software-Restricted Rights (April 1985)."  If the Clause at
 * 18-52.227-74 "Rights in Data General" is specified in the contract,
 * then the "Alternate III" clause applies.
 *
 ****************************************************************************
 *
 * $Log: epersist.h,v $
 * Revision 1.1  1996/04/09 18:44:08  rohde
 * Initial revision
 *
 * Revision 7.33  1996/03/25 22:55:40  kevinj
 * Bug #2941: Added RWTEXPORT_RETURN and changed RWTExport to RWExport
 * in RWTRegularExpressionTrait<char> specialization for MSVC DLL's.
 *
 * Revision 7.32  1996/03/18 06:48:52  jims
 * Use "rw" when including epersist.cc
 *
 * Revision 7.31  1996/03/18 06:41:15  jims
 * Move definitions of rwInsertInXxx to epersist.cc
 *
 * Revision 7.30  1996/03/15 05:34:34  griswolf
 * Bug #2805: Added RWEXPORT_RETURN(x) to new extraction and insertion
 * operators to make them visible to DLL's import libraries.
 *
 * Revision 7.29  1996/03/15 01:54:57  griswolf
 * Patch around rwInsertXXX for microsoft (and others?)
 *
 * Revision 7.28  1996/03/06 18:44:58  griswolf
 * Fix saveGuts to avoid copying data prior to streaming.
 *
 * Revision 7.27  1996/02/18 05:00:35  kyle
 * Millenium changes #includes from <> to ""
 *
 * Revision 7.26  1996/02/18 01:41:06  griswolf
 * Replace tabs with spaces, per Rogue Wave standard.
 *
 * Revision 7.25  1996/02/15 01:18:11  griswolf
 * Format macros for easier reading.
 *
 * Revision 7.24  1996/02/14 18:15:56  kevinj
 * Shortened macro names to less than 32 characters to deal
 * with a compiler limitation.
 *
 * Revision 7.23  1996/02/12 21:03:06  kevinj
 * Switched stream and class args in rw*Guts.
 *
 * Revision 7.22  1996/02/07 06:04:13  jims
 * Remove devnotes (#2555)
 *
 * Revision 7.21  1996/01/31 09:11:20  jims
 * Remove declarations of rwSave/RestoreGuts from template versions
 * of macros to avoid MS Internal Compiler Error
 *
 * Revision 7.20  1996/01/25 01:54:52  kevinj
 * Improved ETP macros.
 *
 * Revision 7.19  1996/01/19 01:15:05  kevinj
 * ETP for RW classes.
 *
 * Revision 7.18  1996/01/13 22:08:59  kevinj
 * Simple External Template Persistence (ETP).
 *
 * Revision 7.17  1996/01/12 23:30:16  kevinj
 * RWT*Map simpler External Template Persistence (ETP).
 *
 * Revision 7.16  1996/01/12 01:39:14  kevinj
 * RWT{Val,Ptr}Set External Template Persistence (ETP).
 *
 * Revision 7.15  1996/01/11 01:33:10  kevinj
 * RWFile external template persistence.
 *
 * Revision 7.14  1996/01/10 21:09:42  kevinj
 * RWDEFINE_PERSISTABLE_CONTAINER and RWDECLARE_PERSISTABLE_CONTAINER.
 *
 * Revision 7.13  1996/01/08 17:58:50  kevinj
 * Removed RW*Persistor stuff.
 *
 * Revision 7.12  1996/01/03 22:55:33  pearson
 * Include edefs.h to bring in definition of RWPersist
 *
 * Revision 7.11  1996/01/03 22:49:14  pearson
 * Dedossify
 *
 * Revision 7.10  1995/12/20 23:19:20  kevinj
 * External template persistence using RW*Persistor.
 *
 * Revision 7.9  1995/11/03 01:32:15  jims
 * Delete pointer in RWMakeNewPtr if stream tests !good()
 *
 * Revision 7.8  1995/10/20  06:47:28  jims
 * port to msvc 4.0
 *
 * Revision 7.7  1995/09/05  21:43:02  jims
 * Change tlyrs.h to tlyrs.str
 *
 * Revision 7.6  1995/08/24  02:11:44  jims
 * Explicitly declare inheritance as "private" to avoid SGI warning
 *
 * Revision 7.5  1995/08/23  23:38:34  kevinj
 * Changed RWMakeNewPtr to return rwnil on stream errors.
 *
 * Revision 7.4  1995/08/16  18:06:13  kevinj
 * Changed RWPersistTable and RWRestoreTable add() to insert().
 * Removed #1812 workarounds.
 *
 * Revision 7.3  1995/08/11  23:38:18  kevinj
 * restoreFrom and saveOn for RWFile.
 *
 * Revision 7.2  1995/08/11  17:03:34  kevinj
 * Template persistence 2.2.
 *
 * Revision 7.1  1995/07/28  19:03:04  jims
 * Move revision to 7.1
 *
 * Revision 1.6  1995/07/28  17:53:35  kevinj
 * Modified in accordance with Template Persistence Design Document 2.1.
 *
 ****************************************************************************/

#include "rw/vstream.h"
#include "rw/rwfile.h"
#include "rw/toolerr.h"
#include "rw/rwstore.h"

#define RWDEFINE_PERSIST_IO(CLASS,ISTR,OSTR)                            \
                                                                        \
RWEXPORT_RETURN(OSTR&) operator<<(OSTR& stream, const CLASS& item)      \
{                                                                       \
  void rwSaveGuts(OSTR&, const CLASS&);                                 \
  int objectNum;                                                        \
                                                                        \
  RWUseStoreTable storeTable;                                           \
                                                                        \
  if(storeTable.add(&item, objectNum)){                                 \
    stream << ':';                                                      \
    rwSaveGuts(stream,item);                                            \
  }                                                                     \
  else                                                                  \
    stream << '@' << objectNum;                                         \
                                                                        \
  return stream;                                                        \
}                                                                       \
                                                                        \
ISTR& operator>>(ISTR& stream, CLASS& object)                           \
{                                                                       \
  void rwRestoreGuts(ISTR&, CLASS&);                                    \
                                                                        \
  if (!stream.good()) return stream;                                    \
                                                                        \
  int            objectNum;                                             \
  char           refFlag;                                               \
  RWUseReadTable readTable;                                             \
                                                                        \
  stream >> refFlag;                                                    \
  if (!stream.good()) return stream;                                    \
                                                                        \
  if(refFlag == '@')                                                    \
  {                                                                     \
    stream >> objectNum;                                                \
    if (!stream.good()) return stream;                                  \
    if (   (size_t) objectNum >= readTable.entries()                    \
        || (&object != (const void*)readTable(objectNum))               \
       )                                                                \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  else if (refFlag == ':') {                                            \
    if (!stream.good()) return stream;                                  \
    readTable.append(&object);                                          \
    rwRestoreGuts(stream, object);                                      \
  }                                                                     \
  else {                                                                \
    /* Neither '@' nor ':' was found.  Bad input stream. */             \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  return stream;                                                        \
}                                                                       \
                                                                        \
ISTR& operator>>(ISTR& stream, CLASS*& objectPtr)                       \
{                                                                       \
  void rwRestoreGuts(ISTR&, CLASS&);                                    \
                                                                        \
  if (!stream.good()) return stream;                                    \
                                                                        \
  int            objectNum;                                             \
  char           refFlag;                                               \
  RWUseReadTable readTable;                                             \
                                                                        \
  stream >> refFlag;                                                    \
  if (!stream.good()) return stream;                                    \
                                                                        \
  if(refFlag == '@')                                                    \
  {                                                                     \
    stream >> objectNum;                                                \
    if (!stream.good()) return stream;                                  \
    if ((size_t) objectNum >= readTable.entries())                      \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
    else                                                                \
      objectPtr = (CLASS*) readTable.getPtr(objectNum);                 \
  }                                                                     \
  else if (refFlag == ':') {                                            \
    if (!stream.good()) return stream;                                  \
    objectPtr = new CLASS;                                              \
    readTable.append(objectPtr);                                        \
    rwRestoreGuts(stream,*objectPtr);                                   \
  }                                                                     \
  else {                                                                \
    /* Neither '@' nor ':' was found.  Bad input stream. */             \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  return stream;                                                        \
}

#define RWDEFINE_PERSIST_TEMPLATE_IO(TEMPLATE,ISTR,OSTR)                \
template <class T>                                                      \
RWTEXPORT_RETURN(OSTR&) operator<<(OSTR& stream, const TEMPLATE<T>& item)\
{                                                                       \
  int objectNum;                                                        \
                                                                        \
  RWUseStoreTable storeTable;                                           \
                                                                        \
  if(storeTable.add(&item, objectNum)){                                 \
    stream << ':';                                                      \
    rwSaveGuts(stream, item);                                           \
  }                                                                     \
  else                                                                  \
    stream << '@' << objectNum;                                         \
                                                                        \
  return stream;                                                        \
}                                                                       \
                                                                        \
template <class T>                                                      \
ISTR& operator>>(ISTR& stream, TEMPLATE<T>& object)                     \
{                                                                       \
  if (!stream.good()) return stream;                                    \
                                                                        \
  int            objectNum;                                             \
  char           refFlag;                                               \
  RWUseReadTable readTable;                                             \
                                                                        \
  stream >> refFlag;                                                    \
  if (!stream.good()) return stream;                                    \
                                                                        \
  if(refFlag == '@')                                                    \
  {                                                                     \
    stream >> objectNum;                                                \
    if (!stream.good()) return stream;                                  \
    if (   (size_t) objectNum >= readTable.entries()                    \
        || (&object != (const void*)readTable(objectNum))               \
       )                                                                \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  else if (refFlag == ':') {                                            \
    if (!stream.good()) return stream;                                  \
    readTable.append(&object);                                          \
    rwRestoreGuts(stream, object);                                      \
  }                                                                     \
  else {                                                                \
    /* Neither '@' nor ':' was found.  Bad input stream. */             \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  return stream;                                                        \
}                                                                       \
                                                                        \
template <class T>                                                      \
ISTR& operator>>(ISTR& stream, TEMPLATE<T>*& objectPtr)                 \
{                                                                       \
  if (!stream.good()) return stream;                                    \
                                                                        \
  int            objectNum;                                             \
  char           refFlag;                                               \
  RWUseReadTable readTable;                                             \
                                                                        \
  stream >> refFlag;                                                    \
  if (!stream.good()) return stream;                                    \
                                                                        \
  if(refFlag == '@')                                                    \
  {                                                                     \
    stream >> objectNum;                                                \
    if (!stream.good()) return stream;                                  \
    if ((size_t) objectNum >= readTable.entries())                      \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
    else                                                                \
      objectPtr = (TEMPLATE<T>*) readTable.getPtr(objectNum);           \
  }                                                                     \
  else if (refFlag == ':') {                                            \
    if (!stream.good()) return stream;                                  \
    objectPtr = new TEMPLATE<T>;                                        \
    readTable.append(objectPtr);                                        \
    rwRestoreGuts(stream, *objectPtr);                                  \
  }                                                                     \
  else {                                                                \
    /* Neither '@' nor ':' was found.  Bad input stream. */             \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  return stream;                                                        \
}

#define RWDEFINE_PERSIST_TEMPLATE_IO_2(TEMPLATE,ISTR,OSTR)              \
template <class T1, class T2>                                           \
RWTEXPORT_RETURN(OSTR&) operator<<(OSTR& stream, const TEMPLATE<T1,T2>& item) \
{                                                                       \
  int objectNum;                                                        \
                                                                        \
  RWUseStoreTable storeTable;                                           \
                                                                        \
  if(storeTable.add(&item, objectNum)){                                 \
    stream << ':';                                                      \
    rwSaveGuts(stream, item);                                           \
  }                                                                     \
  else                                                                  \
    stream << '@' << objectNum;                                         \
                                                                        \
  return stream;                                                        \
}                                                                       \
                                                                        \
template <class T1, class T2>                                           \
ISTR& operator>>(ISTR& stream, TEMPLATE<T1,T2>& object)                 \
{                                                                       \
  if (!stream.good()) return stream;                                    \
                                                                        \
  int            objectNum;                                             \
  char           refFlag;                                               \
  RWUseReadTable readTable;                                             \
                                                                        \
  stream >> refFlag;                                                    \
  if (!stream.good()) return stream;                                    \
                                                                        \
  if(refFlag == '@')                                                    \
  {                                                                     \
    stream >> objectNum;                                                \
    if (!stream.good()) return stream;                                  \
    if (   (size_t) objectNum >= readTable.entries()                    \
        || (&object != (const void*)readTable(objectNum))               \
       )                                                                \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  else if (refFlag == ':') {                                            \
    if (!stream.good()) return stream;                                  \
    readTable.append(&object);                                          \
    rwRestoreGuts(stream, object);                                      \
  }                                                                     \
  else {                                                                \
    /* Neither '@' nor ':' was found.  Bad input stream. */             \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  return stream;                                                        \
}                                                                       \
                                                                        \
template <class T1, class T2>                                           \
ISTR& operator>>(ISTR& stream, TEMPLATE<T1,T2>*& objectPtr)             \
{                                                                       \
  if (!stream.good()) return stream;                                    \
                                                                        \
  int            objectNum;                                             \
  char           refFlag;                                               \
  RWUseReadTable readTable;                                             \
                                                                        \
  stream >> refFlag;                                                    \
  if (!stream.good()) return stream;                                    \
                                                                        \
  if(refFlag == '@')                                                    \
  {                                                                     \
    stream >> objectNum;                                                \
    if (!stream.good()) return stream;                                  \
    if ((size_t) objectNum >= readTable.entries())                      \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
    else                                                                \
      objectPtr = (TEMPLATE<T1,T2>*) readTable.getPtr(objectNum);       \
  }                                                                     \
  else if (refFlag == ':') {                                            \
    if (!stream.good()) return stream;                                  \
    objectPtr = new TEMPLATE<T1,T2>;                                    \
    readTable.append(objectPtr);                                        \
    rwRestoreGuts(stream, *objectPtr);                                  \
  }                                                                     \
  else {                                                                \
    /* Neither '@' nor ':' was found.  Bad input stream. */             \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  return stream;                                                        \
}

#define RWDEFINE_PERSIST_TEMPLATE_IO_3(TEMPLATE,ISTR,OSTR)              \
template <class T1, class T2, class T3>                                 \
RWTEXPORT_RETURN(OSTR&)                                                  \
     operator<<(OSTR& stream, const TEMPLATE<T1,T2,T3>& item)           \
{                                                                       \
  int objectNum;                                                        \
                                                                        \
  RWUseStoreTable storeTable;                                           \
                                                                        \
  if(storeTable.add(&item, objectNum)){                                 \
    stream << ':';                                                      \
    rwSaveGuts(stream, item);                                           \
  }                                                                     \
  else                                                                  \
    stream << '@' << objectNum;                                         \
                                                                        \
  return stream;                                                        \
}                                                                       \
                                                                        \
template <class T1, class T2, class T3>                                 \
ISTR& operator>>(ISTR& stream, TEMPLATE<T1,T2,T3>& object)              \
{                                                                       \
  if (!stream.good()) return stream;                                    \
                                                                        \
  int            objectNum;                                             \
  char           refFlag;                                               \
  RWUseReadTable readTable;                                             \
                                                                        \
  stream >> refFlag;                                                    \
  if (!stream.good()) return stream;                                    \
                                                                        \
  if(refFlag == '@')                                                    \
  {                                                                     \
    stream >> objectNum;                                                \
    if (!stream.good()) return stream;                                  \
    if (   (size_t) objectNum >= readTable.entries()                    \
        || (&object != (const void*)readTable(objectNum))               \
       )                                                                \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  else if (refFlag == ':') {                                            \
    if (!stream.good()) return stream;                                  \
    readTable.append(&object);                                          \
    rwRestoreGuts(stream, object);                                      \
  }                                                                     \
  else {                                                                \
    /* Neither '@' nor ':' was found.  Bad input stream. */             \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  return stream;                                                        \
}                                                                       \
                                                                        \
template <class T1, class T2, class T3>                                 \
ISTR& operator>>(ISTR& stream, TEMPLATE<T1,T2,T3>*& objectPtr)          \
{                                                                       \
  if (!stream.good()) return stream;                                    \
                                                                        \
  int            objectNum;                                             \
  char           refFlag;                                               \
  RWUseReadTable readTable;                                             \
                                                                        \
  stream >> refFlag;                                                    \
  if (!stream.good()) return stream;                                    \
                                                                        \
  if(refFlag == '@')                                                    \
  {                                                                     \
    stream >> objectNum;                                                \
    if (!stream.good()) return stream;                                  \
    if ((size_t) objectNum >= readTable.entries())                      \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
    else                                                                \
      objectPtr = (TEMPLATE<T1,T2,T3>*) readTable.getPtr(objectNum);    \
  }                                                                     \
  else if (refFlag == ':') {                                            \
    if (!stream.good()) return stream;                                  \
    objectPtr = new TEMPLATE<T1,T2,T3>;                                 \
    readTable.append(objectPtr);                                        \
    rwRestoreGuts(stream, *objectPtr);                                  \
  }                                                                     \
  else {                                                                \
    /* Neither '@' nor ':' was found.  Bad input stream. */             \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  return stream;                                                        \
}

#define RWDEFINE_PERSIST_TEMPLATE_IO_4(TEMPLATE,ISTR,OSTR)              \
template <class T1, class T2, class T3, class T4>                       \
RWTEXPORT_RETURN(OSTR&)                                                  \
     operator<<(OSTR& stream, const TEMPLATE<T1,T2,T3,T4>& item)        \
{                                                                       \
  int objectNum;                                                        \
                                                                        \
  RWUseStoreTable storeTable;                                           \
                                                                        \
  if(storeTable.add(&item, objectNum)){                                 \
    stream << ':';                                                      \
    rwSaveGuts(stream, item);                                           \
  }                                                                     \
  else                                                                  \
    stream << '@' << objectNum;                                         \
                                                                        \
  return stream;                                                        \
}                                                                       \
                                                                        \
template <class T1, class T2, class T3, class T4>                       \
ISTR& operator>>(ISTR& stream, TEMPLATE<T1,T2,T3,T4>& object)           \
{                                                                       \
  if (!stream.good()) return stream;                                    \
                                                                        \
  int            objectNum;                                             \
  char           refFlag;                                               \
  RWUseReadTable readTable;                                             \
                                                                        \
  stream >> refFlag;                                                    \
  if (!stream.good()) return stream;                                    \
                                                                        \
  if(refFlag == '@')                                                    \
  {                                                                     \
    stream >> objectNum;                                                \
    if (!stream.good()) return stream;                                  \
    if (   (size_t) objectNum >= readTable.entries()                    \
        || (&object != (const void*)readTable(objectNum))               \
       )                                                                \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  else if (refFlag == ':') {                                            \
    if (!stream.good()) return stream;                                  \
    readTable.append(&object);                                          \
    rwRestoreGuts(stream, object);                                      \
  }                                                                     \
  else {                                                                \
    /* Neither '@' nor ':' was found.  Bad input stream. */             \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  return stream;                                                        \
}                                                                       \
                                                                        \
template <class T1, class T2, class T3, class T4>                       \
ISTR& operator>>(ISTR& stream, TEMPLATE<T1,T2,T3,T4>*& objectPtr)       \
{                                                                       \
  if (!stream.good()) return stream;                                    \
                                                                        \
  int            objectNum;                                             \
  char           refFlag;                                               \
  RWUseReadTable readTable;                                             \
                                                                        \
  stream >> refFlag;                                                    \
  if (!stream.good()) return stream;                                    \
                                                                        \
  if(refFlag == '@')                                                    \
  {                                                                     \
    stream >> objectNum;                                                \
    if (!stream.good()) return stream;                                  \
    if ((size_t) objectNum >= readTable.entries())                      \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
    else                                                                \
      objectPtr = (TEMPLATE<T1,T2,T3,T4>*) readTable.getPtr(objectNum); \
  }                                                                     \
  else if (refFlag == ':') {                                            \
    if (!stream.good()) return stream;                                  \
    objectPtr = new TEMPLATE<T1,T2,T3,T4>;                              \
    readTable.append(objectPtr);                                        \
    rwRestoreGuts(stream, *objectPtr);                                  \
  }                                                                     \
  else {                                                                \
    /* Neither '@' nor ':' was found.  Bad input stream. */             \
      RWTHROW(RWInternalErr(RWMessage(RWTOOL_REF)));                    \
  }                                                                     \
  return stream;                                                        \
}


#define RWDEFINE_PERSISTABLE(CLASS)                                     \
RWDEFINE_PERSIST_IO(CLASS,RWvistream,RWvostream)                        \
RWDEFINE_PERSIST_IO(CLASS,RWFile,RWFile)

#define RWDEFINE_PERSISTABLE_TEMPLATE(TEMPLATE)                         \
RWDEFINE_PERSIST_TEMPLATE_IO(TEMPLATE,RWvistream,RWvostream)            \
RWDEFINE_PERSIST_TEMPLATE_IO(TEMPLATE,RWFile,RWFile)

#define RWDEFINE_PERSISTABLE_TEMPLATE_2(TEMPLATE)                       \
RWDEFINE_PERSIST_TEMPLATE_IO_2(TEMPLATE,RWvistream,RWvostream)          \
RWDEFINE_PERSIST_TEMPLATE_IO_2(TEMPLATE,RWFile,RWFile)

#define RWDEFINE_PERSISTABLE_TEMPLATE_3(TEMPLATE)                       \
RWDEFINE_PERSIST_TEMPLATE_IO_3(TEMPLATE,RWvistream,RWvostream)          \
RWDEFINE_PERSIST_TEMPLATE_IO_3(TEMPLATE,RWFile,RWFile)

#define RWDEFINE_PERSISTABLE_TEMPLATE_4(TEMPLATE)                       \
RWDEFINE_PERSIST_TEMPLATE_IO_4(TEMPLATE,RWvistream,RWvostream)          \
RWDEFINE_PERSIST_TEMPLATE_IO_4(TEMPLATE,RWFile,RWFile)

#ifdef RW_NO_STL
// provide functions so we can use apply() to stream data out
#  ifndef RW_NO_REF_TO_REF
template<class T>
void rwInsertInStream(T& item, void*x);

template<class T>
void rwInsertInFile(T& item, void*x);

#  else

template<class T>
void rwInsertInStream(T item, void*x);

template<class T>
void rwInsertInFile(T item, void*x);

#  endif /* RW_NO_REF_TO_REF */

#endif /* RW_NO_STL */

#ifdef RW_COMPILE_INSTANTIATE
#include "rw/epersist.cc"
#endif

#endif /* __RWEPERSIST_H__ */
